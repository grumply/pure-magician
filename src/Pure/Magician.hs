{-# language AllowAmbiguousTypes, DeriveAnyClass, DuplicateRecordFields, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-name-shadowing #-}
module Pure.Magician where

import Pure.Auth (Config(..),Token(..),Username(..),Password,Email,auth,authDB,tryCreateUser)
import Pure.Conjurer hiding (Cache,cache)
import qualified Pure.Conjurer as Conjurer
import Pure.Convoker as Convoker
import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Data.Txt as Txt
import Pure.Elm.Component (Elm,Default(..),View,pattern Null,Component(..),inject,body,delay,pattern Minute,command,pattern SimpleHTML,(<||>))
import Pure.Hooks (useEffectWith')
import Pure.Sorcerer (Listener,sorcerer)
import qualified Pure.Server as Server
import Pure.WebSocket ( WebSocket, enact, repeal, clientWS, activate)

import Data.Yaml (decodeFileThrow)

import Control.Monad ( liftM2, forever )
import Data.Char
import Data.List as List
import Data.Typeable ( Typeable, Proxy(..) )
import GHC.Generics hiding (Meta)
import System.IO

class Serve a where
  type Resources a :: [*]
  type Caches a :: [*]
  type Caches a = Resources a
  type Statics a :: [*]
  type Statics a = Resources a

data Config = Config
  { host     :: String
  , port     :: Int
  , admin    :: Username
  , password :: Password
  , email    :: Email
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

getConfig :: IO Pure.Magician.Config
getConfig = decodeFileThrow "config.yaml"

server 
  :: forall a resources cache static. 
    ( Typeable a
    , Component (Connection a)
    , Resources a ~ resources, ListenMany resources, ServeMany resources
    , Caches a ~ cache, CacheMany cache
    , Statics a ~ static, StaticMany static
    ) => (Elm (Msg (WithSocket a)) => Pure.Auth.Config a) -> IO ()
server authConfig = do
  cfg@Pure.Magician.Config {..} <- getConfig
  hSetBuffering stdout NoBuffering 
  inject body (sorcerer (authDB @a ++ conjure @Admins ++ listenMany @resources))
  tryCreateUser @a admin email password 
  inject body (run (Server authConfig cfg :: Server a))
  ws <- clientWS host port
  staticMany @static ws
  forever (delay Minute)

data Server a = Server (Elm (Msg (WithSocket a)) => Pure.Auth.Config a) Pure.Magician.Config
instance (Typeable a, Component (Connection a), Caches a ~ caches, CacheMany caches, Resources a ~ resources, ServeMany resources) => Component (Server a) where
  data Model (Server a) = Model
    
  initialize (Server _ Pure.Magician.Config {..}) = do
    cacheMany @(Caches a)
    tryCreateAdmins [admin]
    pure Model
  
  view (Server acfg Pure.Magician.Config {..}) Model =
    Server.Server host port (run . WithSocket acfg)

data WithSocket a = WithSocket (Elm (Msg (WithSocket a)) => Pure.Auth.Config a) WebSocket
instance (Typeable a, Component (Connection a), Resources a ~ resources, ServeMany resources) => Component (WithSocket a) where
  data Model (WithSocket a) = WithSocketModel (Maybe (Token a))

  data Msg (WithSocket a)
    = GetUserToken (Maybe (Token a) -> IO ())
    | SetUserToken (Token a)
    | ClearUserToken
    
  upon msg _ (WithSocketModel userToken)  = 
    case msg of

      GetUserToken with -> do
        with userToken 
        pure (WithSocketModel userToken)

      ClearUserToken -> do
        pure (WithSocketModel Nothing)

      SetUserToken t -> do
        pure (WithSocketModel (Just t))
    
  model = WithSocketModel Nothing

  view (WithSocket acfg socket) (WithSocketModel token) | user <- fmap (\(Token (un,_)) -> un) token =
    SimpleHTML "withSocket" <||>
      [ SimpleHTML "endpoints" <||> (serveMany @(Resources a) socket user)
      , let cfg = acfg 
        in let effect = enact socket (auth cfg) >>= \api -> pure (repeal api) 
           in useEffectWith' effect () Null
      , run @(Connection a) Connection {..}
      ]

defaultUserConfig :: forall a. Elm (Msg (WithSocket a)) => Pure.Auth.Config a 
defaultUserConfig = Pure.Auth.Config {..}
  where
    blacklist = []
    implicitlyWhitelisted = Prelude.not . (`elem` blacklist)

    validateUsername un = 
      List.and 
        [ Txt.length un <= 20 
        , Txt.all ((&&) <$> isAscii <*> isAlphaNum) un
        , implicitlyWhitelisted un
        ] 

    onTokenChange = command . maybe ClearUserToken SetUserToken

    onDeleted username email = pure ()

    onRegister username email key activate = activate

    onRecover username email key = pure ()

    onDelete username email key = pure ()

data Connection (a :: *) = Connection
  { socket :: WebSocket
  , user   :: Maybe Username
  }

instance {-# INCOHERENT #-} (Typeable a) => Component (Connection a) where
  initialize Connection {..} = do
    activate socket
    pure undefined

class CacheMany (a :: [*]) where
  cacheMany :: IO ()

instance (Cacheable x, CacheMany xs) => CacheMany (x : xs) where
  cacheMany = cache @x >> cacheMany @xs

instance CacheMany '[] where
  cacheMany = pure ()

class Cacheable a where
  cache :: IO ()

instance {-# INCOHERENT #-} Conjurable a => Cacheable a where
  cache = Conjurer.cache @a

cacheWithDiscussion :: forall a. (Conjurable a, _) => IO ()
cacheWithDiscussion = do
  Conjurer.cache @a
  Convoker.convokerCache @a

class StaticMany (a :: [*]) where
  staticMany :: WebSocket -> IO ()

instance (Staticable x, StaticMany xs) => StaticMany (x : xs) where
  staticMany ws = static @x ws >> staticMany @xs ws

instance StaticMany '[] where
  staticMany _ = pure ()

class Staticable a where
  static :: WebSocket -> IO () 

instance {-# INCOHERENT #-} (Conjurable a, Routable a, Component (Product a)) => Staticable a where
  static ws = generateStatic @a ws

class ListenMany (a :: [*]) where
  listenMany :: [Listener]

instance (Listenable x, ListenMany xs) => ListenMany (x : xs) where
  listenMany = listen @x ++ listenMany @xs

instance ListenMany '[] where
  listenMany = []

class Listenable x where
  listen :: [Listener]
  
instance {-# INCOHERENT #-} Conjurable a => Listenable a where
  listen = conjure @a

class ServeMany (a :: [*]) where
  serveMany :: WebSocket -> Maybe Username -> [View]

instance (Servable x, ServeMany (xs :: [*])) => ServeMany ((x :: *) : xs) where 
  serveMany ws mun = serve @x ws mun ++ serveMany @xs ws mun

instance ServeMany '[] where
  serveMany _ _ = []

class Servable a where
  serve :: WebSocket -> Maybe Username -> [View]
  
instance {-# INCOHERENT #-} (Typeable a, Conjurable a, DefaultPermissions a, DefaultCallbacks a) => Servable a where
  serve = defaultServeCaching @a

defaultServe 
  :: forall x. 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , Conjurable x
    ) => Proxy x -> WebSocket -> Maybe Username -> [View]
defaultServe _ ws mun =
  [ useEffectWith' (effect mun) mun Null ]
  where
    effect = \case
      Just un -> do
        r <- enact ws (reading @x readPermissions (callbacks (Just un)))
        p <- enact ws (publishing @x (permissions (Just un)) (callbacks (Just un)) def)
        pure do
          repeal r
          repeal p

      _ -> do
        r <- enact ws (reading @x readPermissions (callbacks Nothing))
        pure do
          repeal r

defaultServeWithDiscussion
  :: forall x. 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , DefaultPermissions (Comment x), DefaultPermissions (Meta x)
    , DefaultCallbacks (Discussion x), DefaultCallbacks (Comment x), DefaultCallbacks (Meta x), DefaultCallbacks (Mods x), DefaultCallbacks (UserVotes x)
    , DefaultInteractions (Comment x), DefaultInteractions (Meta x)
    , Conjurable x
    , Conjurable (Meta x)
    , Conjurable (Comment x)
    ) => Proxy x -> WebSocket -> Maybe Username -> [View]
defaultServeWithDiscussion _ ws mun =
  [ useEffectWith' (effect mun) mun Null ]
  where
    effect = \case
      Just un -> do
        readResource     <- enact ws (reading @x readPermissions (callbacks (Just un)))
        publishResource  <- enact ws (publishing @x (permissions (Just un)) (callbacks (Just un)) def)

        readDiscussion   <- enact ws (cachingReading @(Discussion x) readPermissions (callbacks (Just un)))
        readComment      <- enact ws (reading @(Comment x) (permissions (Just un)) (callbacks (Just un)))
        readMeta         <- enact ws (cachingReading @(Meta x) (permissions (Just un)) (callbacks (Just un)))
        readMods         <- enact ws (cachingReading @(Mods x) readPermissions (callbacks (Just un)))
        readUserVotes    <- enact ws (reading @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)))

        publishComment   <- enact ws (publishing @(Comment x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
        publishMeta      <- enact ws (cachingPublishing @(Meta x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
        publishMods      <- enact ws (cachingPublishing @(Mods x) (modsPermissions un) (callbacks (Just un)) modsInteractions)
        publishUserVotes <- enact ws (publishing @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)) userVotesInteractions)
        
        pure do
          repeal readResource
          repeal publishResource
          repeal readDiscussion
          repeal readComment
          repeal readMeta
          repeal readMods
          repeal readUserVotes
          repeal publishComment
          repeal publishMeta
          repeal publishMods
          repeal publishUserVotes

      _ -> do
        readResource   <- enact ws (reading @x readPermissions (callbacks Nothing))
        readDiscussion <- enact ws (cachingReading @(Discussion x) readPermissions (callbacks Nothing))
        readMeta       <- enact ws (cachingReading @(Meta x) readPermissions (callbacks Nothing))
        readMods       <- enact ws (cachingReading @(Mods x) readPermissions (callbacks Nothing))

        pure do
          repeal readResource
          repeal readDiscussion
          repeal readMeta
          repeal readMods

defaultServeCaching 
  :: forall x. 
    ( DefaultPermissions x, DefaultCallbacks x
    , Conjurable x
    ) => WebSocket -> Maybe Username -> [View]
defaultServeCaching ws mun =
  [ useEffectWith' (effect mun) mun Null ]
  where
    effect = \case
      Just un -> do
        r <- enact ws (cachingReading @x readPermissions (callbacks (Just un)))
        p <- enact ws (cachingPublishing @x (permissions (Just un)) (callbacks (Just un)) def)
        pure do
          repeal r
          repeal p

      _ -> do
        r <- enact ws (cachingReading @x readPermissions (callbacks Nothing))
        pure do
          repeal r

defaultServeCachingWithDiscussion
  :: forall x. 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , DefaultPermissions (Comment x), DefaultPermissions (Meta x)
    , DefaultCallbacks (Discussion x), DefaultCallbacks (Comment x), DefaultCallbacks (Meta x), DefaultCallbacks (Mods x), DefaultCallbacks (UserVotes x)
    , DefaultInteractions (Comment x), DefaultInteractions (Meta x)
    , Conjurable x
    , Conjurable (Meta x)
    , Conjurable (Comment x)
    ) => Proxy x -> WebSocket -> Maybe Username -> [View]
defaultServeCachingWithDiscussion _ ws mun =
  [ useEffectWith' (effect mun) mun Null ]
  where
    effect = \case
      Just un -> do
        readResource     <- enact ws (cachingReading @x readPermissions (callbacks (Just un)))
        publishResource  <- enact ws (cachingPublishing @x (permissions (Just un)) (callbacks (Just un)) def)

        readDiscussion   <- enact ws (cachingReading @(Discussion x) readPermissions (callbacks (Just un)))
        readComment      <- enact ws (reading @(Comment x) (permissions (Just un)) (callbacks (Just un)))
        readMeta         <- enact ws (cachingReading @(Meta x) (permissions (Just un)) (callbacks (Just un)))
        readMods         <- enact ws (cachingReading @(Mods x) readPermissions (callbacks (Just un)))
        readUserVotes    <- enact ws (reading @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)))

        publishComment   <- enact ws (publishing @(Comment x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
        publishMeta      <- enact ws (cachingPublishing @(Meta x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
        publishMods      <- enact ws (cachingPublishing @(Mods x) (modsPermissions un) (callbacks (Just un)) modsInteractions)
        publishUserVotes <- enact ws (publishing @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)) userVotesInteractions)
               
        pure do
          repeal readResource
          repeal publishResource
          repeal readDiscussion
          repeal readComment
          repeal readMeta
          repeal readMods
          repeal readUserVotes
          repeal publishComment
          repeal publishMeta
          repeal publishMods
          repeal publishUserVotes

      _ -> do
        readResource   <- enact ws (cachingReading @x readPermissions (callbacks Nothing))
        readDiscussion <- enact ws (cachingReading @(Discussion x) readPermissions (callbacks Nothing))
        readMeta       <- enact ws (cachingReading @(Meta x) readPermissions (callbacks Nothing))
        readMods       <- enact ws (cachingReading @(Mods x) readPermissions (callbacks Nothing))

        pure do
          repeal readResource
          repeal readDiscussion
          repeal readMeta
          repeal readMods

class DefaultPermissions x where
  permissions :: Maybe Username -> Permissions x
  default permissions :: Ownable x => Maybe Username -> Permissions x
  permissions Nothing = readPermissions
  permissions (Just un) =
    readPermissions
      { canCreate = canCreate' 
      , canUpdate = canUpdate'
      , canAmend  = canAmend'
      , canInteract = canInteract' 
      }
    where
      writePermission ctx nm = liftM2 (||) (isOwner un ctx nm) (isAdmin un)

      canCreate' ctx nm res = writePermission ctx nm
      canUpdate' ctx nm = writePermission ctx nm
      canAmend' ctx nm _ = writePermission ctx nm
      canInteract' ctx nm _ = writePermission ctx nm

instance {-# INCOHERENT #-} Ownable a where
  isOwner un _ _ = isAdmin un

instance {-# INCOHERENT #-} DefaultPermissions x

class DefaultCallbacks x where
  callbacks :: Maybe Username -> Callbacks x
  callbacks _ = def

instance {-# INCOHERENT #-} DefaultCallbacks x where
  callbacks _ = def 

class Typeable x => DefaultInteractions x where
  interactions :: Maybe Username -> Interactions x
  interactions _ = def

instance {-# INCOHERENT #-} Typeable x => DefaultInteractions x where
  interactions _ = def