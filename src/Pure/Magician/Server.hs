{-# language AllowAmbiguousTypes, DeriveAnyClass, DuplicateRecordFields, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-methods #-}
module Pure.Magician.Server where

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
import Pure.WebSocket ( WebSocket, enact, repeal, clientWS, activate )
import qualified Pure.WebSocket as WS

import Data.Yaml (decodeFileThrow)

import Control.Monad ( liftM2, forever, void )
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
  , key      :: Maybe FilePath
  , cert     :: Maybe FilePath
  , chain    :: Maybe FilePath
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

getConfig :: IO Pure.Magician.Server.Config
getConfig = decodeFileThrow "config.yaml"

server 
  :: forall a resources cache static. 
    ( Typeable a
    , Component (Connection a)
    , Resources a ~ resources, ListenMany resources, ServeMany resources
    , Caches a ~ cache, CacheMany cache
    , Statics a ~ static, StaticMany static
    ) => (Elm (Msg (WithSocket a)) => WebSocket -> Pure.Auth.Config a) -> IO ()
server authConfig = do
  cfg@Pure.Magician.Server.Config {..} <- getConfig
  hSetBuffering stdout NoBuffering 
  inject body (sorcerer (authDB @a ++ conjure @Admins ++ listenMany @resources))
  tryCreateUser @a admin email password 
  tryReadProduct fullPermissions def AdminsContext AdminsName >>= \case
    Just (Admins _) -> pure ()
    Nothing -> void (tryCreateAdmins [admin])
  cache @Admins
  cacheMany @(Caches a)
  inject body do
    case (,) <$> key <*> cert of
      Just (k,c) -> Server.SecureServer host port k c chain (run . WithSocket authConfig)
      _ -> Server.Server host port (run . WithSocket authConfig)
  ws <- clientWS host port
  staticMany @static ws
  forever (delay Minute)

data WithSocket a = WithSocket (Elm (Msg (WithSocket a)) => WebSocket -> Pure.Auth.Config a) WebSocket
instance (Typeable a, Component (Connection a), Resources a ~ resources, ServeMany resources) => Component (WithSocket a) where
  data Model (WithSocket a) = WithSocketModel (Maybe (Token a))

  model = WithSocketModel Nothing

  data Msg (WithSocket a)
    = Startup 
    | GetUserToken (Maybe (Token a) -> IO ())
    | SetUserToken (Token a)
    | ClearUserToken

  startup = [Startup]
    
  upon msg (WithSocket cfg socket) (WithSocketModel userToken)  = 
    case msg of

      Startup -> do
        enact socket (auth (cfg socket))
        serveMany @(Resources a) socket Nothing
        pure (WithSocketModel userToken)

      GetUserToken with -> do
        with userToken 
        pure (WithSocketModel userToken)

      ClearUserToken -> do
        pure (WithSocketModel Nothing)

      SetUserToken t -> do
        pure (WithSocketModel (Just t))

  view (WithSocket _ socket) (WithSocketModel token) | user <- fmap (\(Token (un,_)) -> un) token =
    run @(Connection a) Connection {..}

defaultUserConfig :: forall a. (Elm (Msg (WithSocket a)), ServeMany (Resources a), RemoveMany (Resources a)) => WebSocket -> Pure.Auth.Config a 
defaultUserConfig socket = Pure.Auth.Config {..}
  where
    blacklist = []
    implicitlyWhitelisted = Prelude.not . (`elem` blacklist)

    validateUsername un = 
      List.and 
        [ Txt.length un <= 20 
        , Txt.all ((&&) <$> isAscii <*> isAlphaNum) un
        , implicitlyWhitelisted un
        ] 

    onTokenChange mt = do
      command (maybe ClearUserToken SetUserToken mt)
      defaultServeCaching @Admins socket Nothing 
      case mt of
        Nothing -> void do
          removeMany @(Resources a) socket
          serveMany @(Resources a) socket Nothing

        Just (Token (un,_)) -> void do
          removeMany @(Resources a) socket
          serveMany @(Resources a) socket (Just un)

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

cacheWithDiscussion 
  :: forall a. 
    ( Conjurable a
    , ToJSON (Product (Meta a)), FromJSON (Product (Meta a))
    , ToJSON (Preview (Meta a)), FromJSON (Preview (Meta a))
    ) => IO ()
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
  serveMany :: WebSocket -> Maybe Username -> IO ()

instance (Servable x, ServeMany xs) => ServeMany (x : xs) where 
  serveMany ws mun = serve @x ws mun >> serveMany @xs ws mun

instance ServeMany '[] where
  serveMany _ _ = pure ()

class RemoveMany (a :: [*]) where
  removeMany :: WebSocket -> IO ()

instance (Removable x, RemoveMany xs) => RemoveMany (x : xs) where
  removeMany ws = remove @x ws >> removeMany @xs ws

instance RemoveMany '[] where
  removeMany _ = pure ()

class Servable (a :: *) where
  serve :: WebSocket -> Maybe Username -> IO ()
  
instance {-# INCOHERENT #-} (Typeable a, Conjurable a, DefaultPermissions a, DefaultCallbacks a) => Servable a where
  serve = defaultServeCaching @a

class Removable (a :: *) where
  remove :: WebSocket -> IO ()

instance {-# INCOHERENT #-} (Typeable a, Conjurable a ) => Removable a where
  remove = defaultRemove @a

defaultServe 
  :: forall x. 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , Conjurable x
    ) => WebSocket -> Maybe Username -> IO ()
defaultServe ws = \case
  Just un -> void do
    enact ws (reading @x readPermissions (callbacks (Just un)))
    enact ws (publishing @x (permissions (Just un)) (callbacks (Just un)) def)

  _ -> void do
    enact ws (reading @x readPermissions (callbacks Nothing))

defaultRemove
  :: forall (x :: *).
    ( Typeable x, Conjurable x ) => WebSocket -> IO ()
defaultRemove ws = do
  WS.remove ws (readingAPI @x) 
  WS.remove ws (publishingAPI @x)

defaultServeWithDiscussion
  :: forall x. 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , DefaultPermissions (Comment x), DefaultPermissions (Meta x)
    , DefaultCallbacks (Discussion x), DefaultCallbacks (Comment x), DefaultCallbacks (Meta x), DefaultCallbacks (Mods x), DefaultCallbacks (UserVotes x)
    , DefaultInteractions (Comment x), DefaultInteractions (Meta x)
    , Conjurable x
    , Conjurable (Meta x)
    , Conjurable (Comment x)
    ) => WebSocket -> Maybe Username -> IO ()
defaultServeWithDiscussion ws = \case
  Just un -> void do
    enact ws (reading @x readPermissions (callbacks (Just un)))
    enact ws (publishing @x (permissions (Just un)) (callbacks (Just un)) def)

    enact ws (cachingReading @(Discussion x) readPermissions (callbacks (Just un)))
    enact ws (reading @(Comment x) (permissions (Just un)) (callbacks (Just un)))
    enact ws (cachingReading @(Meta x) (permissions (Just un)) (callbacks (Just un)))
    enact ws (cachingReading @(Mods x) readPermissions (callbacks (Just un)))
    enact ws (reading @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)))

    enact ws (publishing @(Comment x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (cachingPublishing @(Meta x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (cachingPublishing @(Mods x) (modsPermissions un) (callbacks (Just un)) modsInteractions)
    enact ws (publishing @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)) userVotesInteractions)

  _ -> void do
    enact ws (reading @x readPermissions (callbacks Nothing))
    enact ws (cachingReading @(Discussion x) readPermissions (callbacks Nothing))
    enact ws (cachingReading @(Meta x) readPermissions (callbacks Nothing))
    enact ws (cachingReading @(Mods x) readPermissions (callbacks Nothing))

defaultRemoveWithDiscussion
  :: forall (x :: *). 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , DefaultPermissions (Comment x), DefaultPermissions (Meta x)
    , DefaultCallbacks (Discussion x), DefaultCallbacks (Comment x), DefaultCallbacks (Meta x), DefaultCallbacks (Mods x), DefaultCallbacks (UserVotes x)
    , DefaultInteractions (Comment x), DefaultInteractions (Meta x)
    , Conjurable x
    , Conjurable (Meta x)
    , Conjurable (Comment x)
    ) => WebSocket -> IO ()
defaultRemoveWithDiscussion ws = do
  WS.remove ws (readingAPI @x)
  WS.remove ws (publishingAPI @x)
  WS.remove ws (readingAPI @(Discussion x))
  WS.remove ws (readingAPI @(Comment x))
  WS.remove ws (readingAPI @(Meta x))
  WS.remove ws (readingAPI @(Mods x))
  WS.remove ws (readingAPI @(UserVotes x))
  WS.remove ws (publishingAPI @(Comment x))
  WS.remove ws (publishingAPI @(Meta x))
  WS.remove ws (publishingAPI @(Mods x))
  WS.remove ws (publishingAPI @(UserVotes x))

defaultServeCaching 
  :: forall x. 
    ( DefaultPermissions x, DefaultCallbacks x
    , Conjurable x
    ) => WebSocket -> Maybe Username -> IO ()
defaultServeCaching ws = \case
  Just un -> void do
    enact ws (cachingReading @x readPermissions (callbacks (Just un)))
    enact ws (cachingPublishing @x (permissions (Just un)) (callbacks (Just un)) def)

  _ -> void do
    enact ws (cachingReading @x readPermissions (callbacks Nothing))

defaultServeCachingWithDiscussion
  :: forall x. 
    ( Typeable x, DefaultPermissions x, DefaultCallbacks x
    , DefaultPermissions (Comment x), DefaultPermissions (Meta x)
    , DefaultCallbacks (Discussion x), DefaultCallbacks (Comment x), DefaultCallbacks (Meta x), DefaultCallbacks (Mods x), DefaultCallbacks (UserVotes x)
    , DefaultInteractions (Comment x), DefaultInteractions (Meta x)
    , Conjurable x
    , Conjurable (Meta x)
    , Conjurable (Comment x)
    ) => WebSocket -> Maybe Username -> IO ()
defaultServeCachingWithDiscussion ws = \case
  Just un -> void do
    enact ws (cachingReading @x readPermissions (callbacks (Just un)))
    enact ws (cachingPublishing @x (permissions (Just un)) (callbacks (Just un)) def)

    enact ws (cachingReading @(Discussion x) readPermissions (callbacks (Just un)))
    enact ws (reading @(Comment x) (permissions (Just un)) (callbacks (Just un)))
    enact ws (cachingReading @(Meta x) (permissions (Just un)) (callbacks (Just un)))
    enact ws (cachingReading @(Mods x) readPermissions (callbacks (Just un)))
    enact ws (reading @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)))

    enact ws (publishing @(Comment x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (cachingPublishing @(Meta x) (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (cachingPublishing @(Mods x) (modsPermissions un) (callbacks (Just un)) modsInteractions)
    enact ws (publishing @(UserVotes x) (userVotesPermissions un) (callbacks (Just un)) userVotesInteractions)
           
  _ -> void do
    enact ws (cachingReading @x readPermissions (callbacks Nothing))
    enact ws (cachingReading @(Discussion x) readPermissions (callbacks Nothing))
    enact ws (cachingReading @(Meta x) readPermissions (callbacks Nothing))
    enact ws (cachingReading @(Mods x) readPermissions (callbacks Nothing))

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

instance {-# INCOHERENT #-} Ownable x => DefaultPermissions x

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