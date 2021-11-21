{-# language AllowAmbiguousTypes, DeriveAnyClass, DuplicateRecordFields, UndecidableInstances #-}
module Pure.Magician.Server (module Pure.Magician.Server, Server(..), module Export) where

import Pure.Magician.Resources as Export
import Pure.Magician.Server.Cache
import qualified Pure.Magician.Server.Config as Config
import Pure.Magician.Server.Listen
import Pure.Magician.Server.Serve
import Pure.Magician.Server.Static

import Pure.Auth (Config(..),Token(..),Username(..),Password,Email,auth,authDB,tryCreateUser)
import Pure.Conjurer hiding (Cache,cache)
import qualified Pure.Conjurer as Conjurer
import Pure.Convoker as Convoker
import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Data.Txt as Txt
import Pure.Elm.Component (Elm,Default(..),View,pattern Null,Component(..),inject,body,delay,pattern Minute,command,pattern SimpleHTML,(<||>))
import Pure.Sorcerer (Listener,sorcerer)
import qualified Pure.Server as Server
import Pure.WebSocket ( WebSocket, enact, repeal, clientWS, activate )
import qualified Pure.WebSocket as WS


import Control.Monad ( liftM2, forever, void )
import Data.Char
import Data.List as List
import Data.Typeable ( Typeable, Proxy(..) )
import GHC.Generics hiding (Meta)
import System.IO

type UserConfig a = Elm (Msg (WithSocket a)) => WebSocket -> Pure.Auth.Config a 

serve
  :: forall a resources cache static. 
    ( Typeable a
    , Server a
    , Subset (Caches a) (Resources a) ~ True
    , Subset (Statics a) (Resources a) ~ True
    , Component (Connection a)
    , ListenMany a (Resources a)
    , ServeMany a (Resources a)
    , CacheMany a (Caches a)
    , StaticMany a (Statics a)
    ) => UserConfig a -> IO ()
serve userConfig = do
  cfg@Config.Config {..} <- Config.getConfig
  hSetBuffering stdout NoBuffering 
  inject body (sorcerer (authDB @a ++ conjure @Admins ++ listenAll @a))
  tryCreateUser @a admin email password 
  tryReadProduct fullPermissions def AdminsContext AdminsName >>= \case
    Just (Admins _) -> pure ()
    Nothing -> void (tryCreateAdmins [admin])
  Conjurer.cache @Admins
  cacheAll @a
  inject body do
    case (,) <$> key <*> cert of
      Just (k,c) -> Server.SecureServer host port k c chain (run . WithSocket userConfig)
      _ -> Server.Server host port (run . WithSocket userConfig)
  ws <- clientWS host port
  staticAll @a ws
  forever (delay Minute)

data WithSocket a = WithSocket (Elm (Msg (WithSocket a)) => WebSocket -> Pure.Auth.Config a) WebSocket
instance (Typeable a, Server a, Component (Connection a), ServeMany a (Resources a)) => Component (WithSocket a) where
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
        serveAll @a socket Nothing
        enact socket (auth (cfg socket))
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

defaultUserConfig :: forall a. (Elm (Msg (WithSocket a)), Server a, ServeMany a (Resources a), RemoveMany a (Resources a)) => WebSocket -> Pure.Auth.Config a 
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
      case mt of
        Nothing -> void do
          removeAll @a socket
          serveAll @a socket Nothing

        Just (Token (un,_)) -> void do
          removeAll @a socket
          serveAll @a socket (Just un)

    onDeleted username email = pure ()

    onRegister username email key activate = activate

    onRecover username email key = pure ()

    onDelete username email key = pure ()

data Connection (a :: *) = Connection
  { socket :: WebSocket
  , user   :: Maybe Username
  }

-- Extension point for servers; override with custom instance, but be sure to call `activate socket`!
instance {-# INCOHERENT #-} (Typeable a) => Component (Connection a) where
  initialize Connection {..} = do
    activate socket
    pure undefined
