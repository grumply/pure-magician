module Pure.Magician.Server.Config where
  
import Pure.Auth (Username,Password,Email)
import Pure.Data.JSON (ToJSON,FromJSON)

import Data.Yaml (decodeFileThrow)

import GHC.Generics

data Config = Config
  { host     :: String
  , port     :: Int
  , admin    :: Username
  , password :: Password
  , email    :: Email
  , refresh  :: Int -- milliseconds
  , key      :: Maybe String
  , cert     :: Maybe String
  , chain    :: Maybe String
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

getConfig :: IO Config
getConfig = getConfigFrom "config.yaml"

getConfigFrom :: FilePath -> IO Config
getConfigFrom = decodeFileThrow