module Lib
  ( app
  )
where

import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Data.Function                  ( (&) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable )
import           Network.Socket                 ( SockAddr )
import           Servant                        ( (:>)
                                                , Get
                                                , RemoteHost
                                                , PlainText
                                                )
import           Servant.Server                 ( ServerT
                                                , hoistServer
                                                , Application
                                                , serve
                                                )
import           Text.Read                      ( readMaybe )
import qualified Control.Monad.IO.Class        as MTL
import qualified Control.Monad.Reader          as MTL
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as Text
import qualified Database.Redis                as Redis

newtype AppMonad a = AppMonad
  { unAppMonad :: MTL.ReaderT Redis.Connection IO a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MTL.MonadReader Redis.Connection
           , MTL.MonadIO)

type API = RemoteHost :> Get '[PlainText] Text.Text

server :: ServerT API AppMonad
server = getVisitCount

app :: Redis.Connection -> Application
app conn = serve api $ hoistServer api nt server
 where
  api = Proxy @API
  nt ma = MTL.liftIO $ MTL.runReaderT (unAppMonad ma) conn

getVisitCount
  :: MTL.MonadIO m
  => MTL.MonadReader Redis.Connection m
  => SockAddr
  -> m Text.Text
getVisitCount sockAddr = do
  redisConn  <- MTL.ask
  visitCount <-
    getAndIncrementCount sockAddr 
      & Redis.runRedis redisConn 
      & MTL.liftIO
  return $ Text.concat ["Your visit count is ", Text.pack $ show visitCount]

getAndIncrementCount :: SockAddr -> Redis.Redis Int
getAndIncrementCount sockAddr = do
  let key = BS.pack $ show sockAddr
  query <- Redis.get key
  case query of
    Left  _       -> MTL.liftIO $ throwIO RedisException
    Right Nothing -> do
      Redis.set key "1"
      return 1
    Right (Just count) -> 
      case readMaybe @Int $ BS.unpack count of
        Nothing       -> MTL.liftIO $ throwIO InvalidCountFormat
        Just oldCount -> do
          let newCount = oldCount + 1
          Redis.set key (BS.pack $ show newCount)
          return newCount

data RedisException = RedisException
  deriving (Show, Typeable)
instance Exception RedisException

data InvalidCountFormat = InvalidCountFormat
  deriving (Show, Typeable)
instance Exception InvalidCountFormat
