module Main where

import           Lib
import           Network.Wai.Handler.Warp       ( run )
import           System.Environment             ( getEnv )
import qualified Database.Redis                as Redis

main :: IO ()
main = do
  conn <- Redis.checkedConnect Redis.defaultConnectInfo
  run 8081 (app conn)
