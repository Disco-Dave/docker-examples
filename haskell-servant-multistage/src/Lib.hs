{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import           Servant

type API = Get '[PlainText] String

server :: Server API
server = return "hello from docker!"

app :: Application
app = serve (Proxy @API) server
