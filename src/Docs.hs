{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import           Data.ByteString.Lazy           ( ByteString )
import           Data.Proxy
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Text.Lazy                 ( pack )
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Docs
import           Servant.Server
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , ToForm(..)
                                                )

import           Server                         ( Email(..)
                                                , ClientInfo(..)
                                                , Position(..)
                                                , HelloMessage(..)
                                                , server3
                                                , emailForClient
                                                )
                                                

type ExampleAPI = "position" 
                    :> Capture "x" Int 
                    :> Capture "y" Int 
                    :> Get '[JSON] Position
             :<|> "hello" 
                    :> QueryParam "name" String 
                    :> Get '[JSON] HelloMessage
             :<|> "marketing" 
                :> ReqBody '[JSON] ClientInfo 
                :> Post '[JSON] Email

exampleAPI :: Proxy ExampleAPI 
exampleAPI = Proxy 


instance ToCapture (Capture "x" Int) where
    toCapture _ = 
        DocCapture "x"                               -- name
                  "(integer) position on the x axis" -- description

instance ToCapture (Capture "y" Int) where
    toCapture _ = 
        DocCapture "y"                               -- name
                  "(integer) position on the y axis" -- description

instance ToSample Position where 
    toSamples _ = singleSample (Position 3 14) -- example of output

instance ToParam (QueryParam "name" String) where
    toParam _ = 
        DocQueryParam "name"                                -- name
                      ["Alp", "John Doe", "..."]            -- example of values (not necessarily exhaustive)
                      "Name of the person to say hello to." -- description
                      Normal                                -- Normal, List or Flag

instance ToSample HelloMessage where 
    toSamples _ = 
        [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
        , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
        ] -- multiple examples to display this time

ci :: ClientInfo
ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample ClientInfo where
    toSamples _ = singleSample ci 

instance ToSample Email where 
    toSamples _ = singleSample $ emailForClient ci


apiDocs :: API 
apiDocs = docs exampleAPI

docsBS :: ByteString 
docsBS = encodeUtf8 
       . pack
       . markdown
       $ docsWithIntros [intro] exampleAPI
       where
           intro = DocIntro "Welcome" 
                [ "This is our super webservice's API."
                , "Enjoy!"
                ] 

type DocsAPI = ExampleAPI :<|> Raw 

api :: Proxy DocsAPI 
api = Proxy 

server :: Server DocsAPI 
server = Server.server3 :<|> Tagged serveDocs where 
    serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS 
    plain = ("Content-Type", "text/plain")

app :: Application 
app = serve api server

main :: IO ()
main = run 8080 app