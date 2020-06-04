{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Javascript where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Proxy
import           Data.Text                     as T
                                                ( Text )
import           Data.Text.IO                  as T
                                                ( writeFile
                                                , readFile
                                                )
import           GHC.Generics
import           Language.Javascript.JQuery
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Data.Text as T
import           Servant
import           Servant.JS
import           System.Random


data Point = Point 
    { x :: Double 
    , y :: Double 
    } deriving Generic 
instance ToJSON Point 

randomPoint :: MonadIO m => m Point
randomPoint = liftIO . getStdRandom $ \g -> 
    let (rx, g')  = randomR (-1, 1) g
        (ry, g'') = randomR (-1, 1) g'
    in (Point rx ry, g'')


data Search a = Search 
    { query   :: Text 
    , results :: [a]
    } deriving Generic 
instance ToJSON a => ToJSON (Search a)

mkSearch :: Text -> [a] -> Search a 
mkSearch = Search

data Book = Book 
    { author :: Text 
    , title  :: Text 
    , year   :: Int 
    } deriving Generic 
instance ToJSON Book 

book :: Text -> Text -> Int -> Book 
book = Book

books :: [Book]
books =
    [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
    , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
    , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
    , book "Graham Hutton" "Programming in Haskell" 2007
    , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
    , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
    ]

searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing = return $ mkSearch "" books
searchBook (Just q) = return $ mkSearch q books'
    where
        books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
                            || q' `T.isInfixOf` T.toLower (title b)
                        )
                        books
        q' = T.toLower q


type API = "point" :> Get '[JSON] Point 
      :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)
    
type API' = API :<|> Raw 

api :: Proxy API
api = Proxy 

api' :: Proxy API'
api' = Proxy 

server :: Server API 
server = randomPoint 
    :<|> searchBook

server' :: Server API'
server' = server 
    :<|> serveDirectoryFileServer "static-files"

app :: Application
app = serve api' server'

main :: IO ()
main = run 8080 app


apiJS1 :: Text 
apiJS1 = jsForAPI api jquery

writeJSFiles :: IO ()
writeJSFiles = do 
    T.writeFile "static-files/api.js" apiJS1
    jq <- T.readFile =<< Language.Javascript.JQuery.file 
    T.writeFile "static-files/jq.js" jq

apiJS2 :: Text 
apiJS2 = jsForAPI api vanillaJS

apiJS3 :: Text 
apiJS3 = jsForAPI api $ axios defAxiosOptions

data AxiosOptions = AxiosOptions 
    { withCredentials :: !Bool
    , xsrfCookieName  :: !(Maybe Text)
    , xsrfHeaderName  :: !(Maybe Text)
    }

apiJS4 :: Text 
apiJS4 = jsForAPI api $ angular defAngularOptions

apiJS5 :: Text 
apiJS5 = jsForAPI api $ angularService defAngularOptions

data AngularOptions = AngularOptions 
    { serviceName :: Text 
    , prologue    :: Text -> Text -> Text 
    , epilogue    :: Text 
    }

apiJS6 :: Text 
apiJS6 = jsForAPI api $ jqueryWith defCommonGeneratorOptions 
    { functionNameBuilder = snakeCase }