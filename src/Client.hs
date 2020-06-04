{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Servant.API
import           Servant.Client
import           Servant.Types.SourceT          ( foreach )

import qualified Servant.Client.Streaming      as S


-- Data definitions
data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    } deriving (Show, Generic)
instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
    deriving (Show, Generic)
instance FromJSON HelloMessage

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    } deriving Generic
instance ToJSON ClientInfo

data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    } deriving (Show, Generic)
instance FromJSON Email


-- API definitions
type API = "position" 
        :> Capture "x" Int 
        :> Capture "y" Int 
        :> Get '[JSON] Position
    :<|> "hello"
        :> QueryParam "name" String 
        :> Get '[JSON] HelloMessage
    :<|> "marketing"
        :> ReqBody '[JSON] ClientInfo
        :> Post '[JSON] Email

api :: Proxy API 
api = Proxy 


-- Client definitions
position :: Int -> Int -> ClientM Position

hello :: Maybe String -> ClientM HelloMessage

marketing :: ClientInfo -> ClientM Email

position :<|> hello :<|> marketing = client api

queries :: ClientM (Position, HelloMessage, Email)
queries = do 
    pos <- position 10 10
    message <- hello (Just "servant")
    em <- marketing (ClientInfo 
            "worm2fed" 
            "worm2fed@sonus.space" 
            24 
            ["haskell", "spiral dynamics"]
        )
    return (pos, message, em)

run :: IO ()
run = do 
    manager' <- newManager defaultManagerSettings 
    res <- runClientM queries (mkClientEnv manager' 
            (BaseUrl Http "localhost" 8081 "")
        )
    case res of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right (pos, message, em) -> do
            print pos
            print message
            print em


-- changing the monad for client
type HoistClientAPI = Get '[JSON] Int
    :<|> Capture "n" Int :> Post '[JSON] Int 

hoistClientAPI :: Proxy HoistClientAPI 
hoistClientAPI = Proxy 

getIntClientM :: ClientM Int 
postIntClientM :: Int -> ClientM Int 
getIntClientM :<|> postIntClientM = client hoistClientAPI

-- conversion function has type: forall a. ClientM a -> IO a 
-- result has type: Client IO HoistClientAPI = IO Int :<|> (Int -> IO Int)
getClients :: ClientEnv -> Client IO HoistClientAPI
getClients clientEnv = hoistClient hoistClientAPI 
    ( fmap (either (error . show) id)
    . flip runClientM clientEnv
    )
    (client hoistClientAPI)


-- querying streaming API
type StreamAPI = "positionStream" 
    :> StreamGet NewlineFraming JSON (SourceIO Position)

streamAPI :: Proxy StreamAPI 
streamAPI = Proxy 

posStream :: S.ClientM (SourceIO Position)
posStream = S.client streamAPI

printSourceIO :: Show a => ClientEnv -> S.ClientM (SourceIO a) -> IO ()
printSourceIO env c = S.withClientM c env $ \e -> case e of 
    Left err -> putStrLn $ "Error: " ++ show err 
    Right rs -> foreach fail print rs