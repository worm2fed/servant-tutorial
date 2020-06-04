{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Authentication where

import           Data.Aeson                     ( ToJSON )
import           Data.ByteString                ( ByteString )
import           Data.Map                       ( Map
                                                , fromList
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Map                      as Map
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Network.Wai                    ( Request
                                                , requestHeaders
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Servant.API                    ( (:<|>)((:<|>))
                                                , (:>)
                                                , BasicAuth
                                                , Get
                                                , JSON
                                                )
import           Servant.API.BasicAuth          ( BasicAuthData(BasicAuthData) )
import           Servant.API.Experimental.Auth  ( AuthProtect )
import           Servant                        ( throwError )
import           Servant.Server                 ( BasicAuthCheck(BasicAuthCheck)
                                                , BasicAuthResult
                                                    ( Authorized
                                                    , Unauthorized
                                                    )
                                                , Context((:.), EmptyContext)
                                                , err401
                                                , err403
                                                , errBody
                                                , Server
                                                , serveWithContext
                                                , Handler
                                                )
import           Servant.Server.Experimental.Auth
                                                ( AuthHandler
                                                , AuthServerData
                                                , mkAuthHandler
                                                )
import           Web.Cookie                     ( parseCookies )


-- | private data that needs protection 
newtype PrivateData = PrivateData { ssshhh :: Text }
    deriving (Eq, Show, Generic)
instance ToJSON PrivateData

-- | public data than anyone can use
newtype PublicData = PublicData { someData :: Text }
    deriving (Eq, Show, Generic)
instance ToJSON PublicData

-- | a user we'll grab from the database when we authenticate someone
newtype User = User { userName :: Text }
    deriving (Eq, Show)

-- | a type to wrap our public api
type PublicAPI = Get '[JSON] [PublicData]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData

-- | our API 
type BasicAPI = "public"  :> PublicAPI
        :<|>    "private" :> BasicAuth "foo-realm" User :> PrivateAPI 


-- | a value holding a proxy of our API type
basicAuthApi :: Proxy BasicAPI 
basicAuthApi = Proxy 


-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password
authCheck :: BasicAuthCheck User 
authCheck = 
    let 
        check (BasicAuthData "servant" "server") = 
            return $ Authorized (User "servant")
        check (BasicAuthData _         _)        = 
            return Unauthorized
    in BasicAuthCheck check

-- | We need to supply our handlers with the right Context.
-- In this case, Basic Authentication requires a Context Entry with 
-- the 'BasicAuthCheck' value tagged with "foo-tag".
-- This context is then supplied to 'server' and threaded to the 
-- BasicAuth HasServer handlers
basicAuthServerContext :: Context (BasicAuthCheck User ': ' [])
basicAuthServerContext = authCheck :. EmptyContext 

-- | an implementation of our server
-- Here is where we pass all the handlers to our endpoints. 
-- In particular, for the BasicAuth protected handler, we need 
-- to supply a function that takes 'User' as argument.
basicAuthServer :: Server BasicAPI 
basicAuthServer = 
    let 
        publicAPIHandler = return [PublicData "foo", PublicData "bar"]
        privateAPIHandler (user :: User) = 
            return $ PrivateData (userName user)
    in publicAPIHandler :<|> privateAPIHandler

-- | server
basicAuthMain :: IO ()
basicAuthMain = run 8080 $ 
    serveWithContext basicAuthApi basicAuthServerContext basicAuthServer


-- | An account type that we "fetch from the database" after 
-- performing authentication 
newtype Account = Account { unAccount :: Text }

-- | A (pure) database mapping keys to accounts
database :: Map ByteString Account 
database = fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghedalia Tazattes")
                    ]

-- | A method that, when given a password, will return a Account. 
-- This is our bespoke (and bad) authentication logic
lookupAccount :: ByteString -> Handler Account 
lookupAccount key = case Map.lookup key database of 
    Nothing  -> throwError $ err403 { errBody = "Invalid Cookie"}
    Just usr -> return usr

-- | The auth handler wraps a function from Request -> Handler Account.
-- We look for a token in the request headers that we expect to be in 
-- the cookie. The token is then passed to our `lookupAccount` function
authHandler :: AuthHandler Request Account 
authHandler = mkAuthHandler handler
    where 
        maybe2either e = maybe (Left e) Right 
        throw401 msg = throwError $ err401 { errBody = msg }
        handler req = either throw401 lookupAccount $ do 
            cookie <- maybe2either "Missing cookie header" 
                    $ lookup "cookie" 
                    $ requestHeaders req
            maybe2either "Missing token in cookie" 
                    $ lookup "servant-auth-cookie"
                    $ parseCookies cookie

-- | Our API, with auth-protection
type AuthGenAPI = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
             :<|> "public"  :> PublicAPI 

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy 

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Account 

-- | The context that will be made available to request handlers. 
-- We supply the "cookie-auth"-tagged request handler defined above, so
-- that the 'HasServer' instance of 'AuthProtect' can extract the 
-- handler and run it on the request
genAuthServerContext :: Context (AuthHandler Request Account ': ' [])
genAuthServerContext = authHandler :. EmptyContext 

-- | Our APi, where we provide all the author-supplied handlers for 
-- each endpoint. Note that 'privateDataFunc' us a function that takes 
-- 'Account' as an argument. We don't worry about the authentication 
-- instrumentation here, that is taken care of by supplying context
genAuthServer :: Server AuthGenAPI
genAuthServer = 
    let 
        privateDataFunc (Account name) = 
            return $ PrivateData ("this is a secret: " <> name)
        publicData = 
            return [PublicData "this is a public piece of data"]
    in privateDataFunc :<|> publicData 

-- | run our server 
genAuthMain :: IO ()
genAuthMain = run 8080 $ 
    serveWithContext genAuthAPI genAuthServerContext genAuthServer


-- | The datatype we'll use to authenticate a request. 
-- If we were wrapping something like OAuth, this might be a 
-- Bearer token
-- type instance AuthClientData (AuthProtect "cookie-auth") = String 

-- | A method to authenticate a request
-- authenticateReq :: String -> Req -> Req 
-- authenticateReq s req = addHeader "my-bespoke-header" s req

-- | one could curry this to make it simpler to work with 
-- result = runExceptT $ getProtected $ mkAuthenticateReq "secret" authenticateReq
