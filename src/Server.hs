{-# LANGUAGE DataKinds
    , DeriveGeneric
    , FlexibleInstances
    , MultiParamTypeClasses
    , OverloadedStrings
    , RankNTypes
    , ScopedTypeVariables
    , TypeOperators 
#-}

module Server where

import           Prelude                        ( )
import           Prelude.Compat

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import           Text.Blaze
import           Text.Blaze.Html.Renderer.Utf8
import           Servant.Types.SourceT          ( source )
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)
instance ToJSON User

users1 :: [User]
users1 =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

server1 :: Server UserAPI1
server1 = return users1

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy

-- 'serve' comes from servant and hands a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1 server1

type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy

app2 :: Application
app2 = serve userAPI2 server2


type API = "position"  :> Capture "x" Int
                       :> Capture "y" Int
                       :> Get '[JSON] Position
      :<|> "hello"     :> QueryParam "name" String
                       :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo
                       :> Post '[JSON] Email

data Position = Position
    { xCoord :: Int
    , yCoord :: Int
    } deriving Generic
instance ToJSON     Position

newtype HelloMessage = HelloMessage { msg :: String }
    deriving Generic
instance ToJSON     HelloMessage

data ClientInfo = ClientInfo
    { clientName :: String
    , clientEmail :: String
    , clientAge :: Int
    , clientInterestedIn :: [String]
    } deriving Generic
instance FromJSON   ClientInfo
instance ToJSON     ClientInfo

data Email = Email
    { from :: String
    , to :: String
    , subject :: String
    , body :: String
    } deriving Generic
instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
    where
        from'    = "info@cactus.vision"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing
    where
        position :: Int -> Int -> Handler Position
        position x y = return $ Position x y

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
            Nothing -> "Hello, anonymous coward"
            Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing c = return $ emailForClient c

api :: Proxy API
api = Proxy

app :: Application
app = serve api server3


data HTMLLucid
instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml
-- let's also provide an instance for lucid's 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS

data HTMLBlaze
instance Accept HTMLBlaze where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml
-- provide an instance for rendering blaze's 'Html' type
instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
    mimeRender _ = renderHtml

type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

data Person = Person
    { firstName :: String
    , lastName :: String
    } deriving Generic
instance ToJSON Person -- for the JSON instance
-- HTML serialization of a single person
instance ToHtml Person where
    toHtml person = tr_ $ do
        td_ (toHtml $ firstName person)
        td_ (toHtml $ lastName  person)
    toHtmlRaw = toHtml
-- HTML serialization of a list of persons
instance ToHtml [Person] where
    toHtml persons = table_ $ do
        tr_ $ do
            th_ "first name"
            th_ "last name"
        -- this just calls toHtml on each person of the list
        -- and concatenates the resulting pieces of HTML together
        foldMap toHtml persons
    toHtmlRaw = toHtml

people :: [Person]
people =
    [ Person "Isaac"  "Newton"
    , Person "Albert" "Einstein"
    ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people

app4 :: Application
app4 = serve personAPI server4


type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent { content :: String }
    deriving Generic
instance ToJSON FileContent

ioAPI1 :: Proxy IOAPI1
ioAPI1 = Proxy

server5 :: Server IOAPI1
server5 = do
    fileContent <- liftIO (readFile "myfile.txt")
    return (FileContent fileContent)

app5 :: Application
app5 = serve ioAPI1 server5

failingHandler :: Handler ()
failingHandler = throwError err
    where
        err :: ServerError
        err = err503 { errBody = "Sorry dear user" }

server6 :: Server IOAPI1
server6 = do
        exists <- liftIO (doesFileExist "myfile.txt")
        if exists
            then liftIO (readFile "myfile.txt") >>= return . FileContent
            else throwError custom404Err
    where
        custom404Err = err404 { errBody = "myfile.txt just isn't there" }

app6 :: Application
app6 = serve ioAPI1 server6


type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1791 albert

type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Boll" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert

type MyMaybeHeaderHandler = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert
                                       else noHeader albert


type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

app7 :: Application
app7 = serve staticAPI server7


type UserAPI13 = -- view the user with given userid, in JSON
                 Capture "userid" Int :> Get '[JSON] User
            :<|> -- delete the user with given userid. empty response
                 Capture "userid" Int :> DeleteNoContent '[JSON] NoContent

server8 :: Server UserAPI13
server8 = getUser :<|> deleteUser
  where
    getUser :: Int -> Handler User
    getUser _userid = error "..."

    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = error "..."

type UserAPI14 = Capture "userid" Int :>
    (    Get '[JSON] User
    :<|> DeleteNoContent '[JSON] NoContent
    )

server9 :: Server UserAPI14
server9 _userid = getUser _userid :<|> deleteUser _userid
  where
    getUser :: Int -> Handler User
    getUser = error "..."

    deleteUser :: Int -> Handler NoContent
    deleteUser = error "..."

-- we just factor out the "users" path fragment
type API1 = "users" :>
    (    Get '[JSON] [User] -- user listing
    :<|> Capture "userid" Int :> Get '[JSON] User -- view a particlar user
    )

-- we factor out the Request Body
type API2 = ReqBody '[JSON] User :>
    (    -- just display the same user back
         Get '[JSON] User
    :<|> -- register the user, empty response
         PostNoContent '[JSON] NoContent
    )

-- we factor out a Header
type API3 = Header "Authorization" Token :>
    (    -- get some secret data, if authorized
         Get '[JSON] SecretData
    :<|> -- add some secret data, if authorized
         ReqBody '[JSON] SecretData :> PostNoContent '[JSON] NoContent
    )
newtype Token = Token ByteString
newtype SecretData = SecretData ByteString


type UsersAPI =  -- list users
             Get '[JSON] [User]
                -- add user
        :<|> ReqBody '[JSON] User :> PostNoContent '[JSON] NoContent
        :<|> Capture "userid" Int :>
            (    -- view user
                 Get '[JSON] User
                 -- update a user
            :<|> ReqBody '[JSON] User :> PutNoContent '[JSON] NoContent
                 -- delete a user
            :<|> DeleteNoContent '[JSON] NoContent
            )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations
    where
        getUsers :: Handler [User]
        getUsers = error "..."

        newUser :: User -> Handler NoContent
        newUser = error "..."

        userOperations userid =
            viewUser userid :<|> updateUser userid :<|> deleteUser userid
            where
                viewUser :: Int -> Handler User
                viewUser = error "..."

                updateUser :: Int -> User -> Handler NoContent
                updateUser = error "..."

                deleteUser :: Int -> Handler NoContent
                deleteUser = error "..."

type ProductsAPI =  -- list products
             Get '[JSON] [Product]
                    -- add a product
        :<|> ReqBody '[JSON] Product :> PostNoContent '[JSON] NoContent
        :<|> Capture "productid" Int :>
            (    -- view a product
                 Get '[JSON] Product
                 -- update a product
            :<|> ReqBody '[JSON] Product :> PutNoContent '[JSON] NoContent
                 -- delete a product
            :<|> DeleteNoContent '[JSON] NoContent
            )
newtype Product = Product { productId :: Int }

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations
    where
        getProducts :: Handler [Product]
        getProducts = error "..."

        newProduct :: Product -> Handler NoContent
        newProduct = error "..."

        productOperations productid =
            viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
            where
                viewProduct :: Int -> Handler Product
                viewProduct = error "..."

                updateProduct :: Int -> Product -> Handler NoContent
                updateProduct = error "..."

                deleteProduct :: Int -> Handler NoContent
                deleteProduct = error "..."

type CombinedAPI = "users"    :> UsersAPI
              :<|> "products" :> ProductsAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer


-- API for values of type 'a'
-- indexed by values of type 'i'
type APIFor a i =   -- list 'a's
                 Get '[JSON] [a]
                    -- add an 'a'
            :<|> ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent
            :<|> Capture "id" i :>
                (   -- view an 'a' given its "identifier" of type 'i'
                     Get '[JSON] a
                    -- update an 'a'
                :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent
                    -- delete an 'a'
                :<|> DeleteNoContent '[JSON] NoContent
                )

-- build the appropriate 'Server'
-- given the handlers of the right type
serverFor :: Server (APIFor a i)
serverFor = get :<|> new :<|> operations
    where
        get :: Handler [a]
        get = error "..."

        new :: a -> Handler NoContent
        new = error "..."

        operations id =
            view id :<|> update id :<|> delete id
            where
                view :: i -> Handler a
                view = error "..."

                update :: i -> a -> Handler NoContent
                update = error "..."

                delete :: i -> Handler NoContent
                delete = error "..."


type CombinedAPI2 = API :<|> "empty" :> EmptyAPI

server11 :: Server CombinedAPI2
server11 = server3 :<|> emptyServer


type (~>) m n = forall a. m a -> n a

list2Maybe :: [] ~> Maybe
list2Maybe = listToMaybe

reader2handler :: Reader String a -> Handler a
reader2handler r = return $ runReader r "hi"

type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b
    where
        a :: Reader String Int
        a = return 1797

        b :: Double -> Reader String Bool
        b _ = asks (== "hi")

readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI reader2handler readerServerT

readerApp :: Application
readerApp = serve readerAPI readerServer


funServerT :: ServerT ReaderAPI ((->) String)
funServerT = a :<|> b
    where
        a :: String -> Int
        a _ = 1797

        b :: Double -> String -> Bool
        b _ s = s == "hi"

fun2handler :: (String -> a) -> Handler a
fun2handler f = return (f "hi")

funApp :: Application
funApp = serve readerAPI $ hoistServer readerAPI fun2handler funServerT


type StreamAPI = "userStream" :> StreamGet NewlineFraming JSON (SourceIO User)

streamAPI :: Proxy StreamAPI
streamAPI = Proxy

streamUsers :: SourceIO User
streamUsers = source [isaac, albert, albert]

streamApp :: Application
streamApp = serve streamAPI $ return streamUsers

main :: IO ()
main = run 8081 streamApp