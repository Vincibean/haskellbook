
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database where

import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Control.Monad                  ( forever )
import           Data.List                      ( intersperse )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.Typeable                  ( Typeable )
import           Database.SQLite.Simple         ( execute
                                                , execute_
                                                , open
                                                , query
                                                , query_
                                                , Only(Only)
                                                , Connection
                                                , Query
                                                )
import qualified Database.SQLite.Simple        as SQLite
import           Database.SQLite.Simple.Types   ( Only(Only)
                                                , Query
                                                , Null(Null)
                                                )
import           Network.Socket                 ( gracefulClose
                                                , accept
                                                , Socket
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           Text.RawString.QQ              ( r )
import           Domain                         ( User(..)
                                                , NewUser
                                                  ( newUsername
                                                  , newShell
                                                  , newHomeDirectory
                                                  , newRealName
                                                  , newphone
                                                  )
                                                )

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Integer, Text, Text, Text, Text, Text)

type NewUserRow = (Null, Text, Text, Text, Text, Text)

type UpdateUserRow = (Text, Text, Text, Text, Text, Integer)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

updateUserQuery :: Query
updateUserQuery =
  "UPDATE users\
  \ SET username =      ?,\
  \     shell =         ?,\
  \     homeDirectory = ?,\
  \     realName =      ?,\
  \     phone =         ?\
  \ WHERE id =          ?"

insertNewUser :: NewUser -> IO ()
insertNewUser usr = do
  conn <- open "finger.db"
  execute conn insertUser row
  SQLite.close conn
 where
  row :: NewUserRow
  row =
    ( Null
    , newUsername usr
    , newShell usr
    , newHomeDirectory usr
    , newRealName usr
    , newphone usr
    )

updateUser :: User -> IO ()
updateUser usr = do
  conn <- open "finger.db"
  execute conn updateUserQuery row
  SQLite.close conn
 where
  row :: UpdateUserRow
  row =
    ( username usr
    , shell usr
    , homeDirectory usr
    , realName usr
    , phone usr
    , userId usr
    )

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> return Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
 where
  meRow :: NewUserRow
  meRow =
    (Null, "callen", "/bin/zsh", "/home/callen", "Chris Allen", "555-123-4567")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames        = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  [ "Login: "
  , e username
  , "\t\t\t\t"
  , "Name: "
  , e realName
  , "\n"
  , "Directory: "
  , e homeDir
  , "\t\t\t"
  , "Shell: "
  , e shell
  , "\n"
  ]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couldn't find matching user\
                       \ for username: "
        ++ show username
        )
      return ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name   -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  gracefulClose soc 5000
