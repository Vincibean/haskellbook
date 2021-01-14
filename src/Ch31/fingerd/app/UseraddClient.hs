{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module UseraddClient where

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text                     as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import           System.Console.CmdArgs
import           Types                          ( NewUser(NewUser)
                                                , User(User)
                                                )

data Useradd = Useradd
  { username      :: String
  , shell         :: String
  , homeDirectory :: String
  , realName      :: String
  , phone         :: String
  }
  deriving (Eq, Show, Data, Typeable)

useradd :: Useradd
useradd =
  Useradd
      { username      = def &= help "The username of the user"
      , shell         = def &= help "The shell of the user"
      , homeDirectory = def &= help "The home directory of the user"
      , realName      = def &= help "The real name of the user"
      , phone         = def &= help "The phone number of the user"
      }
    &= verbosity
    &= help "Add new users to the finger database"
    &= summary "useradd v0.0.0.1, (C) Vincibean"

asNewUser :: Useradd -> NewUser
asNewUser (Useradd usrname shll homeDir realNam phon) = NewUser
  (T.pack usrname)
  (T.pack shll)
  (T.pack homeDir)
  (T.pack realNam)
  (T.pack phon)

main :: IO ()
main = do
  usradd <- cmdArgs useradd
  putStr "Received options: "
  print usradd
  let newUsr = asNewUser usradd
  putStr "User to add: "
  print newUsr
  runClient newUsr 

runClient :: NewUser -> IO ()
runClient newUsr = runTCPClient "127.0.0.1" "3000" $ \s -> do
    let msg2send = encode newUsr
    sendAll s msg2send
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

encode :: NewUser -> ByteString
encode (NewUser newUsername newShell newHomeDirectory newRealName newphone) =
    B8.pack $ T.unpack newUsername <> "\t" <> T.unpack newShell <> "\t" <> T.unpack newHomeDirectory <> "\t" <> T.unpack newRealName <> "\t" <> T.unpack  newphone
