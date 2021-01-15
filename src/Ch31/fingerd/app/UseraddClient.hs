{-# LANGUAGE OverloadedStrings #-}

module UseraddClient where

import           Cli                            ( asNewUser
                                                , useradd
                                                )
import qualified Control.Exception             as E
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Char8         as C
import qualified Data.Text                     as T
import           Network.Socket                 ( Socket
                                                , defaultHints
                                                , getAddrInfo
                                                , withSocketsDo
                                                , connect
                                                , socket
                                                , close
                                                , AddrInfo
                                                  ( addrAddress
                                                  , addrFamily
                                                  , addrSocketType
                                                  , addrProtocol
                                                  )
                                                , HostName
                                                , ServiceName
                                                , SocketType(Stream)
                                                )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           System.Console.CmdArgs         ( cmdArgs )
import           Domain                         ( NewUser(NewUser) )

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
openSocket addr =
  socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

encode :: NewUser -> ByteString
encode (NewUser newUsername newShell newHomeDirectory newRealName newphone) =
  B8.pack
    $  T.unpack newUsername
    <> "\t"
    <> T.unpack newShell
    <> "\t"
    <> T.unpack newHomeDirectory
    <> "\t"
    <> T.unpack newRealName
    <> "\t"
    <> T.unpack newphone
