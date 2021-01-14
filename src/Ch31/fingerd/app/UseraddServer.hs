{-# LANGUAGE OverloadedStrings #-}

module UseraddServer where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Data.ByteString (ByteString)
import Database
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8         as B8
import Data.Text (Text) 
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Types
import Unsafe.Coerce ( unsafeCoerce )

main :: IO ()
main = do
  putStrLn "Starting..."
  runTCPServer Nothing "3000" talk
  where
    talk s = do
        putStrLn "Server running"
        msg <- recv s 1024
        putStr "Received message: "
        B8.putStrLn msg
        unless (S.null msg) $ do
          let usr = decode msg
          putStr "User to add: "
          print usr
          insertNewUser usr
          sendAll s msg
          talk s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

decode :: ByteString -> NewUser
decode bs = 
  let tab = unsafeCoerce $ fromEnum '\t'
      [username', shell', homeDirectory', realName', phone'] = S.split tab bs
      user = NewUser (toText username') (toText shell') (toText homeDirectory') (toText realName') (toText phone')
  in user

toText :: ByteString -> Text
toText = T.pack . B8.unpack