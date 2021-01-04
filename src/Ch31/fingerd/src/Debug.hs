module Debug where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Stream }) Nothing (Just "79")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock (addrAddress serveraddr)
    listen sock 1024
    logAndEcho sock
    gracefulClose sock 5000

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
    (soc, _) <- accept sock
    printAndKickback soc
    gracefulClose soc 5000
    where printAndKickback conn = do
            msg <- recv conn 1024
            print msg
            sendAll conn msg