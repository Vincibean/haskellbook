{-# LANGUAGE OverloadedStrings #-}

module Fingerd where

import           Database                       ( handleQueries )
import           Database.SQLite.Simple         ( open )
import qualified Database.SQLite.Simple        as SQLite
import           Network.Socket                 ( setCloseOnExecIfNeeded
                                                , defaultHints
                                                , getAddrInfo
                                                , withSocketsDo
                                                , setSocketOption
                                                , gracefulClose
                                                , bind
                                                , listen
                                                , socket
                                                , defaultProtocol
                                                , withFdSocket
                                                , AddrInfo
                                                  ( addrFlags
                                                  , addrSocketType
                                                  , addrFamily
                                                  , addrAddress
                                                  )
                                                , AddrInfoFlag(AI_PASSIVE)
                                                , SocketOption(ReuseAddr)
                                                , SocketType(Stream)
                                                )

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream })
    Nothing
    (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  gracefulClose sock 5000
