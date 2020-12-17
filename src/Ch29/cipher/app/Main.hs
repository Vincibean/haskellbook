{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.Word                      ( Word16 )
import qualified Cipher                        as C
import           Types                          ( Decryptable
                                                  ( extractDecryptable
                                                  )
                                                , Encryptable
                                                  ( extractEncryptable
                                                  )
                                                , decryptable
                                                , encryptable
                                                , key
                                                )
import           System.Console.CmdArgs         ( Data
                                                , Typeable
                                                , (&=)
                                                , cmdArgs
                                                , enum
                                                , help
                                                , name
                                                , summary
                                                , verbosity
                                                , Default(def)
                                                )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           System.IO                      ( hSetBuffering
                                                , stderr
                                                , stdin
                                                , stdout
                                                , hGetLine
                                                , hPutStrLn
                                                , hWaitForInput
                                                , BufferMode(NoBuffering)
                                                )
import           Control.Monad                  ( forever
                                                , when
                                                , unless
                                                )

data Op = Encrypt | Decrypt deriving (Show, Eq, Data, Typeable)

data Program = Program {op :: Op, stringKey :: String, waitTimeMillis :: Word16} deriving (Show, Eq, Data, Typeable)

defaultWaitTimeMillis :: Word16
defaultWaitTimeMillis = 5 * 1000

program :: Program
program =
  Program
      { stringKey      = def
        &= help "The string to use as key. It must not be an empty string"
      , op             = enum
                           [ Encrypt &= help "Encrypt a message" &= name "e"
                           , Decrypt &= help "Decrypt a message" &= name "d"
                           ]
      , waitTimeMillis =
        defaultWaitTimeMillis
          &= help
               "Timeout if no input is provided within a span of time of your choosing (unit: milliseconds)"
      }
    &= help "Use Program's encryption / decryption algorithm"

cipher :: Program
cipher =
  program
    &= verbosity
    &= help "Encrypt and decrypt messages using various algorithms"
    &= summary "cipher-cli v0.0.0.1, (C) Vincibean"

codec :: Program -> String -> String
codec (Program Encrypt stringKey _) msg =
  maybe "Unencryptable messagge" extractDecryptable
    $   C.vigenere
    <$> key stringKey
    <*> encryptable msg
codec (Program Decrypt stringKey _) msg =
  maybe "Undecryptable messagge" extractEncryptable
    $   C.unVigenere
    <$> key stringKey
    <*> decryptable msg

encode :: Program -> IO ()
encode algo = do
  let timeout = fromIntegral . waitTimeMillis $ algo
  available <- hWaitForInput stdin timeout
  when available $ encode' algo
  unless available $ hPutStrLn stderr "Program Timeout" >> exitWith
    (ExitFailure 2)

encode' :: Program -> IO ()
encode' algo = do
  msg <- hGetLine stdin
  let msg' = codec algo msg
  hPutStrLn stdout msg'

main :: IO b
main = do
  hSetBuffering stdout NoBuffering
  algo <- cmdArgs cipher
  forever $ encode algo
