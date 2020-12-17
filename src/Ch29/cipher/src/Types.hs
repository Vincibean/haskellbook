module Types
  ( Encryptable
  , Decryptable
  , Key
  , encryptable
  , decryptable
  , extractEncryptable
  , extractDecryptable
  , key
  , extractKey
  )
where

import           Data.Char

newtype Encryptable = Encryptable {extractEncryptable :: String} deriving (Show, Eq)

newtype Decryptable = Decryptable {extractDecryptable :: String} deriving (Show, Eq)

newtype Key = Key {extractKey :: String} deriving (Show, Eq)

encryptable :: String -> Maybe Encryptable
encryptable s = if all isAscii s then Just $ Encryptable s else Nothing

decryptable :: String -> Maybe Decryptable
decryptable s = if all isAscii s then Just $ Decryptable s else Nothing

key :: String -> Maybe Key
key s = if all isAscii s && (not . null) s then Just $ Key s else Nothing
