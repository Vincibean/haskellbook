module CipherSpec where

import           Cipher
import           Data.Char
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck
import           Types

newtype ArbKey = ArbKey Key deriving (Eq, Show)

instance Arbitrary ArbKey where
  arbitrary = (ArbKey . fromJust) . key <$> listOf1
    (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

newtype ArbEncryptable = ArbEncryptable Encryptable deriving (Eq, Show)

instance Arbitrary ArbEncryptable where
  arbitrary = (ArbEncryptable . fromJust) . encryptable <$> listOf1
    (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

spec :: Spec
spec = describe "vigenere" $ do
  it "should ciper a specific string as intended" $ do
    let hello = fromJust $ encryptable "hello"
    let k     = fromJust $ key "key"
    let res   = extractDecryptable . vigenere k $ hello
    res `shouldBe` "rijvs"
  it "should deciper a specific string as intended" $ do
    let rijvs = fromJust $ decryptable "rijvs"
    let k     = fromJust $ key "key"
    let res   = extractEncryptable . unVigenere k $ rijvs
    res `shouldBe` "hello"
  it "should cipher and decipher any word using any key"
    $ property
    $ \(ArbEncryptable s) (ArbKey k) ->
        (extractEncryptable . unVigenere k . vigenere k) s
          === extractEncryptable s
