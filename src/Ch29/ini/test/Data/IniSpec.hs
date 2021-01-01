{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.IniSpec where

import           Data.Ini                       ( parseAssignment
                                                , parseHeader
                                                , parseIni
                                                , parseSection
                                                , skipComments
                                                , Config(Config)
                                                , Header(Header)
                                                , Section(Section)
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( isAlpha )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import           Text.RawString.QQ              ( r )
import           Text.Trifecta                  ( parseByteString
                                                , Result(Success)
                                                )

spec :: Spec
spec = do
  describe "Assignment Parsing" $ it "can parse a simple assignment" $ do
    let m  = parseByteString parseAssignment mempty assignmentEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just ("woot", "1")
  describe "Header Parsing" $ it "can parse a simple header" $ do
    let m  = parseByteString parseHeader mempty headerEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")

  describe "Comment parsing" $ it "Skips comment before header" $ do
    let p  = skipComments >> parseHeader
        i  = "; woot\n[blah]"
        m  = parseByteString p mempty i
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")

  describe "Section parsing" $ it "can parse a simple section" $ do
    let m         = parseByteString parseSection mempty sectionEx
        r'        = maybeSuccess m
        states    = M.fromList [("Chris", "Texas")]
        expected' = Just (Section (Header "states") states)
    print m
    r' `shouldBe` expected'

  describe "INI parsing" $ it "Can parse multiple sections" $ do
    let
      m              = parseByteString parseIni mempty sectionEx''
      r'             = maybeSuccess m
      sectionValues  = M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
      whatisitValues = M.fromList [("red", "intoothandclaw")]
      expected'      = Just
        (Config
          (M.fromList
            [ (Header "section" , sectionValues)
            , (Header "whatisit", whatisitValues)
            ]
          )
        )
    print m
    r' `shouldBe` expected'

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing

headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1"

commentEx :: ByteString
commentEx = "; last modified 1 April\
    \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n \n;hah"


sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]
