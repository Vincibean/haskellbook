{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Ch24.Ch24Spec where


import           Ch24.Ch24
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                , Expectation
                                                )
import           Test.QuickCheck                ( (==>)
                                                , ASCIIString(ASCIIString)
                                                , NonEmptyList(NonEmpty)
                                                , Positive(Positive)
                                                , Testable(property)
                                                )
import           Test.HUnit                     ( assertFailure )
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( isInfixOf )
import           Data.Ratio                     ( (%) )
import           Text.Trifecta                  ( some
                                                , parseString
                                                , CharParsing(string)
                                                , Parser
                                                , Result(..)
                                                )
import           Text.RawString.QQ              ( r )

spec :: Spec
spec = do
  describe "Parsing practice" $ do
    it "1." $ do
      parseString' one'' "1" `shouldBeSuccess` '1'
      shouldBeFailure $ parseString' one'' "123"
      shouldBeFailure $ parseString' one'' "2"
      parseString' oneTwo'' "12" `shouldBeSuccess` '2'
      shouldBeFailure $ parseString' oneTwo'' "123"
      shouldBeFailure $ parseString' oneTwo'' "345"
    it "2." $ do
      shouldBeFailure $ parseString' p123 ""
      shouldBeFailure $ parseString' p123 "a"
      parseString' p123 "1" `shouldBeSuccess` "1"
      parseString' p123 "12" `shouldBeSuccess` "12"
      parseString' p123 "123" `shouldBeSuccess` "123"
      parseString' p123 "1234" `shouldBeSuccess` "123"
    it "3. (equality)" $ property $ \(ASCIIString s) -> do
      parseString' (string s) s `shouldBeSuccess` s
      parseString' (string' s) s `shouldBeSuccess` s
    it "3. (disequality)" $ property $ \(NonEmpty s) s' ->
      not (s `isInfixOf` s') ==> do
        shouldBeFailure $ parseString' (string s) s'
        shouldBeFailure $ parseString' (string' s) s'
  describe "Unit of success" $ it "parses" $ do
    parseString' one'' "1" `shouldBeSuccess` '1'
    parseString' integerEof "123" `shouldBeSuccess` 123
    shouldBeFailure $ parseString' integerEof "123abc"
  describe "Try try" $ do
    it "Integer" $ property $ \(Positive i) ->
      parseString' integerOrRational (show i) `shouldBeSuccess` Integer' i
    it "Rational" $ property $ \(Positive i) (Positive i') ->
      parseString' integerOrRational (show i <> "/" <> show i')
        `shouldBeSuccess` Rational' (i % i')
  describe "Parse a list of numbers or strings" $ do
    it "parses an empty list" $ dec' "[]" `shouldBe` Just []
    it "parses a list of one element" $ dec' "[123]" `shouldBe` Just [Numba 123]
    it "parses a list of many elements"
      $          dec' "[123, \"blah\", \"123\"]"
      `shouldBe` Just [Numba 123, Stringy "blah", Stringy "123"]
  describe "Chapter exercise: 1. Write a parser for semantic versions" $ do
    it "can pass the tests in the book" $ do
      parseString' parseSemVer "2.1.1" `shouldBeSuccess` SemVer 2 1 1 [] []
      parseString' parseSemVer "1.0.0-x.7.z.92"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      parseString' parseSemVer "1.0.0-gamma+002"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "gamma"] [NOSI 2]
      parseString' parseSemVer "1.0.0-beta+oof.sha.41af286" `shouldBeSuccess` SemVer 1 0 0 [NOSS "beta"] [NOSS "oof", NOSS "sha", NOSS "41af286"]
      SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] `shouldBe` True
    it "can parse normal version numbers"
      $                 parseString' parseSemVer "1.11.0"
      `shouldBeSuccess` SemVer 1 11 0 [] []
    it "can parse pre-release version numbers" $ do
      parseString' parseSemVer "1.0.0-alpha"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "alpha"] []
      parseString' parseSemVer "1.0.0-alpha.1"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "alpha", NOSI 1] []
      parseString' parseSemVer "1.0.0-0.3.7"
        `shouldBeSuccess` SemVer 1 0 0 [NOSI 0, NOSI 3, NOSI 7] []
      parseString' parseSemVer "1.0.0-x.7.z.92"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
      parseString' parseSemVer "1.0.0-x-y-z.-"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "x-y-z", NOSS "-"] []
    it "can parse metadata numbers" $ do
      parseString' parseSemVer "1.0.0-alpha+001"
        `shouldBeSuccess` SemVer 1 0 0 [NOSS "alpha"] [NOSI 1]
      parseString' parseSemVer "1.0.0+20130313144700"
        `shouldBeSuccess` SemVer 1 0 0 [] [NOSI 20130313144700]
      parseString' parseSemVer "1.0.0-beta+exp.sha.a5114f85"
        `shouldBeSuccess` SemVer 1
                                 0
                                 0
                                 [NOSS "beta"]
                                 [NOSS "exp", NOSS "sha", NOSS "a5114f85"]
      parseString' parseSemVer "1.0.0+a21AF26D3--117B344092BD"
        `shouldBeSuccess` SemVer 1 0 0 [] [NOSS "a21AF26D3--117B344092BD"]
  describe "Chapter exercise: 2. Write a parser for positive integer values"
    $ do
        it "can parse a digit" $ do
          parseString' parseDigit "1" `shouldBeSuccess` '1'
          parseString' parseDigit "123" `shouldBeSuccess` '1'
          shouldBeFailure $ parseString' parseDigit "a"
          shouldBeFailure $ parseString' parseDigit "abc"
        it "can parse any digit" $ traverse_
          (\d -> parseString' parseDigit (d : "") `shouldBeSuccess` d)
          ['0' .. '9']
        it "can parse an integer" $ do
          parseString' base10Integer "12" `shouldBeSuccess` 12
          parseString' (some parseDigit) "123abc"
            `shouldBeSuccess` ['1', '2', '3']
          parseString' base10Integer "123abc" `shouldBeSuccess` 123
          shouldBeFailure $ parseString' base10Integer "abc"
        it "can parse any integer" $ property $ \(Positive i) ->
          parseString' base10Integer (show i) `shouldBeSuccess` i
  describe
      "Chapter exercise: 3. Extend the parser you wrote to handle negative and positive integers. "
    $ do
        it "can parse positive integer (no plus sign)"
          $                 parseString' negAndPosInteger "12"
          `shouldBeSuccess` 12
        it "can parse positive integer (plus sign)"
          $                 parseString' negAndPosInteger "+12"
          `shouldBeSuccess` 12
        it "can parse negative integer (plus sign)"
          $                 parseString' negAndPosInteger "-12"
          `shouldBeSuccess` (-12)
        it "can parse any integer" $ property $ \i ->
          parseString' negAndPosInteger (show i) `shouldBeSuccess` i
  describe
      "Chapter exercise: 4. Write a parser for US/Canada phone numbers with varying formats"
    $ it "can parse the examples provided in the book"
    $ do
        parseString' parsePhone "123-456-7890"
          `shouldBeSuccess` PhoneNumber 123 456 7890
        parseString' parsePhone "1234567890"
          `shouldBeSuccess` PhoneNumber 123 456 7890
        parseString' parsePhone "(123) 456-7890"
          `shouldBeSuccess` PhoneNumber 123 456 7890
        parseString' parsePhone "1-123-456-7890"
          `shouldBeSuccess` PhoneNumber 123 456 7890
  describe
      "Chapter exercise: 5. Write a parser for a log file format, and sum the time spent in each activity"
    $ do
        it "can parse a date"
          $                 parseString' parseDate "2025-02-05"
          `shouldBeSuccess` Date 2025 02 05
        it "can parse a time"
          $                 parseString' parseTime "08:00"
          `shouldBeSuccess` Time 8 0
        it "can parse a record"
          $                 parseString' parseRecord "08:00 Breakfast"
          `shouldBeSuccess` Record (Time 8 0) "Breakfast"
        it "can parse a record with a comment"
          $ parseString' parseRecord "08:00 Breakfast -- comment"
          `shouldBeSuccess` Record (Time 8 0) "Breakfast"
        it "can parse a daily record"
          $                 parseString' parseDailyRecord dailyRecordString
          `shouldBeSuccess` dailyRecord
        it "can parse a daily record with comment"
          $                 parseString' parseDailyRecord dailyRecordString'
          `shouldBeSuccess` dailyRecord'
        it "can parse a log"
          $                 parseString' parseLog logString
          `shouldBeSuccess` [dailyRecord, dailyRecord']
        it "get the average time spent per activity per day"
          $ let exp =
                  [ ( Date 2025 2 5
                    , [ ("Breakfast"                    , 60.0)
                      , ("Commuting home in rover"      , 30.0)
                      , ("Dinner"                       , 120.0)
                      , ("Exercising in high-grav gym"  , 60.0)
                      , ("Lunch"                        , 60.0)
                      , ("Programming"                  , 240.0)
                      , ("R&R"                          , 90.0)
                      , ("Read"                         , 45.0)
                      , ("Sanitizing moisture collector", 120.0)
                      , ("Shower"                       , 15.0)
                      , ("Sleep"                        , 120.0)
                      ]
                    )
                  , ( Date 2025 2 7
                    , [ ("Breakfast"              , 60.0)
                      , ("Bumped head, passed out", 276.0)
                      , ("Commute home for rest"  , 30.0)
                      , ("Dinner"                 , 15.0)
                      , ("Go to medbay"           , 3.0)
                      , ("Patch self up"          , 5.0)
                      , ("Read"                   , 225.0)
                      , ("Sleep"                  , 120.0)
                      , ("Wake up, headache"      , 1.0)
                      ]
                    )
                  ]
                act = averageTimeSpentPerActivityPerDay
                  [dailyRecord, dailyRecord']
            in  act `shouldBe` exp
  describe "Chapter exercise: 6. Write a parser for IPv4 addresses" $ do
    it "can parse the example found in the book" $ do
      parseString' parseIPAddress "172.16.254.1"
        `shouldBeSuccess` IPAddress 2886794753
      parseString' parseIPAddress "204.120.0.15"
        `shouldBeSuccess` IPAddress 3430416399
    it "can parse the example found in Wikipedia"
      $                 parseString' parseIPAddress "192.0.2.235"
      `shouldBeSuccess` IPAddress 3221226219
    it "works" $
      makeIPv4 [192, 0, 2, 235] `shouldBe` IPAddress 3221226219
  describe "Chapter exercise: 7. Same as before, but IPv6"
    $ it "can parse the example found in the book"
    $ do
        parseString' parseIPAddress6 "0:0:0:0:0:ffff:ac10:fe01"
          `shouldBeSuccess` IPAddress6 281473568538113
        parseString' parseIPAddress6 "0:0:0:0:0:ffff:cc78:f"
          `shouldBeSuccess` IPAddress6 281474112159759
        parseString' parseIPAddress6 "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
          `shouldBeSuccess` IPAddress6 338288524927261089654163772891438416681
        parseString' parseIPAddress6 "FE80::0202:B3FF:FE1E:8329"
          `shouldBeSuccess` IPAddress6 338288524927261089654163772891438416681
        parseString' parseIPAddress6 "2001:DB8::8:800:200C:417A"
          `shouldBeSuccess` IPAddress6 42540766411282592856906245548098208122
  describe
      "Chapter exercise: 8. Remove the derived Show instances from the IPAddress/IPAddress6 types, and write your own"
    $ do
        it "can show a IPv4" $ do
          show (IPAddress 2886794753) `shouldBe` "172.16.254.1"
          show (IPAddress 3430416399) `shouldBe` "204.120.0.15"
        it "can show a IPv6" $ do
          show (IPAddress6 281473568538113) `shouldBe` "::ffff:ac10:fe01"
          show (IPAddress6 281474112159759) `shouldBe` "::ffff:cc78:f"
          show (IPAddress6 338288524927261089654163772891438416681)
            `shouldBe` "fe80::202:b3ff:fe1e:8329"
          show (IPAddress6 42540766411282592856906245548098208122)
            `shouldBe` "2001:db8::8:800:200c:417a"




parseString' :: Parser a -> String -> Result a
parseString' p = parseString p mempty

shouldBeSuccess :: (Show a, Eq a) => Result a -> a -> Expectation
shouldBeSuccess (Success a  ) a' = a `shouldBe` a'
shouldBeSuccess (Failure err) _  = assertFailure $ show err

shouldBeFailure :: (Show a, Eq a) => Result a -> Expectation
shouldBeFailure (Failure _) = pure ()
shouldBeFailure s           = assertFailure $ show s <> " was not a Failure"

dailyRecordString :: String
dailyRecordString = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

dailyRecord :: DailyRecord
dailyRecord = DailyRecord
  (Date 2025 02 5)
  [ Record (Time 8 0)   "Breakfast"
  , Record (Time 9 0)   "Sanitizing moisture collector"
  , Record (Time 11 0)  "Exercising in high-grav gym"
  , Record (Time 12 0)  "Lunch"
  , Record (Time 13 0)  "Programming"
  , Record (Time 17 0)  "Commuting home in rover"
  , Record (Time 17 30) "R&R"
  , Record (Time 19 0)  "Dinner"
  , Record (Time 21 0)  "Shower"
  , Record (Time 21 15) "Read"
  , Record (Time 22 0)  "Sleep"
  ]

dailyRecordString' :: String
dailyRecordString' = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

dailyRecord' :: DailyRecord
dailyRecord' = DailyRecord
  (Date 2025 02 7)
  [ Record (Time 8 0)   "Breakfast"
  , Record (Time 9 0)   "Bumped head, passed out"
  , Record (Time 13 36) "Wake up, headache"
  , Record (Time 13 37) "Go to medbay"
  , Record (Time 13 40) "Patch self up"
  , Record (Time 13 45) "Commute home for rest"
  , Record (Time 14 15) "Read"
  , Record (Time 21 0)  "Dinner"
  , Record (Time 21 15) "Read"
  , Record (Time 22 0)  "Sleep"
  ]

logString :: String
logString = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
