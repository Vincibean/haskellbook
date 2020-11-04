module Ch24.Ch24 where

import           Text.Trifecta                  ( many
                                                , some
                                                , alphaNum
                                                , digit
                                                , newline
                                                , noneOf
                                                , notFollowedBy
                                                , oneOf
                                                , spaces
                                                , count
                                                , sepBy
                                                , skipOptional
                                                , colon
                                                , decimal
                                                , dot
                                                , integer
                                                , symbolic
                                                , whiteSpace
                                                , CharParsing
                                                  ( anyChar
                                                  , char
                                                  , string
                                                  )
                                                , Parsing
                                                  ( try
                                                  , unexpected
                                                  , eof
                                                  , skipMany
                                                  )
                                                , TokenParsing
                                                , Parser
                                                )

import           Control.Applicative            ( Applicative(liftA2)
                                                , Alternative((<|>))
                                                )
import           Control.Monad                  ( void )
import           Control.Arrow                  ( (&&&) )

import           Data.Aeson                     ( decode
                                                , FromJSON(parseJSON)
                                                , Value(String, Number)
                                                )
import Data.Bits
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Char                      ( ord
                                                , intToDigit
                                                )
import           Data.Foldable                  ( Foldable(fold) )
import           Data.Function                  ( on )
import           Data.List                      ( elemIndex
                                                , groupBy
                                                , intercalate
                                                , sortBy
                                                )
import           Data.Ratio                     ( (%) )
import           Data.Scientific                ( floatingOrInteger )
import           Data.Text                      ( Text )
import           Data.Word                      ( Word8, Word32 )
import           Numeric                        ( readHex )

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1'

-- read a single character '1', then die
one' :: Parser b
one' = one >> stop

-- read two characters, '1' and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

-- Exercises: Parsing practice

-- 1. See if you can make the `one` and `oneTwo` parsers fail, because they don’t exhaust the input stream.

one'' :: Parser Char
one'' = one <* eof

oneTwo'' :: Parser Char
oneTwo'' = oneTwo <* eof

-- 2. Use `string` to make a Parser that parses “1”, “12”, and “123” out of the example input, respectively. Try combining it with stop, too.

-- N.B.: order matters!
p123 :: Parser String
p123 = string "123" <|> string "12" <|> string "1"

-- 3. Try writing a Parser that does what string does, but using `char`.

string' :: CharParsing m => String -> m String
string' = traverse char

-- Unit of success

integerEof :: TokenParsing m => m Integer
integerEof = integer <* eof

-- Try try
-- Make a parser, using the existing fraction parser plus a new decimal parser, that can parse either decimals or fractions.

data IntegerOrRational = Integer' Integer | Rational' Rational deriving (Eq, Show)

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

integerOrRational :: Parser IntegerOrRational
integerOrRational = Rational' <$> try fraction <|> Integer' <$> decimal

-- see how you need to change the type of dec to be able to parse a list of numbers or strings.

data NumberOrString = Numba Integer | Stringy Text deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) = case floatingOrInteger i of
    (Left  _      ) -> fail "Must be integral number"
    (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _          = fail "NumberOrString must be number or string"

dec :: ByteString -> Maybe NumberOrString
dec = decode

dec' :: ByteString -> Maybe [NumberOrString]
dec' = decode

-- Chapter exercises
-- 1. Write a parser for semantic versions
--    After making a working parser, write an Ord instance for the SemVer type that obeys the specification outlined 
--    on the SemVer website:

data NumberOrString' = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString']
type Metadata = [NumberOrString']
data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord NumberOrString' where
  NOSS s <= NOSS s' = s <= s'
  NOSI i <= NOSI i' = i <= i'
  NOSI _ <= NOSS _  = False
  NOSS _ <= NOSI _  = True

instance Ord SemVer where
  (SemVer major minor patch _ _) <= (SemVer major' minor' patch' _ _) =
    [major, minor, patch] <= [major', minor', patch']

hyphen :: TokenParsing m => m Char
hyphen = symbolic '-'

validChar :: Parser Char
validChar = alphaNum <|> hyphen

parseNumberOrString :: Parser NumberOrString'
parseNumberOrString =
  NOSI <$> try (decimal <* notFollowedBy validChar) <|> NOSS <$> some (alphaNum <|> hyphen)

parseRelease :: Parser [NumberOrString']
parseRelease = skipOptional hyphen >> parseNumberOrString `sepBy` dot

parseMetadata :: Parser [NumberOrString']
parseMetadata = skipOptional (char '+') >> parseNumberOrString `sepBy` dot

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  dot
  minor <- decimal
  dot
  patch   <- decimal
  release <- parseRelease
  SemVer major minor patch release <$> parseMetadata

-- 2. Write a parser for positive integer values. Don’t reuse the preexisting digit or integer functions, but you can use the rest of
--    the libraries we’ve shown you so far. You are not expected to write a parsing library from scratch:

parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  let digits' = fmap (subtract 48 . toInteger . ord) digits
  let is = fmap (\(el, base) -> el * 10 ^ base) (reverse digits' `zip` [0 ..])
  return $ sum is

base10Integer' :: Parser Integer
base10Integer' = read <$> some parseDigit

-- 3. Extend the parser you wrote to handle negative and positive integers. 

negInteger :: Parser Integer
negInteger = negate <$> (hyphen >> base10Integer)

posInteger :: Parser Integer
posInteger = skipOptional (char '+') >> base10Integer

negAndPosInteger :: Parser Integer
negAndPosInteger = negInteger <|> posInteger


-- 4. Write a parser for US/Canada phone numbers with varying formats

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parseNumberingPlanArea :: Parser Int
parseNumberingPlanArea = read <$> count 3 digit

parseExchange :: Parser Int
parseExchange = read <$> count 3 digit

parseLineNumber :: Parser Int
parseLineNumber = read <$> count 4 digit

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipOptional (symbolic '(')
  skipOptional (string "1-")
  numberingPlanArea <- parseNumberingPlanArea
  skipOptional (symbolic ')')
  skipOptional whiteSpace
  skipOptional hyphen
  exchange <- parseExchange
  skipOptional hyphen
  lineNumber <- parseLineNumber
  let phoneNumber = PhoneNumber numberingPlanArea exchange lineNumber
  return phoneNumber

-- 5. Write a parser for a log file format, and sum the time spent in each activity. 
--    Additionally, provide an alternative aggregation of the data that provides average time spent per activity per day.

type Day = Int
type Month = Int
type Year = Int
data Date = Date Year Month Day deriving (Eq, Show, Ord)

type Hour = Int
type Minutes = Int
data Time = Time { hour :: Hour, minutes :: Minutes } deriving (Eq, Show)

instance Ord Time where
  (Time h1 m1) <= (Time h2 m2) = h1 * 60 + m1 <= h2 * 60 + m2

type Activity = String

data Record = Record Time Activity deriving (Eq, Show)

instance Ord Record where
  (Record t1 _) <= (Record t2 _) = t1 <= t2

data FullRecord = FullRecord { startTime :: Time , endTime :: Time , activity :: Activity} deriving (Eq, Show)

instance Ord FullRecord where
  (FullRecord t1 _ _) <= (FullRecord t2 _ _) = t1 <= t2

data DailyRecord = DailyRecord Date [Record] deriving (Eq, Show)

type Log = [DailyRecord]

parseDay :: Parser Day
parseDay = do
  ds <- count 2 digit
  let d = read ds :: Day
  if d > 31 || d <= 0 then fail "Day isn't in the interval 1 - 31" else return d

parseMonth :: Parser Month
parseMonth = do
  ds <- count 2 digit
  let d = read ds :: Month
  if d > 12 || d <= 0
    then fail "Month isn't in the interval 1 - 12"
    else return d

parseYear :: Parser Year
parseYear = read <$> count 4 digit

parseDate :: Parser Date
parseDate = do
  year <- parseYear
  hyphen
  month <- parseMonth
  hyphen
  day <- parseDay
  skipMany comment
  let date = Date year month day
  return date

parseHour :: Parser Hour
parseHour = do
  ds <- count 2 digit
  let h = read ds :: Hour
  if h > 23 || h < 0 then fail "Hour isn't in the interval 0 - 23" else return h

parseMinutes :: Parser Minutes
parseMinutes = do
  ds <- count 2 digit
  let h = read ds :: Minutes
  if h > 59 || h < 0
    then fail "Minute isn't in the interval 0 - 59"
    else return h

parseTime :: Parser Time
parseTime = Time <$> parseHour <*> (symbolic ':' >> parseMinutes)

comment :: Parser String
comment = try (spaces >> string "--" >> many (noneOf "\n"))

eol :: Parser ()
eol = void newline <|> void comment

parseActivity :: Parser Activity
parseActivity =
  (eof >> return mempty)
    <|> (eol >> return mempty)
    <|> liftA2 (:) anyChar parseActivity

parseRecord :: Parser Record
parseRecord = do
  time <- parseTime
  whiteSpace
  activity <- parseActivity
  skipMany eol
  let record = Record time activity
  return record

nl :: Parser Char
nl = oneOf "\n"

parseDailyRecord :: Parser DailyRecord
parseDailyRecord = do
  skipMany eol
  skipMany nl
  symbolic '#'
  whiteSpace
  date <- parseDate
  skipMany nl
  records <- some parseRecord
  let dailyRecord = DailyRecord date records
  return dailyRecord

parseLog :: Parser Log
parseLog = many parseDailyRecord


-- https://stackoverflow.com/a/15412231
myGroupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
myGroupBy f =
  map (f . head &&& id) . groupBy ((==) `on` f) . sortBy (compare `on` f)

fmapValues :: Functor f => (b -> c) -> f (a, b) -> f (a, c)
fmapValues f = fmap (\(a, b) -> (a, f b))

avgTimeSpent :: Num b => [Time] -> [b]
avgTimeSpent ts = uncurry timeDiff <$> times where times = ts `zip` tail ts

timeDiff :: Num a => Time -> Time -> a
timeDiff t1 t2 = inMinutes t1 - inMinutes t2

inMinutes :: Num b => Time -> b
inMinutes t = fromIntegral $ hour t * 60 + minutes t

toFullRecord :: [Record] -> [FullRecord]
toFullRecord rs =
  (\(Record time1 activity, Record time2 _) -> FullRecord time1 time2 activity)
    <$> rss
 where
  withMidnight = rs ++ [Record (Time 24 0) "end of day"]
  rss          = withMidnight `zip` tail withMidnight


fullRecordsPerDate :: Log -> [(Date, [(Activity, [FullRecord])])]
fullRecordsPerDate l = recordsPerDate
 where
  dailyRecordsPerDate = myGroupBy (\(DailyRecord date _) -> date) l
  f dailyRecords =
    dailyRecords
      >>= (\(DailyRecord _ records) -> myGroupBy
            (\(FullRecord _ _ activity) -> activity)
            (toFullRecord records)
          )
  recordsPerDate = fmapValues f dailyRecordsPerDate

-- TODO It doesn't cover the last activity (sleep)
averageTimeSpentPerActivityPerDay
  :: Fractional c => [DailyRecord] -> [(Date, [(Activity, c)])]
averageTimeSpentPerActivityPerDay l = avgTSpent
 where
  srtdRecs     = fullRecordsPerDate l
  minutesSpent = (fmapValues . fmapValues)
    (fmap (\r -> timeDiff (endTime r) (startTime r)))
    srtdRecs
  avg xs = fromIntegral (sum xs) / fromIntegral (length xs)
  avgTSpent = (fmapValues . fmapValues) avg minutesSpent

-- 6. Write a parser for IPv4 addresses

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord)

octets :: Parser [Word8]
octets = try $ (fmap . fmap) read some digit `sepBy` dot

parseIPAddress :: Parser IPAddress
parseIPAddress = makeIPv4 <$> octets


-- https://stackoverflow.com/a/9166342
toBin :: Integral a => a -> [a]
toBin 0 = [0]
toBin n = reverse (helper n)

helper :: Integral a => a -> [a]
helper 0 = []
helper n = let (q, r) = n `divMod` 2 in r : helper q


-- https://stackoverflow.com/a/48438340
binToDec :: Integral p => p -> p
binToDec 0 = 0
binToDec i = 2 * binToDec (div i 10) + mod i 10


-- https://stackoverflow.com/a/63221002
pad :: Int -> String -> String
pad n x = take (n - length x) (cycle "0") ++ x

-- octets :: Parser [Integer]
-- octets = try decimal `sepBy` dot

-- parseIPAddress :: Parser IPAddress
-- parseIPAddress = do
--   octs <- octets
--   let padBin = pad 8 . foldMap show . toBin <$> octs
--   let word32 = fromInteger . binToDec . read . fold . take 4 $ padBin
--   return $ IPAddress word32

makeIPv4 :: Integral a => [a] -> IPAddress
makeIPv4 = IPAddress . allocBitBlocks 8 . take 4

allocBitBlocks :: (Foldable t, Integral a1, Bits a2, Num a2) => Int -> t a1 -> a2
allocBitBlocks b = foldl (\acc d -> fromIntegral d + shiftL acc b) 0

-- 7. Same as before, but IPv6
newtype IPAddress6 = IPAddress6 Integer deriving (Eq, Ord)

sixteentets :: Parser [String]
sixteentets = try (many b16) `sepBy` colon
  where b16 = oneOf (['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F'])

-- https://github.com/taruti/haskell-hex/blob/master/Data/Hex.hs#L32-L35
hexToDec :: String -> Integer
hexToDec = fst . head . readHex

substEmpty :: Int -> [a] -> [[a]] -> [[a]]
substEmpty count def = foldr subst []
 where
  subst [] acc = replicate count def ++ acc
  subst el acc = el : acc

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  sixteens <- sixteentets
  let count   = 8 - length sixteens + 1
  let amended = substEmpty count "0" sixteens
  let padBin  = pad 4 <$> amended
  let i       = fromInteger . hexToDec . fold . take 8 $ padBin
  return $ IPAddress6 i

-- 8. Remove the derived Show instances from the IPAddress/IPAddress6 types, and write your own
instance Show IPAddress where
  show (IPAddress w32) = res
   where
    asBin       = toBin w32
    asBinString = foldMap show asBin
    padded      = pad 32 asBinString
    chunked     = chunksOf 8 padded
    asDec       = binToDec . read <$> chunked
    res         = intercalate "." . fmap show $ asDec

instance Show IPAddress6 where
  show (IPAddress6 w64) = res
   where
    asHex   = toHex w64
    padded  = pad 32 asHex
    chunked = chunksOf 4 padded
    noLeadingZeroes =
      normalize
        . omitConsecutiveSectionsOfZeroes
        . fmap removeLeadingZeroes
        $ chunked
    res = intercalate ":" noLeadingZeroes

-- https://hackage.haskell.org/package/split-0.2.3.4/docs/src/Data.List.Split.Internals.html#chunksOf
chunksOf :: Int -> [a] -> [[a]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l  c n = l `c` splitter (drop i l) c n
  build g = g (:) []

-- https://stackoverflow.com/a/26333467
toHex :: Integer -> String
toHex = reverse . recurse
 where
  recurse n
    | n < 16
    = [intToDigit . fromInteger $ n]
    | otherwise
    = let (q, r) = n `divMod` 16 in (intToDigit . fromInteger) r : recurse q

removeLeadingZeroes :: String -> String
removeLeadingZeroes "0"          = "0"
removeLeadingZeroes ('0' : rest) = removeLeadingZeroes rest
removeLeadingZeroes s            = s

omitConsecutiveSectionsOfZeroes :: [String] -> [String]
omitConsecutiveSectionsOfZeroes ("0" : rest@("0" : _)) =
  "" : omitConsecutiveSectionsOfZeroes rest
omitConsecutiveSectionsOfZeroes ("0" : rest) = "" : rest
omitConsecutiveSectionsOfZeroes (s : ss) =
  s : omitConsecutiveSectionsOfZeroes ss

normalize :: [String] -> [String]
normalize s = case elemIndex ("", "") tails of
  Nothing  -> s
  (Just x) -> let (h, t) = splitAt x s in h ++ spaces x ++ dropWhile null t
 where
  tails = s `zip` tail s
  spaces x = if x == 0 then ["", ""] else [""]

-- showHex

-- splitAtEvery :: (Num a, Bits a, Show a) => Int -> a -> [a]
-- splitAtEvery n x =
--   let ys = (unfoldr (f n) x) --  [1,254,16,127] for "127.16.254.1"
--   in ys ++ replicate (4 - length ys) 0  -- padding, cos of reverse order, no unfoldl --;
--   where f :: (Num a, Bits a) => Int -> a -> Maybe (a, a)
--         f _ 0 = Nothing
--         f n x = Just ((2^n - 1) .&. x, shiftR x n)

-- intercalatel :: [a] -> [[a]] -> [a]
-- intercalatel sep = intercalate sep . reverse

-- instance Show IPAddress where
--   show (IPAddress ip) = intercalatel "." $ (printf "%d") <$> splitAtEvery 8 ip

-- instance Show IPAddress6 where
--   show (IPAddress6 l r) =
--     let nums = (splitAtEvery 16 r) ++ (splitAtEvery 16 l)
--     in intercalatel ":" $ (printf "%0X") <$> nums
    

-- 9. Write a function that converts between your types for IPAddress and IPAddress6
convert :: IPAddress -> IPAddress6
convert (IPAddress w32) = IPAddress6 $ toInteger w32
