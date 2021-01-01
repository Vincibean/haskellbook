{-# LANGUAGE OverloadedStrings #-}

module Data.Ini
  ( parseAssignment
  , parseHeader
  , parseIni
  , parseSection
  , skipComments
  , Config(Config)
  , Header(Header)
  , Section(Section)
  )
where

import           Control.Applicative            ( Alternative(some, (<|>)) )
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( isAlpha )
import           Data.Map                       ( Map )

import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Text.Trifecta                  ( some
                                                , Parser
                                                , letter
                                                , noneOf
                                                , oneOf
                                                , CharParsing(char)
                                                , Parsing(skipMany)
                                                )

newtype Header = Header String deriving (Eq, Ord, Show)

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _    <- char '='
  val  <- some (noneOf "\n")
  skipEOL -- important!
  return (name, val)

-- | Skip end of line and
-- whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- | Skip comments starting at the
-- beginning of the line.
skipComments :: Parser ()
skipComments = skipMany
  (do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL
  )

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)
