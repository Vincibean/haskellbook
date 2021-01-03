{-# LANGUAGE OverloadedStrings #-}

module Data.Ini
  ( parseAssignment
  , parseHeader
  , parseIni
  , parseIniFile
  , parseSection
  , skipComments
  , Config(Config)
  , Header(Header)
  , Section(Section)
  )
where

import           Types                          ( Assignments
                                                , Value
                                                , Name
                                                , Config(..)
                                                , Section(..)
                                                , Header(..)
                                                , File(filename)
                                                , fullPath
                                                )

import           Control.Applicative            ( Alternative(some, (<|>)) )
import           Data.Char                      ( isAlpha )
import           Data.Map                       ( Map )

import qualified Data.Map                      as M
import           Text.Trifecta                  ( some
                                                , Parser
                                                , foldResult
                                                , letter
                                                , noneOf
                                                , oneOf
                                                , parseString
                                                , CharParsing(char)
                                                , Parsing(skipMany)
                                                , Result
                                                )
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitFailure)
                                                )
import           System.IO                      ( hClose
                                                , openFile
                                                , stderr
                                                , hGetContents
                                                , hPrint
                                                , IOMode(ReadMode)
                                                )

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

parseIniFile :: File -> IO (Map FilePath Config)
parseIniFile file = do
  let path = fullPath file
  handle   <- openFile path ReadMode
  contents <- hGetContents handle
  config   <- asIO $ parseString parseIni mempty contents
  hClose handle
  let name = filename file
  return $ M.singleton name config

asIO :: Result a -> IO a
asIO = foldResult err pure
  where err e = hPrint stderr e >> exitWith (ExitFailure 2)
