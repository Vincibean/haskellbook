{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Types                          ( Extension
                                                , File(File, extension)
                                                )
import           Control.Monad                  ( unless )
import           Data.Ini                       ( parseIniFile )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           System.Console.CmdArgs         ( Data
                                                , Typeable
                                                , (&=)
                                                , cmdArgs
                                                , help
                                                , opt
                                                , summary
                                                , typFile
                                                , verbosity
                                                , Default(def)
                                                )
import           System.Directory               ( doesPathExist
                                                , listDirectory
                                                )
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitFailure)
                                                )
import           System.FilePath                ( takeExtension )
import           System.IO                      ( hClose
                                                , openFile
                                                , stderr
                                                , hGetContents
                                                , hPutStrLn
                                                , hPrint
                                                , IOMode(ReadMode)
                                                )

import           Text.Trifecta                  ( parseString
                                                , foldResult
                                                , Result
                                                )



-- Reusing the INI parser that you wrote as an exercise in Chapter 24,
-- on parser combinators, parse a directory of INI config files into a Map,
-- the key of which is the filename and the value of which is the result
-- of parsing the INI file. Only parse files in the directory that have the
-- file extension .ini.

-- https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:listDirectory

data Ini = Ini { directory :: FilePath } deriving (Eq, Show, Data, Typeable)

ini :: Ini
ini =
  Ini { directory = def &= opt "." &= typFile &= help "The directory to parse" }
    &= verbosity
    &= help "Parse a directory of INI config files into a Map"
    &= summary "ini v0.0.0.1, (C) Vincibean"

main :: IO ()
main = do
  (Ini dir)       <- cmdArgs ini
  directoryExists <- doesPathExist dir
  unless directoryExists
    $  hPutStrLn stderr "The given directory does not exist"
    >> exitWith (ExitFailure 2)
  filenames <- listDirectory dir
  let files    = asFileInDir dir <$> filenames
  let iniFiles = filter (byExtension ".ini") files
  maps <- traverse parseIniFile iniFiles
  print maps

asFileInDir :: String -> FilePath -> File
asFileInDir path filename = File path filename $ takeExtension filename

byExtension :: Extension -> File -> Bool
byExtension ext file = extension file == ext
