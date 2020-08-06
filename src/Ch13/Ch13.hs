module Ch13.Ch13 where

import Control.Monad
import Data.Bifoldable
import Data.Char
import System.IO

--  Check your understanding

-- import qualified Control.Concurrent        as CC
-- import qualified Control.Concurrent.MVar   as MV
-- import           Control.Exception         (mask, try)
-- import           Control.Monad             (forever, when)
-- import           Data.Bits
-- import           Data.Bits.Bitwise         (fromListBE)
-- import qualified Data.ByteString.Char8     as B
-- import           Data.List.Split           (chunksOf)
-- import qualified Data.Locator              as DL
-- import qualified Data.Time.Clock.POSIX     as PSX
-- import           Database.Blacktip.Types
-- import qualified Filesystem                as FS
-- import qualified Filesystem.Path.CurrentOS as FPC
-- import qualified Network.Info              as NI
-- import qualified Safe
-- import           System.IO.Unsafe          (unsafePerformIO)

-- 1. What functions are being imported from Control.Monad?
--    forever, when

-- 2. Which imports are both unqualified and imported in their entirety?
--    Data.Bits, Database.Blacktip.Types

-- 3. From the name, what do you suppose importing Blacktip’s Types module brings in?
--    Type definition, data constructors and type constructors

-- 4. a) The type signature refers to three aliased imports. What modules are named in those aliases?
--       Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent

--    b) Which import does FS.writeFile refer to?
--       Filesystem

--    c) Which import did forever come from?
--       Control.Monad

-- What happens when the two input characters are equal?
-- The string "True" is displayed

-- What happens when they aren’t?
-- Nothing is displayed

-- Modify this block of code to exit successfully after a False result.
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  when (palindrome' line1) $ putStrLn "It's a palindrome!"

palindrome' :: String -> Bool
palindrome' w = sanitized == reverse sanitized
  where
    sanitized = sanitize w

sanitize :: String -> String
sanitize = map toLower . filter isLetter

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $
      PersonInvalidUnknown $
        "Name was: " ++ show name
          ++ " Age was: "
          ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStrLn "Hey there!"
  putStrLn "This is iPerson, where you can create a new person"
  putStrLn "and make it your bestie (or your personal nemesis; who hasn't ever wished for a mortal nemesis?)"
  putStrLn "First, I'll need the name of this person."
  putStrLn "Please, provide a valid name: no one deserves a silly name, not even your nemesis"
  name <- getLine
  putStrLn $ "Cool! '" ++ name ++ "' is an awesome name!"
  putStrLn "Now, I need to know the age of this person."
  putStrLn "Please, provide a reasonable age: I become angry when people use weird numbers (or no number at all), and you don't want to see me angry"
  age <- getLine
  if not $ all isDigit age
    then putStrLn "OK, now I am... ANGRY!!!"
    else do
      let i = read age :: Integer
      let eitherP = mkPerson name i
      let outcome = bifoldMap (\err -> "There was an error: " ++ show err) (\p -> "Yay! Your person is now ready: " ++ show p) eitherP
      putStrLn outcome
      putStrLn "Thank you for choosing GimmePerson TM, the (second) best way to create a new person"
