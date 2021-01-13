{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Useradd where

import qualified Data.Text                     as T
import           Database
import           System.Console.CmdArgs
import           Types                          ( NewUser(..)
                                                , User(..)
                                                )

data Useradd = Useradd
  { user_id             :: Maybe Integer
  , user_name           :: String
  , user_shell          :: String
  , user_home_directory :: String
  , user_real_name      :: String
  , user_phone          :: String
  }
  deriving (Eq, Show, Data, Typeable)

useradd :: Useradd
useradd =
  Useradd
      { user_id             = def &= opt (Nothing :: Maybe Integer) &= help
                                "The id of the user that should be updated, if any"
      , user_name           = def &= help "The username of the user"
      , user_shell          = def &= help "The shell of the user"
      , user_home_directory = def &= help "The home directory of the user"
      , user_real_name      = def &= help "The real name of the user"
      , user_phone          = def &= help "The phone number of the user"
      }
    &= verbosity
    &= help "Add new users to the finger database"
    &= summary "useradd v0.0.0.1, (C) Vincibean"

asNewUser :: Useradd -> NewUser
asNewUser (Useradd _ usrname shll homeDir realNam phon) = NewUser
  (T.pack usrname)
  (T.pack shll)
  (T.pack homeDir)
  (T.pack realNam)
  (T.pack phon)

asUser :: Useradd -> Integer -> User
asUser (Useradd _ usrname shll homeDir realNam phon) usrid = User
  usrid
  (T.pack usrname)
  (T.pack shll)
  (T.pack homeDir)
  (T.pack realNam)
  (T.pack phon)

main :: IO ()
main = do
  usradd <- cmdArgs useradd
  let maybeId = user_id useradd
  maybe (insertNewUser $ asNewUser usradd) (updateUser . asUser usradd) maybeId
