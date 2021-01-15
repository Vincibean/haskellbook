{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Useradd where

import Cli
import           Database
import           System.Console.CmdArgs
import           Domain                         ( NewUser(NewUser)
                                                , User(User)
                                                )


main :: IO ()
main = do
  usradd <- cmdArgs useradd
  let maybeId = userId usradd
  maybe (insertNewUser $ asNewUser usradd) (updateUser . asUser usradd) maybeId
