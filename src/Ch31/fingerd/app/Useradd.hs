module Useradd where

import           Cli                            ( asNewUser
                                                , asUser
                                                , useradd
                                                , Useradd(userId)
                                                )
import           Database                       ( insertNewUser
                                                , updateUser
                                                )
import           System.Console.CmdArgs         ( cmdArgs )

main :: IO ()
main = do
  usradd <- cmdArgs useradd
  let maybeId = userId usradd
  maybe (insertNewUser $ asNewUser usradd) (updateUser . asUser usradd) maybeId
