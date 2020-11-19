module Ch26.FixTheCode where

import           Control.Monad.Trans.Maybe      ( MaybeT(runMaybeT) )
import           Control.Monad                  ( guard )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)
