module Ch26.Morra where

import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State.Lazy ( get
                                                , modify
                                                , StateT(runStateT)
                                                )
import           Data.Word                      ( Word8 )
import           System.Exit                    ( exitSuccess )
import           System.Random                  ( Random(randomRIO) )

-- Morra
-- 1. Write the game Morra using StateT and IO

data Game = Game { player1 :: Word8, player2 :: Word8, history :: [Word8] } deriving (Show, Eq)

instance Semigroup Game where
  (Game p1 p2 h) <> (Game p1' p2' h') =
    Game (p1 + p1') (p2 + p2') (take 3 (h <> h'))

instance Monoid Game where
  mempty = Game 0 0 []

type Morra = StateT Game IO

gameStatusCheck :: Game -> IO ()
gameStatusCheck (Game p1 p2 _)
  | p1 >= 3   = putStrLn "Player 1 wins" >> exitSuccess
  | p2 >= 3   = putStrLn "Player 2 wins" >> exitSuccess
  | otherwise = return ()

play :: StateT Game IO ()
play = forever $ do
  lift $ putStr "P: "
  playerMove <- lift (read <$> getLine :: IO Word8)
  pcMove     <- lift $ randomRIO (1, 2)
  lift $ putStrLn $ "C: " <> show pcMove
  if odd $ playerMove + pcMove
    then lift (putStrLn "- P wins ")
      >> modify (\g -> g { player1 = player1 g + 1 })
    else lift (putStrLn "- C wins ")
      >> modify (\g -> g { player2 = player2 g + 1 })
  modify (\g -> g { history = take 3 (playerMove : history g) })
  s' <- get
  lift $ gameStatusCheck s'

play' :: StateT Game IO ()
play' = forever $ do
  lift $ putStr "P1: "
  player1Move <- lift (read <$> getLine :: IO Word8)
  lift $ putStr "P2: "
  player2Move <- lift (read <$> getLine :: IO Word8)
  if odd $ player1Move + player2Move
    then lift (putStrLn "- P1 wins ")
      >> modify (\g -> g { player1 = player1 g + 1 })
    else lift (putStrLn "- p2 wins ")
      >> modify (\g -> g { player2 = player2 g + 1 })
  s' <- get
  lift $ gameStatusCheck s'

playMorraAgainstPC :: IO ()
playMorraAgainstPC = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, Computer is evens"
  runStateT play mempty
  return ()

playMorraAgainstHuman :: IO ()
playMorraAgainstHuman = do
  putStrLn "-- P1 is Player 1"
  putStrLn "-- P2 is Player 2"
  putStrLn "-- Player 1 is odds, Player 2 is evens"
  runStateT play' mempty
  return ()

