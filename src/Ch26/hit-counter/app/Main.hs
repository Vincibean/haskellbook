{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = 
    Config {
      -- that's one, one click!
      -- two... two clicks!
      -- Three BEAUTIFUL clicks! ah ah ahhhh
      counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (m', i')
  where m' = M.insert k i' m
        i' = i + 1
        i = M.findWithDefault 0 k m

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key"
        withPrefix <- lift $ asks prefix
        let key' = mappend withPrefix unprefixed
        counter <- lift $ asks counts
        newInteger <- liftIO $ atomicModifyIORef' counter $ bumpBoomp key'
        html $ mconcat [ "<h1>Success! Count was: " , TL.pack $ show newInteger , "</h1>" ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter $ TL.pack prefixArg 
        runR :: ReaderT Config IO a -> IO a
        runR r = runReaderT r config
    scottyT 3000 runR app