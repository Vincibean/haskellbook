module Ch30.Ch30 where

import Control.Exception ( try, throwIO, Exception )

canICatch :: Exception e => e -> IO (Either e ())
canICatch e = try $ throwIO e

tryS :: Exception e => IO () -> IO (Either e ())
tryS = try