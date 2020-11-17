{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Ch26.Ch26 where

import           Control.Applicative            ( Applicative(liftA2) )
import Data.Functor.Classes

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Data.Functor.Identity


newtype EitherT smas m a = EitherT { runEitherT :: m (Either smas a) }

instance (Eq smas, Eq1 m) => Eq1 (EitherT smas m) where
    liftEq eq (EitherT mas) (EitherT y) = liftEq (liftEq eq) mas y

instance (Eq1 m, Eq smas, Eq a) => Eq (EitherT smas m a)
  where (==) = eq1

instance (Show smas, Show1 m) => Show1 (EitherT smas m) where
    liftShowsPrec sp sl d (EitherT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "EitherT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Show smas, Show1 m, Show a) => Show (EitherT smas m a) where
    showsPrec = showsPrec1

-- EitherT
-- 1. Write the Functor instance for EitherT
instance Functor m => Functor (EitherT smas m) where
    fmap f (EitherT smas) = EitherT $ (fmap . fmap) f smas

-- 2. Write the Applicative instance for EitherT
instance Applicative m => Applicative (EitherT smas m) where
  pure = EitherT . pure . pure

  (EitherT fgab) <*> (EitherT fga) = EitherT $ liftA2 (<*>) fgab fga

-- 3. Write the Monad instance for EitherT
instance Monad m => Monad (EitherT smas m) where
  return = pure

  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Right a -> runEitherT $ f a
      Left smas -> return $ Left smas

-- 4. Write the swapEitherT helper function for EitherT
swapEitherT :: (Functor m) => EitherT smas m a -> EitherT a m smas
swapEitherT (EitherT mea)= EitherT $ swapEither <$> mea

swapEither :: Either smas a -> Either a smas
swapEither (Left smas) = Right smas
swapEither (Right a) = Left a

-- 5. Write the transformer variant of the either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT lf rf (EitherT mea) = mea >>= either lf rf

-- TODO try to write the instances for ReaderT

--  StateT

newtype StateT' s m a = StateT' { runStateT' :: s -> m (a,s) }

-- 1. Write the Functor instance for StateT
instance (Functor m) => Functor (StateT' s m) where
  fmap f (StateT' smas) = StateT' $ \s -> let mas = smas s in (\(a, s') -> (f a, s')) <$> mas

-- 1. Write the Applicative instance for StateT
instance (Monad m) => Applicative (StateT' s m) where
  pure a = StateT' $ \s -> pure (a, s)
  
  (<*>) :: StateT' s m (a -> b) -> StateT' s m a -> StateT' s m b
  (StateT' smab) <*> (StateT' sma) = StateT' $ \s -> do
    (ab, s') <- smab s 
    (a, s'') <- sma s'
    return (ab a, s'')

-- 3. Write the Monad instance for StateT
instance (Monad m) => Monad (StateT' s m) where
  return = pure
  
  (StateT' smas) >>= f = StateT' $ \s -> do
    (a, s') <- smas s
    let smbs = runStateT' (f a)
    smbs s'

-- Wrap it up
-- Turn readerUnwrap from the previous example back into embedded through the use of the data constructors for each transformer
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ const $ pure (Right (Just 1))

-- Lift more

-- 1. EitherT
instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

-- 2. StateT
instance MonadTrans (StateT' s) where
  lift x = StateT' $ \s -> fmap (,s) x

-- Some instances
-- 1. MaybeT
newtype MaybeT' m a = MaybeT' { runMaybeT' :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT' m) where
  fmap f (MaybeT' ma) = MaybeT' $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT' m) where
  pure x = MaybeT' (pure (pure x))
  
  (MaybeT' fab) <*> (MaybeT' mma) = MaybeT' $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT' m) where
  return = pure

  (MaybeT' ma) >>= f = MaybeT' $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT' (f y)

instance (MonadIO m) => MonadIO (MaybeT' m) where
  liftIO = MaybeT' . fmap Just . liftIO

-- 2. ReaderT
newtype ReaderT' r m a = ReaderT' { runReaderT' :: r -> m a }

instance (Functor m) => Functor (ReaderT' r m) where
  fmap f (ReaderT' rma) = ReaderT' $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT' r m) where
  pure a = ReaderT' (pure (pure a))
  
  (ReaderT' fmab) <*> (ReaderT' rma) = ReaderT' $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT' r m) where
  return = pure
  
  (ReaderT' rma) >>= f = ReaderT' $ \r -> do
    a <- rma r
    runReaderT' (f a) r

instance (MonadIO m) => MonadIO (ReaderT' r m) where
  liftIO = ReaderT' . const . liftIO

-- 3. StateT
instance (MonadIO m) => MonadIO (StateT' s m) where
  liftIO ioa = StateT' $ \s -> fmap (,s) (liftIO ioa)

-- Chapter Exercises
-- 1. 2. rDec is a function that should get its argument in the context of Reader and return a value decremented by one:
rDec :: Num a => Reader a a
rDec = reader $ subtract 1

-- 3. 4. rShow is show, but in Reader
rShow :: Show a => ReaderT a Identity String
rShow = reader show

-- 5. rPrintAndInc will first print the input with a greeting, then return the input incremented by one
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ (>>) <$> greet <*> inc
  where greet = putStrLn . ("Hi: " <>) . show
        inc = pure . (1 +)


rPrintAndInc' :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc' = ReaderT $ \r -> do
                            putStrLn $ "Hi: " ++ show r
                            return $ r + 1

-- 6. sPrintIncAccum first prints the input with a greeting, then "puts" the incremented input as the new state 
--    and returns the original input as a String
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =  do
  r <- get 
  let greetings = "Hi: " ++ show r
  lift $ putStrLn greetings
  modify (1+)
  let res = show r
  lift $ pure res

sPrintIncAccum' :: (Num a, Show a) => StateT a IO String
sPrintIncAccum' = StateT $ \s -> do
                            putStrLn $ "Hi: " ++ show s
                            return (show s, s + 1)