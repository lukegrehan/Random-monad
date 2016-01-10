module Random where

import System.Random (StdGen)
import qualified System.Random as R
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

data RandT m a = RandT { runRandT :: (StdGen -> m (a,StdGen)) }
type Rand = RandT Identity

runRand :: Rand a -> IO a
runRand = (return . runIdentity) <=< randAsIO

randAsIO :: Monad m => RandT m a -> IO (m a)
randAsIO i = R.newStdGen >>= return . (fst <$>) . runRandT i

withGen :: Monad m => (StdGen -> (a,StdGen)) -> RandT m a
withGen f = RandT (return . f)

instance Monad m => Functor (RandT m) where
  fmap f a = pure f <*> a

instance Monad m => Applicative (RandT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (RandT m) where
  return a = RandT $ \g -> return (a, g)
  (RandT a) >>= f = RandT $ \g -> do
    (a', g') <- a g
    runRandT (f a') g'

instance MonadTrans RandT where
  lift a = RandT $ \g -> do
    a' <- a
    return (a', g)

instance MonadIO m => MonadIO (RandT m) where
  liftIO = lift.liftIO

random :: (Monad m, R.Random a) => RandT m a
random = withGen R.random

randomR :: (Monad m, R.Random a) => (a,a) -> RandT m a
randomR range = withGen $ R.randomR range

randoms :: (Monad m, R.Random a) => RandT m [a]
randoms = RandT $ \g ->
  let (og,ng) = R.split g in return (R.randoms og, ng)

randomRs :: (Monad m, R.Random a) => (a,a) -> RandT m [a]
randomRs range = RandT $ \g ->
  let (og, ng) = R.split g in return (R.randomRs range og, ng)

-----

-- 'pluck' a random item from a list
pluck :: Monad m => [a] -> RandT m (Maybe a, [a])
pluck [] = return (Nothing, [])
pluck as = do
  n <- randomR (0, (length as)-1)
  return (Just (as !! n), (take n as) ++ (drop (n+1) as))

-- shuffle a list
shuffle :: Monad m => [a] -> RandT m [a]
shuffle [] = return []
shuffle as = do
  (Just a, rest) <- pluck as
  shufR <- shuffle rest
  return (a:shufR)
