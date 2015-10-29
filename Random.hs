module Random where

import System.Random (StdGen)
import qualified System.Random as R
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

data RandT m a = RandT { runRandT :: (StdGen -> m (a,StdGen)) }
type Rand a = RandT Identity a

runRand :: Rand a -> IO a
runRand = (return . runIdentity) <=< randAsIO

randAsIO :: Monad m => RandT m a -> IO (m a)
randAsIO i = R.newStdGen >>= return . (fst <$>) . runRandT i

getGen :: Monad m => RandT m StdGen
getGen = RandT $ \g -> return (g,g)

useGen :: Monad m => RandT m (a, StdGen) -> RandT m a
useGen r = RandT $ ((fst <$>) . (runRandT r))

instance Monad m => Functor (RandT m) where
  fmap f a = pure f <*> a

instance Monad m => Applicative (RandT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (RandT m) where
  return a = RandT $ \g -> return (a, g)
  (RandT a) >>= f = RandT $ \g -> do
    (a', g') <- a g
    runRandT (f a') g

instance MonadTrans RandT where
  lift a = RandT $ \g -> do
    a' <- a
    return (a', g)

instance MonadIO m => MonadIO (RandT m) where
  liftIO = lift.liftIO

random :: (Monad m, R.Random a) => RandT m a
random = useGen $ R.random <$> getGen

randomR :: (Monad m, R.Random a) => (a,a) -> RandT m a
randomR range = useGen $ R.randomR range <$> getGen

randoms :: (Monad m, R.Random a) => RandT m [a]
randoms = RandT $ \g ->
  let (og,ng) = R.split g in return (R.randoms og, ng)

randomRs :: (Monad m, R.Random a) => (a,a) -> RandT m [a]
randomRs range = RandT $ \g ->
  let (og, ng) = R.split g in return (R.randomRs range og, ng)
