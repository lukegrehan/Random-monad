module Random where

import System.Random (StdGen)
import qualified System.Random as R
import Control.Applicative
import Control.Monad

data Rand a = Rand { runRand :: (StdGen -> (a,StdGen)) }

randAsIO :: Rand a -> IO a
randAsIO = R.getStdRandom.runRand

getGen :: Rand StdGen
getGen = Rand $ \g -> (g,g)

useGen :: Rand (a, StdGen) -> Rand a
useGen = (>>= (Rand . const))

instance Functor Rand where
  fmap f a = pure f <*> a

instance Applicative Rand where
  pure = return
  (<*>) = ap

instance Monad Rand where
  return a = Rand $ \g -> (a, g)
  (Rand a) >>= f = Rand $ \g -> 
    let (a', g') = a g
    in runRand (f a') g'

random :: (R.Random a) => Rand a
random = useGen $ R.random <$> getGen

randomR :: (R.Random a) => (a,a) -> Rand a
randomR range = useGen $ R.randomR range <$> getGen

randoms :: (R.Random a) => Rand [a]
randoms = Rand $ \g -> 
  let (og,ng) = R.split g in (R.randoms og, ng)

randomRs :: (R.Random a) => (a,a) -> Rand [a]
randomRs range = Rand $ \g -> 
  let (og, ng) = R.split g in (R.randomRs range og, ng)
