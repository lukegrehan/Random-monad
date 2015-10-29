import Random
import Control.Monad
import Data.List
import Control.Monad.Identity

data RPS = Rock | Paper | Scissors deriving (Enum, Show, Read, Eq)

instance Ord RPS where
  compare a b | a == b   = EQ
  compare Rock Scissors  = GT
  compare Paper Rock     = GT
  compare Scissors Paper = GT
  compare _ _            = LT

randomMove :: Rand RPS
randomMove = toEnum <$> (randomR (0,2))

aiMove :: IO RPS
aiMove = runRand randomMove

playerMove :: IO RPS
playerMove = read <$> getLine

gameRound :: IO Ordering
gameRound = do
  ai <- aiMove
  putStr ">"
  player <- playerMove
  putStrLn $ "ai's move -> "++ show ai

  case player `compare` ai of
    EQ -> putStrLn "Draw" >> gameRound
    GT -> putStrLn "Player wins round!" >> return GT
    LT -> putStrLn "Ai wins round!" >> return LT

numRounds = 3

main :: IO ()
main = do
  putStrLn "Input moves as one of: Rock Paper Scissors"
  rounds <- sequence $ replicate numRounds gameRound
  let (plWins, aiWins) = partition (==GT) rounds
  putStrLn $ case (length plWins) `compare` (length aiWins) of
                EQ -> "The game is a draw!"
                GT -> "The player wins!"
                LT -> "The computer wins!"
