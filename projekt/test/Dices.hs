import System.Random
import Control.Monad

rollDice :: IO Int
rollDice = randomRIO (1, 6)

rollNDice :: Int -> IO [Int]
rollNDice n = sequence (replicate n rollDice)

