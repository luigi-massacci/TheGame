import System.Random

-- Function to flip a coin and return True for heads and False for tails
coinFlip :: IO Bool
coinFlip = do
    randomValue <- randomIO :: IO Bool
    return randomValue

use :: Int -> Int -> IO (Int)
use a b = do
    coin <- coinFlip
    return (if coin then a else b)