import System.Random (randomRIO)
import Control.Monad (replicateM)
coinToss :: IO String
coinToss = do
    result <- randomRIO (0, 1) :: IO Int
    return $ if result == 0 then "Heads" else "Tails"

-- Simulates multiple coin tosses and calculates the probabilities of heads and tails
simulateTosses :: Int -> IO ()
simulateTosses n = do
    results <- replicateM n coinToss
    let headsCount = length $ filter (== "Heads") results
        tailsCount = length $ filter (== "Tails") results
        totalTosses = fromIntegral n :: Double
        headsProb = fromIntegral headsCount / totalTosses
        tailsProb = fromIntegral tailsCount / totalTosses
    putStrLn $ "Heads: " ++ show headsCount ++ " (" ++ show (headsProb * 100) ++ "%)"
    putStrLn $ "Tails: " ++ show tailsCount ++ " (" ++ show (tailsProb * 100) ++ "%)"

main :: IO ()
main = do
    putStrLn "Enter the number of coin tosses:"
    n <- readLn
    simulateTosses n
