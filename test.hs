import Arithmetic
import System.Random
import System.Environment

type Rounds = Int

defaultRounds :: Rounds
defaultRounds = 1000

dispatch :: [(String, (Int -> Int -> Bool))]
dispatch = [("add", addTest)]

-- NOTE: Just testing with positive integers for now
runTest :: (RandomGen g) => (Int -> Int -> Bool) -> Rounds -> g -> Bool
runTest t r g = all (==True) results
  where (randX, randY) = splitAt r $ take (r*2) $ randomRs (0, maxBound :: Int) g
        rands = zip randX randY
        results = map (\(x, y) -> t x y) rands

main = do
    (command:additionalCommands) <- getArgs
    let rounds = if null additionalCommands 
                    then defaultRounds 
                    else case reads $ head additionalCommands of
                      [] -> defaultRounds
                      [(r,_)] -> r
    gen <- getStdGen
    case lookup command dispatch of
        Nothing -> putStrLn "Invalid Command"
        (Just test) -> 
            if runTest test rounds gen 
                then putStrLn "Test Passed"
                else putStrLn "Test Failed"

-- Tests
addTest :: Int -> Int -> Bool
addTest x y = applyBitCalc addBits x y == x + y

-- Helpers
decToBits :: Int -> [Bool]
decToBits n = reverse $ map (odd) powers
  where powers = takeWhile (\x -> x > 0) $ map (\x -> n `div` 2^x) [0..]

bitsToDec :: [Bool] -> Int
bitsToDec [] = 0
bitsToDec (x:xs) = if x then 2 ^ (length xs) + bitsToDec xs else bitsToDec xs

applyBitCalc :: ([Bool] -> [Bool] -> [Bool]) -> Int -> Int -> Int
applyBitCalc f x y = bitsToDec (f (decToBits x) (decToBits y))