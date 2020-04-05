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
    (command:_) <- getArgs
    gen <- getStdGen
    case lookup command dispatch of
        Nothing -> putStrLn "Invalid Command"
        (Just test) -> 
            if runTest test defaultRounds gen 
                then putStrLn "Test Passed"
                else putStrLn "Test Failed"

-- Tests
addTest :: Int -> Int -> Bool
addTest x y = applyBitCalc addBits x y == x + y

-- Helpers
greatestPowerOfTwo :: Int -> (Int,Int)
greatestPowerOfTwo n = let (Just x) = lookup 1 powers in x
  where powers = map (\x -> (n `div` 2^x, (2^x, x))) [0..]

decToBits :: Int -> [Bool]
decToBits 0 = [False]
decToBits x = addBits ([True] ++ replicate p False) (decToBits y)
  where (gp, p) = greatestPowerOfTwo x
        y = x - gp

bitsToDec :: [Bool] -> Int
bitsToDec [] = 0
bitsToDec (x:xs) = if x then 2 ^ (length xs) + bitsToDec xs else bitsToDec xs

applyBitCalc :: ([Bool] -> [Bool] -> [Bool]) -> Int -> Int -> Int
applyBitCalc f x y = bitsToDec (f (decToBits x) (decToBits y))