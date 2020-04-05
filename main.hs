-- These helper functions are just to make verifying calculations quicker and easier
greatestPowerOfTwo :: Int -> (Int,Int)
greatestPowerOfTwo n = let (Just x) = lookup 1 powers in x
  where powers = map (\x -> (n `div` 2^x, (2^x, x))) [0..]

decToBits :: Int -> [Bool]
decToBits 0 = [False]
decToBits x = addBits ([True] ++ replicate (p) False) (decToBits y)
  where (gp, p) = greatestPowerOfTwo x
        y = x - gp

bitsToDec :: [Bool] -> Int
bitsToDec [] = 0
bitsToDec (x:xs) = if x then 2 ^ (length xs) + bitsToDec xs else bitsToDec xs
-- End helpers

xor :: Bool -> Bool -> Bool
xor x y = x /= y

nor :: Bool -> Bool -> Bool
nor x y = not $ x || y

nand :: Bool -> Bool -> Bool
nand x y = not $ x && y

halfAdder :: Bool -> Bool -> [Bool]
halfAdder x y = [x && y, x `xor` y]

fullAdder :: Bool -> Bool -> Bool -> [Bool]
fullAdder x y z = 
  let [xy1, xy2] = halfAdder x y
      [xy3, xy4] = halfAdder xy2 z
  in  [xy3 || xy1, xy4]

-- NOTE: Reversing lists here to allow params to be passed in
-- left justified
-- EX: [True, False] is processed as 10, not 01
addBits :: [Bool] -> [Bool] -> [Bool]
addBits x y = bitAdder (reverse x) (reverse y) False

-- TODO: Refactor
bitAdder :: [Bool] -> [Bool] -> Bool -> [Bool]
bitAdder [] [] c = [c]
bitAdder [] (y:ys) c =
  let [c1, s] = fullAdder False y c 
  in bitAdder [] ys c1 ++ [s]
bitAdder (x:xs) [] c =
  let [c1, s] = fullAdder x False c 
  in bitAdder xs [] c1 ++ [s]
bitAdder (x:xs) (y:ys) c = 
  let [c1, s] = fullAdder x y c 
  in bitAdder xs ys c1 ++ [s]
