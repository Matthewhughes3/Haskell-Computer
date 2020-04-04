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

-- NOTE: Leaving these in for now to reference how fullAdders are chained together
--doubleFullAdder :: [Bool] -> [Bool]
--doubleFullAdder [x1, y1, z, x2, y2] = 
--  let [c, s] = fullAdder x1 y1 z
--      [c2, s2] = fullAdder x2 y2 c
--  in  [c2, s2, s]
--
--tripleFullAdder :: [Bool] -> [Bool]
--tripleFullAdder [x1, y1, z, x2, y2, x3, y3] =
--  let [c, s2, s] = doubleFullAdder [x1, y1, z, x2, y2]
--      [c2, s3] = fullAdder x3 y3 c
--  in  [c2, s3, s2, s]

-- TODO: Figure out the best way to convert a decimal number to a list of Bools
--decToBits :: Int -> [Bool]
--decToBits x = 
--
--greatestPowerOfTwo :: Int -> Int
--greatestPowerOfTwo x = greatestPower x 2
--
--greatestPower :: Int -> Int -> Int
--greatestPower 0 y = y `div` 2
--greatestPower 1 y = y
--greatestPower 2 y = y
--greatestPower x y = greatestPower (x `div` 2) (y*2)

-- NOTE: Reversing lists here to allow params to be passed in
-- left justified since that's how numbers are typically represented
-- EX: [True, False] is processed as 10, not 01
addBits :: [Bool] -> [Bool] -> [Bool]
addBits x y = bitAdder (reverse x) (reverse y) False


bitAdder :: [Bool] -> [Bool] -> Bool -> [Bool]
bitAdder [] _ c = [c]
bitAdder _ [] c = [c]
bitAdder (x:xs) (y:ys) c = 
  let [c1, s] = fullAdder x y c 
  in bitAdder xs ys c1 ++ [s]
