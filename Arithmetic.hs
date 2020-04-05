module Arithmetic (addBits) where

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
-- I think whichever list is longer can just be
-- tacked onto the end of the calculation after
-- the carry is factored in
bitAdder :: [Bool] -> [Bool] -> Bool -> [Bool]
bitAdder [] [] c = [c]
bitAdder list1 list2 c = bitAdder xs ys c1 ++ [s]
  where (x:xs) = if null list1 then False:[] else list1
        (y:ys) = if null list2 then False:[] else list2
        [c1, s] = fullAdder x y c