#include <h>

import Math.NumberTheory.Moduli

subsOfSize 0 [] = [[]]
subsOfSize _ [] = []
subsOfSize n (x:xs) = map (x:) (subsOfSize (n - 1) xs) ++ subsOfSize n xs

{-
data ModN = ModN Int Int

instance Num ModN where
  (ModN n a) + (ModN n b) = ModN n ((a + b) `mod` n)
  (ModN n a) * (ModN n b) = ModN n ((a * b) `mod` n)
  negate (ModN n a) = ModN n (n - a)
  fromInteger = error "ModN fromInteger"
  abs = error "ModN abs"
  signum = error "ModN signum"
-}

boseConstr :: Int -> [[Int]]
boseConstr n6 = 
  map (map (\ (a, b) -> a * 3 + b + 1)) $
  [[(x, 0), (x, 1), (x, 2)] | x <- aZ] ++
  map sort [[(x, i), (y, i), (r x y, (i + 1) `mod` 3)] | 
    x <- aZ, y <- aZ, x < y, i <- [0..2]]
  where
  r x y = (((x + y) `mod` d) * 
    fromIntegral (fromJust $ invertMod 2 (fromIntegral d))) `mod` d
  aZ = [0 .. 2 * n6]
  d = 2 * n6 + 1

skolemConstr :: Int -> [[Int]]
skolemConstr n6 = 
  map (map (\ (a, b) -> a * 3 + b + 1)) $
  [[(x, 0), (x, 1), (x, 2)] | x <- aZ] ++
  map sort [[(x, i), (y, i), (r x y, (i + 1) `mod` 3)] | 
    x <- aZ, y <- aZ, x < y, i <- [0..2]] ++
  [[(x, (i + 1) `mod` 3), ((x + n6) `mod` d, i), (2 * n6, 0)] | 
    x <- [0 .. n6 - 1], i <- [0..2]]
  where
  r x y = 
    lolify $ (x + y) `mod` d
    where
    lolify a = if even a
      then a `div` 2
      else (a + d - 1) `div` 2
  aZ = [0 .. 2 * n6 - 1]
  d = 2 * n6 + 1

-- Find k-element subsets of [1..n] such that every t-element subset is
-- contained in exactly one.
stsGen :: Int -> Int -> Int -> [[Int]]
stsGen t k n
 | k < 1 = error "k < 1"
 | t < 1 = error "t < 1"
 | t > k = error "t > k"
 | t == k = subsOfSize t [1..n]
 | t == 2 && k == 3 =
   case n `mod` 6 of
   1 -> skolemConstr (n `div` 6)
   3 -> boseConstr (n `div` 6)
   _ -> error "n must be 1 or 3 mod 6"
 | otherwise = error "TODO"
  where
  initSubsets = [[1..k]]
  --tElemSubsetsRem = [
  --tsRem = 
