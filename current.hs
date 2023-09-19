import Data.Text.Internal.Fusion.Size (larger)
import Distribution.SPDX (LicenseId (MPL_2_0))

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (n : ns) = n + sum ns

myqsort :: Ord a => [a] -> [a]
myqsort [] = []
myqsort (x : xs) = myqsort smaller ++ [x] ++ myqsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

myqsortRev :: Ord a => [a] -> [a]
myqsortRev [] = []
myqsortRev (x : xs) = myqsortRev larger ++ [x] ++ myqsortRev smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

mydouble :: Num a => a -> a
mydouble a = a * 2

myproduct :: Num a => [a] -> a
myproduct [] = 0
myproduct (x : xs) = x * product xs

myproductFold :: Num a => [a] -> a
myproductFold [] = 0
myproductFold xs = foldr (*) 1 xs

myproductFoldL :: Num a => [a] -> a
myproductFoldL [] = 0
myproductFoldL xs = foldl (*) 1 xs

myproductTest :: Num a => [a] -> a
myproductTest = foldl (*) 1

myqsortTest :: Ord a => [a] -> [a]
myqsortTest [] = []
myqsortTest (x : xs) = myqsort smaller ++ [x] ++ myqsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b >= x]

-- This is a proper recursive solution it would seem
-- This was adapted from: https://stackoverflow.com/questions/13784671/simple-haskell-splitlist
splitlist :: [a] -> ([a], [a])
splitlist xs = f xs xs
  where
    f (y : ys) (_ : _ : zs) =
      let (as, bs) = f ys zs
       in (y : as, bs)
    f (y : ys) [_] = ([y], ys)
    f ys [] = ([], ys)
