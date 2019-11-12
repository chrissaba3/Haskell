doubleMe x = x + x
--doubles int or double value

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
--recursive factorial
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
--Haskell Examples from website
----------------------------------------------------------------------
--Haskell Integration estimation using Trapezoid method

--Rounds the number of decimal points to 0, but keeps in floating point type
round0dp :: Double -> Double
round0dp x = fromIntegral (round $ x)
--This is necessary because entering a decimal value in the 'n' function for integration would give a false estimation
--Implementing this allows the user to enter any decimal value in 'n' and still recieve the same approximation as before


--Integration function, for syntax use "integration (const or function) a b n"
-- f is function or constant, a is integration lower bound, b is upper bound, and n is amount of approximations
--ex integration (const 2) 1 4 150
--ex integration (\x -> x^3) 1 2 400
integration :: (Double -> Double) -> Double -> Double -> Double -> Double
integration f a b n = h / 2 * (f a + f b + 2 * partial_sum)
    where
        h = (b - a) / round0dp n
        most_parts  = map f (pointsWithOffset (round0dp n-1) h a)
        partial_sum = sum most_parts

points :: Double -> Double -> [Double]
points x1 x2
  | x1 <= 0 = []
  | otherwise = (x1*x2) : points (x1-1) x2

pointsWithOffset :: Double -> Double -> Double -> [Double]
pointsWithOffset x1 x2 offset = map (+offset) (points x1 x2)
