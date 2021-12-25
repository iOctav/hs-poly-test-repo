module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed
import Data.List

factorial :: Double -> Double
factorial i = if i <= 1 then 1 else i * factorial (i - 1)

modDoublePi :: Double -> Double
(modDoublePi) x = mod' x (2 * pi)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = let
    term k z = (-1) ** k * (z ** (2 * k + 1) / factorial (2 * k + 1))
    helper k z = if abs (term (k + 1) z) < eps then term k z else term k z + helper (k + 1) z
    in
    helper 0 (modDoublePi x)
  where eps = 0.0001

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = let
    term k z = (-1) ** k * (z ** (2 * k) / factorial (2 * k))
    helper k z = if abs (term (k + 1) z) < eps then term k z else term k z + helper (k + 1) z
    in
    helper 0 (modDoublePi x)
  where eps = 0.0001

-- наибольший общий делитель двух чисел
-- The Euclidean Algorithm
myGCD :: Integer -> Integer -> Integer
myGCD x y | x == 0 = abs y
          | y == 0 = abs x
          | otherwise = myGCD y (mod x y)


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y | d < 1 || d > 31 || m < 1 || m > 12 || y < 1 = False
                    | isLeapYear y && m == 2 && d > 29 = False
                    | (not $ isLeapYear y) && m == 2 && d > 28 = False
                    | elem m [4,6,9,11] && d > 30 = False
                    | otherwise = True
                  where isLeapYear yy = mod yy 4 == 0 && (mod yy 100 /= 0 || mod yy 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x p = if p == 0 then 1 else (*) x $ myPow x $ p - 1

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | elem x [2,3] = True
          | x <= 1 || mod x 2 == 0 || mod x 3 == 0 = False
          | otherwise  = let
            isPrime' x i
                | i >= x = True
                | otherwise = if mod x i == 0 || mod x (i + 2) == 0 then False else isPrime' x $ i + 6 in
              isPrime' x 5

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea coords = let
    plusCoords ((x, y) : (x', y') : xs) = x * y' - y * x' + plusCoords ((x', y') : xs)
    plusCoords ((x, y) : []) = x * snd (shapeHead) - y * fst shapeHead
    plusCoords [] = 0
    in
    abs $ plusCoords coords / 2
    where shapeHead = head coords

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | c' > a' + b' = -1
                   | comparisonSqrts > 0 = 0
                   | comparisonSqrts < 0 = 1
                   | comparisonSqrts == 0 = 2
                where sortedLegs = sort [a, b, c]
                      c' = sortedLegs !! 2
                      b' = sortedLegs !! 1
                      a' = sortedLegs !! 0
                      comparisonSqrts = c' ** 2 - (a' ** 2 + b' ** 2)
