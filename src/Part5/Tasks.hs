module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ ini [] = ini
myFoldl f ini (x : xs) = foldl f (f ini x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ ini [] = ini
myFoldr f ini (x : xs) = x `f` foldr f ini xs

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x s -> (:) (f x) s) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\x s -> (++) (f x) s) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (\x s -> (++) x s) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\x s -> (:) s x) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x s -> if p x then x : s else s) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\x s -> if p x then (x : fst s, snd s) else (fst s, x : snd s)) ([],[])

