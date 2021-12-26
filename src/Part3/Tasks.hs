module Part3.Tasks where

import Util (notImplementedYet)
import Data.List
import Data.Char(digitToInt)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n) : (finc f $ n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
-- ff :: (a -> a) -> a -> [a]
ff f x = let
  nxt y = (f y) : (nxt (f y))
  in
    x : (nxt x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq a = snd $ foldl (\s x -> if length x > fst s then (length x, digitToInt $ head x) else s) (0,0) groupedArr
  where groupedArr = group (sort (concatMap show a))

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = notImplementedYet
--uniq a = let
--  eqGroup | acc [] = acc
--          | acc (x : xs) = if find (\y -> y == x) acc /= Nothing then eqGroup acc xs else eqGroup (x : acc) xs
--  in
--  map (\x -> head x) $ eqGroup [] a

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy = notImplementedYet
--grokBy f l = let
--  helper acc [] = []
--  helper acc (x:xs) = if findGroup x acc /= Nothing then (x : snd (findGroup x acc))  else helper ((f x, [x]) : acc) xs
--  in
--    helper [] l
--  where 
--    findGroup x arr = find (\y -> fst y == f x) arr
