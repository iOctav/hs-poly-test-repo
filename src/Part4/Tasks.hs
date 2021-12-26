module Part4.Tasks where

import Util(notImplementedYet)


-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst =
    reversed (reverse lst)
    where reversed [] = REmpty
          reversed (first : tl) = (reversed tl) :< first

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show REmpty = "[]"
    show lst = let
      helper REmpty = ""
      helper (REmpty :< lst) = show lst
      helper (hd :< lst) = helper hd ++ "," ++ show lst
      in
      "[" ++ helper lst ++ "]"
    showsPrec = notImplementedYet
instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) _ REmpty = False
    (==) REmpty _ = False
    (==) (lprt :< lLast) (rprt :< rLast) = if lLast /= rLast then False else (==) lprt rprt
    (/=) trw wer = not $ (trw == wer)
instance Semigroup (ReverseList a) where
    (<>) REmpty REmpty = REmpty
    (<>) REmpty rl = rl
    (<>) rl REmpty = rl
    (<>) lRL (rHead :< rLast) = (:<) ((<>) lRL rHead) rLast
instance Monoid (ReverseList a) where
    mempty = REmpty
instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (rHead :< rLast) = (fmap f rHead) :< f rLast
instance Applicative ReverseList where
    pure a = REmpty :< a
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) (fs :< f) xLst = (<*>) fs xLst <> fmap f xLst
instance Monad ReverseList where
    return = pure
    (>>=) REmpty _ = REmpty
    (>>=) (xs :< x) f =  (>>=) xs f <> f x
