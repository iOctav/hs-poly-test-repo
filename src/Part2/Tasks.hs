module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) lTerm rTerm= BinaryTerm Plus lTerm rTerm
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) lTerm rTerm = BinaryTerm Minus lTerm rTerm
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) lTerm rTerm = BinaryTerm Times lTerm rTerm
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    IntConstant _ -> expression
    Variable vlr -> if vlr == varName then replacement else expression
    BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expr = case expr of 
    IntConstant value -> expr
    BinaryTerm op lhv rhv -> 
        let 
            lhv' = evaluate lhv
            rhv' = evaluate rhv
        in
        case (op, lhv', rhv') of
            (_, Variable _, _) -> expr
            (_, _, Variable _) -> expr
            (_, BinaryTerm _ _ _, _) -> BinaryTerm op lhv' rhv'
            (_, _, BinaryTerm _ _ _) -> BinaryTerm op lhv' rhv'
            (Plus, IntConstant value1, IntConstant value2) -> IntConstant $ value1 + value2
            (Minus, IntConstant value1, IntConstant value2) -> IntConstant $ value1 - value2
            (Times, IntConstant value1, IntConstant value2) -> IntConstant $ value1 * value2
    _ -> expr