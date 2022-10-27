module DSLsofMath.W01 where
import Numeric.Natural (Natural)
import Data.Ratio (Ratio,(%))
import System.Win32 (COORD(yPos), aCCESS_SYSTEM_SECURITY, xBUTTON1)


-- identity function
-- id:: a -> a

-- higher order function
-- flip :: (a -> b -> c) -> (a -> c -> b)
-- flip op a b = op b a

-- 1.1 --------

data Exp = Con Integer
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Var String
    deriving (Eq, Show)

a1, a2 :: Exp
a1 = Plus (Con 2) (Con 2)
a2 = Plus a1 (Times (Con 7) (Con 9))
-- a3 = 8 * (2 + 11) - (3 + 7) * (a1 + a2)
a3 = Minus part1 part2
    where
        part1 = Times (Con 8) (Plus (Con 2) (Con 11))
        part2 = Times (Plus (Con 3) (Con 7)) (Plus a1 a2)

-- 1.2 --------

eval :: Exp -> Integer 
eval (Con c) = c
eval (Plus x y) = eval x + eval y
eval (Minus x y) = eval x - eval y
eval (Times x y) = eval x * eval y

e1, e2 :: Exp
e1 = Plus (Con 1) (Times (Con 2) (Con 3))
e2 = Times (Plus (Con 1) (Con 2)) (Con 3)

-- 1.3 --------
-- a.

-- Add variable to Exp data type

-- b.

c1 = Times (Minus (Var "x") (Con 15)) 
    (Times (Plus (Var "y") (Con 12)) (Var "z"))

varVal :: String -> Integer 
varVal "x" = 5
varVal "y" = 8
varVal "z" = 13
varVal name = error ("Variable " ++ name ++ " not found")

-- c.

eval' :: Exp -> Integer 
eval' (Con c) = c
eval' (Var name) = varVal name
eval' (Plus x y) = eval' x + eval' y
eval' (Minus x y) = eval' x - eval' y
eval' (Times x y) = eval' x * eval' y

-- 1.5 --------

isoR :: (Bool -> t) -> (t, t)
isoR f = (f True, f False)

isoL :: (t, t) -> (Bool -> t)
isoL (x, y) = \z -> if z then x else y

{-
True -> x
False -> y

Show: isoR ○ isoL = id and isoL ○ isoR = id

isoR and isoL are bijections.
(isoL . isoR) f = id f (for all f)
<=>     (isoL(isoR f)) = f
<=>     (isoL(f True, f False)) = f
<=>     \z -> if z then (f True) else (f False) = f
<=>     \z -> case z of
            True -> f True
            False -> f False
-}

-- 1.8 --------

{-
What does function composition do to a sequence?
For a sequence a what is a ◦ (1+)? What is (1+) ◦ a?
 - The sequence a could be thougt of as a function
 application a(i). So for a given index i we get the
 element at a(i). When a ◦ (1+) composed and run for
 som input it will first incriment and then perform a(i+1).
 While (1+) ◦ a will perform the function a(i) and then
 increment, giving us a(i)+1.

How is liftSeq1 related to fmap? liftSeq0 to conSeq?
- The liftSeq1 takes a function and applies it on to
 every element in a sequence, giving us a new sequence.
 So {a_1, a_2, ..., a_n} and f(a) gives us 
 {f(a_1), f(a_2), ..., f(a_n)}. Which is exactly wat fmap
 does, but to a list.

 liftSeq0 is a "nullary" operation taking a constant and an
 index i and returning a sequence of i c:s. This is exactly
 what conSeq does.
-}