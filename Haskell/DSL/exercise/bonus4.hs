{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
import GHC.Exts.Heap (GenClosure(var))


-- i.

-- Ring class
class Ring a where
    add ::  a -> a -> a
    mul ::  a -> a -> a
    neg   ::  a -> a
    one   ::  a
    zero  ::  a
 
-- ii.

-- Datatype for expressions of a Ring
data R v where
    Add  :: R v -> R v -> R v
    Mul  :: R v -> R v -> R v
    Neg  :: R v -> R v
    Zero :: R v
    One  :: R v
    Var  :: v -> R v
    deriving (Eq, Show)

instance Ring (R v) where
    add = Add
    mul = Mul
    neg = Neg
    zero = Zero
    one = One

-- iii.

-- Additional instance for Integers, Z

type Z = Integer

instance Ring Z where
    add = (Prelude.+)
    mul = (Prelude.*)
    neg = Prelude.negate
    zero = 0
    one = 1

-- Additional instance for Bool

instance Ring Bool where
    add = (Prelude.||)
    mul = (Prelude.&&)
    neg = Prelude.not
    zero = Prelude.False
    one = Prelude.True

-- iv.

-- Function for evaluating expression of a Ring

eval :: Ring a => (v -> a) -> (R v -> a)
eval var = e where
    e (Add x y) = add (e x) (e y)
    e (Mul x y) = mul (e x) (e y)
    e (Neg x)   = neg (e x)
    e (Var v)   = var v
    e Zero      = zero
    e One       = one

-- v

-- Expressions and tests

x, y, z :: R String
[x, y, z] = map Var ["x", "y", "z"]

e1, e2, e3 :: R String
e1 = Mul x y
e2 = Neg z
e3 = Add One (Mul e1 e2)

varZ :: Num p => String -> p
varZ "x" = 1
varZ "y" = 2
varZ "z" = 3

varB :: String -> Bool
varB "x" = True
varB "y" = False
varB "z" = True


evalZ :: (v -> Z) -> (R v -> Z)
evalZ = eval

evalB :: (v -> Bool) -> (R v -> Bool)
evalB = eval

tests :: (Bool, Bool)
tests = ( map (evalZ varZ) [e1,e2,e3]  ==  [2, -3, -5]
        , map (evalB varB) [e1,e2,e3]  ==  [False, False, True])