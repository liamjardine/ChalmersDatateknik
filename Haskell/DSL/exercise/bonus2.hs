module DSLsofMath.W01 where
import Numeric.Natural (Natural)
import Data.Ratio (Ratio,(%))
import System.Win32 (COORD(yPos), aCCESS_SYSTEM_SECURITY, xBUTTON1)


data Prop = Implies Prop Prop 
            | And Prop Prop 
            | Or Prop Prop
            | Not Prop 
            | Name Name 
            | Con Bool
            deriving(Show)
type Name = String

p1, p2, p3, p4 :: Prop
p1 = And (Name "a") (Not (Name "a"))
p2 = Or (Name "a") (Not (Name "a"))
p3 = Implies (Name "a") (Name "b")
p4 = Implies (And a b) (And b a)
    where a = Name "a"; b = Name "b"

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> p = p

type Env = Name -> Bool
eval :: Prop -> Env -> Bool
eval (Implies p q) env = eval p env ==> eval q env
eval (And p q) env = eval p env && eval q env
eval (Or p q) env = eval p env || eval q env
eval (Not p) env =  not(eval p env)
eval (Name n) env = env n
eval (Con t) env = t

-- 2.12 --------

deMorgan :: Prop -> Prop
deMorgan (And p q) = Not (Or (Not p) (Not q))
deMorgan (Or p q) = Not (And (Not p) (Not q))
deMorgan (Not (And p q)) = Or (Not p) (Not q)
deMorgan (Not (Or p q)) = And (Not p) (Not q)
deMorgan (Not (Not p)) = deMorgan (Not p)



