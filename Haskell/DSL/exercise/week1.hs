module DSLsofMath.W01 where
import Numeric.Natural (Natural)
import Data.Ratio (Ratio,(%))
import System.Win32 (COORD(yPos), aCCESS_SYSTEM_SECURITY, xBUTTON1)



type REAL = Double

data ImagUnit = IU deriving Show
i :: ImagUnit
i = IU

data CA = Plu1 REAL REAL ImagUnit
        | Plu2 REAL ImagUnit REAL

data CB = Plus1 REAL REAL ImagUnit  -- the form |a + bi|
        | Plus2 REAL ImagUnit REAL  -- the form |a + ib|
    deriving (Show)

showCB :: CB -> String 
showCB (Plus1 x y IU) = show x ++ "+" ++ show y ++ "i"
showCB (Plus2 x IU y) = show x ++ "+" ++ "i" ++ show y

e1, e2, e3 :: CB
e1 = Plus1 3 2 IU
e2 = Plus1 (7/2) (-2/3) IU
e3 = Plus2 0 IU pi

data CC = PlusI REAL REAL
    deriving (Show, Eq)

re, im :: CC -> REAL
re z@(PlusI x _) = x
im z@(PlusI _ y) = y

addCC :: CC -> CC -> CC
addCC w@(PlusI a b) z@(PlusI x y) = PlusI (a+x) (b+y)

mulCC :: CC -> CC -> CC
mulCC (PlusI a b) (PlusI x y) = PlusI real imag
    where   real = a*x - b*y
            imag = a*y + b*x

rconCC :: REAL -> CC
rconCC r = PlusI r 0

iCC :: CC
iCC = PlusI 0 1

data CE = Add CE CE
    | Mul CE CE
    | RCon REAL
    | I

eval :: CE -> CC
eval (Add x y) = addCC (eval x) (eval y)
eval (Mul x y) = mulCC (eval x) (eval y)
eval I = iCC
eval (RCon r) = rconCC r

e5, e6, e7 :: CE
e5 = Add (RCon 1) I
e6 = Mul (Add (RCon 5) I) (RCon 8)
e7 = Mul I I




----------------

data Foo a = Hi a | Ho a a
data Boo = Tru | Fal
data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 

foo :: Foo a -> Foo a
foo (Hi x) = Ho x x
foo (Ho x _) = Hi x




