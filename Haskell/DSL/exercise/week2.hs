{-# LANGUAGE GADTs #-}
module DSLsofMath.W02 where
import Numeric.Natural (Natural)
import Data.Ratio (Ratio,(%))
import System.Win32 (COORD(yPos), aCCESS_SYSTEM_SECURITY, xBUTTON1, BOOL)

data Prop = Implies Prop Prop 
            | And Prop Prop 
            | Or Prop Prop
            | Not Prop 
            | Name Name 
            | Con Bool
type Name = String



type Syn = Prop
type SimpleSem = Bool
type Sem = Bool

p1, p2, p3, p4 :: Prop
p1 = And (Name "a") (Not (Name "a"))
p2 = Or (Name "a") (Not (Name "a"))
p3 = Implies (Name "a") (Name "b")
p4 = Implies (And a b) (And b a)
    where a = Name "a"; b = Name "b"


type Env = Name -> Bool
eval :: Syn -> Env -> Sem
eval (Implies p q) env = eval p env ==> eval q env
eval (And p q) env = andS (eval p env) (eval q env)
eval (Or p q) env = orS (eval p env) (eval q env)
eval (Not p) env =  notS(eval p env)
eval (Name n) env = env n
eval (Con t) env = t

isTautology :: Prop -> Bool
isTautology p = and (map (eval p) (envs (freeNames p)))

conS :: Bool -> Sem
conS = id
notS :: Sem -> Sem
notS = not
andS :: Sem -> Sem -> Sem
andS = (&&)
orS :: Sem -> Sem -> Sem
orS = (||)
nameS :: Name -> (Name -> Bool) -> (Bool)
nameS n env = env n

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> p = p


envs :: [Name] -> [Env]
envs [ ] = [error "envs: never used"]
envs (n : ns) = [ \n' -> if n' == n then b else e n'
                 | b <- [False, True]
                  , e <- envs ns
                ]

freeNames :: Prop -> [Name]
freeNames (Name name) = [name]
freeNames p = [] ++ freeNames p


orElim :: Either a b -> (Either a b -> Bool) -> (Either a b -> Bool) -> Bool 
orElim (Left a) f g = f (Left a)
orElim (Right b) f  g = g (Right b)

type DVar = String

data FOL where
  Andd :: FOL -> FOL -> FOL
  Nott :: DVar -> FOL -> FOL
  Forall :: DVar -> FOL -> FOL
  Exists :: FOL -> FOL -> FOL
  Pred :: Name -> [Dom] -> FOL

data Dom where -- Numbers
  Var :: DVar -> Dom
  Add :: DVar -> Dom
  Mul :: DVar -> Dom
  Conn :: Integer -> Dom




data Fals
data Tru = Obvious

test1 :: Tru
test1 = Obvious

type And p q = (p,q)

test2 :: And Tru Tru
test2 = (Obvious, Obvious)

swap :: And p q -> And q p
swap (a, b) = (b, a) 

type Hi = And Integer Bool 
hi :: Hi
hi = (1, True)

type Or p q = Either p q

swap2 :: Or p q   ->  Or q p
swap2 (Left x   ) = Right  x
swap2 (Right  y ) = Left y

type Not p = p -> Fals


-- 2.25 --------

