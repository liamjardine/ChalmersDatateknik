module DSLsofMath.W02 where
import Numeric.Natural (Natural)
import Data.Ratio (Ratio,(%))
import System.Win32 (COORD(yPos), aCCESS_SYSTEM_SECURITY, xBUTTON1, NLSVERSIONINFOEX (dwNLSVersion))
import Distribution.FieldGrammar (uniqueField)

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

isTautology :: Prop -> Bool
isTautology p = and (map (eval p) (envs (freeNames p)))

envs :: [Name] -> [Env]
envs [ ] = [error "envs: never used"]
envs (n : ns) = [ \n' -> if n' == n then b else e n'
                 | b <- [False, True]
                  , e <- envs ns
                ]

-- 2.1 --------



freeNames :: Prop -> [Name]
freeNames (Name name) = [name]
freeNames (And p q) = freeNames p ++ freeNames q
freeNames (Or p q) = freeNames p ++ freeNames q
freeNames (Implies p q) = freeNames p ++ freeNames q
freeNames (Not p) = freeNames p
freeNames (Con _) = []


-- 2.12 --------

-- 2.14 --------
dfn :: Prop -> Prop
dfn p = dfnOfProp names (eval p)
    where names = freeNames p

dfnOfProp :: [Name] -> (Env -> Bool) -> Prop
dfnOfProp names f = foldl Or (Con False) $ map propOfList trueCombinations
                 where  allCombinations = combs names
                        trueCombinations = filter (f . envOfList) allCombinations

propOfList :: [(Name, Bool)] -> Prop
propOfList ls = foldl And (Con True) $ map propOfPair ls

propOfPair :: (Name, Bool) -> Prop
propOfPair (n,b) = if b then Name n else Not (Name n)

combs :: [Name] -> [[(Name, Bool)]]
combs [] = [[]]
combs (name: names) = [(name,b) : ls | b <- [False, True], ls <- combs names]

names' :: Prop -> [Name]
names' = freeNames

envOfList :: [(Name, Bool)] -> Env
envOfList [] n = True
envOfList ((n',b):ls) n = if n' == n then b else envOfList ls n


-- 2.16 --------


