{-
 A1. 09
 Liam Jardine
 Samuel Kyletoft
 Enayatullah Norozi
-}
import           Data.IntMap     (member)
import           Data.List       (genericLength, nub)
import           Numeric.Natural (Natural)
import           Test.QuickCheck hiding ((==>))

(£) = ($) -- Bri'ish joke
infixr 0 £

(==>) :: Bool -> Bool -> Bool
l ==> r = not l || r

-- Part 1
{-
 Datatypes TERM and Pred sets implements the mathematical concepts of a set of
 expressions and predicates over pure set expressions.
-}
data TERM v
  = Empty
  | Singleton (TERM v)
  | Union (TERM v) (TERM v)
  | Intersection (TERM v) (TERM v)
  | PowerSet (TERM v) -- set of all subsets (including empty and the set itself)
  | Var v
  deriving (Show, Eq)

data PRED v
  = Implies (PRED v) (PRED v)
  | And (PRED v) (PRED v)
  | Or (PRED v) (PRED v)
  | Not (PRED v)
  | Elem (TERM v) (TERM v)
  | Subset (TERM v) (TERM v) -- first arg is subset of second arg?
  deriving (Show, Eq)

-- Part 2
{-
 We define a newtype of hereditary sets (pure sets), where all elements are themselfs sets.
 Using the eval function we can evaluate Terms of a given enviroment to sets.
 The check function gives us a boolean on a given predicate with a given enviroment.
-}
newtype Set =
  S
    { toList :: [Set]
    } -- The semantic domain for pure sets

instance Eq Set where
  -- Assumes no list can contain duplicates
  (==) (S a) (S b) = length a == length b && all (`elem` b) a

eval :: Eq v => Env v Set -> TERM v -> Set
eval e = eval'
  where
    eval' Empty                = S []
    eval' (Singleton s)        = S [eval' s]
    eval' (Union ta tb)        = S . nub . (++) (f ta) . f £ tb -- {1,2} U {3,5} = {1,2,3,5}
    -- Data.List.union is so defintiely a thing, but we're avoiding library functions for the sake of the exercise
    eval' (Intersection ta tb) = S £ filter (`elem` f ta) (f tb) -- {1,2} n {2,3} = {2}
    eval' (PowerSet t)         = S . map S . g . f £ t
    eval' (Var s)              = S [getSetEnv e s]
    f = toList . eval'
    g []     = [[]]
    g (x:xs) = [x : ps | ps <- g xs] ++ g xs

check :: Eq v => Env v Set -> PRED v -> Bool
check e = check'
  where
    check' (Implies pa pb) = check' pa ==> check' pb
    check' (And pa pb)     = check' pa && check' pb
    check' (Or pa pb)      = check' pa || check' pb
    check' (Not p)         = not . check' £ p
    check' (Elem v t)      = eval e v `elem` f t
    check' (Subset ta tb)  = all (`elem` f tb) (f ta) -- first arg is subset of second arg?
    f = toList . eval e

type Env var dom = [(var, dom)]

getSetEnv :: Eq var => Env var dom -> var -> dom
getSetEnv ((v, s):xs) key
  | key == v = s
  | otherwise = getSetEnv xs key
getSetEnv [] _ = error "Key not in map"


-- Part 3

instance Show Set where
  show (S x) = show x

vonNeumann :: Natural -> TERM v
vonNeumann 0 = Empty
vonNeumann n = Union ((vonNeumann (n-1))) (Singleton (vonNeumann (n-1)))

fromVonNeumann :: TERM v -> Natural
fromVonNeumann Empty = 0
fromVonNeumann (Singleton a) = 1
fromVonNeumann (Union (a) (b)) = (fromVonNeumann a) + (fromVonNeumann b)
fromVonNeumann _ = error "itäs wrong i tink"

vonNeumannTest :: Natural -> Bool
vonNeumannTest n = (n ==) . fromVonNeumann . vonNeumann £ n

instance Arbitrary Natural where
  arbitrary = fmap (fromIntegral . abs) (arbitrary :: Gen Integer)


emptyEnv :: Env String Set
emptyEnv = []

-- claim1 n1 n2 = if (n1 <= n2) then (n1 ⊆ n2)
claim1 :: Natural -> Natural -> Bool
claim1 n1 n2 = (n1 <= n2) ==> (check emptyEnv (Subset (vonNeumann n1) (vonNeumann n2)))

testClaim1 = all (uncurry claim1) [(x,y) | x <- [0..15], y <- [0..15]]


-- claim2 n = { 0, 1, . . . , n - 1 }
claim2 :: Natural -> Bool
claim2 0 = vonNeumann 0 == (Empty :: TERM String)
claim2 n = all (check emptyEnv) £ map ((\x -> Elem x (vonNeumann n)) . vonNeumann) [0..(n-1)]

testClaim2 = all claim2 [0..15]
