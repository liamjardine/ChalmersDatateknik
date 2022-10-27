import           Data.IntMap     (member)
import           Data.List       (genericLength, nub)
import           Debug.Trace     ()
import           Numeric.Natural (Natural)

-- Exercise 3.1

-- Partial Derivatives
type REAL = Double 
type F1 = REAL -> REAL
type F2 = (REAL,REAL) -> REAL

type DelZ = REAL
type DelX = REAL
type DelY = REAL

type D1 = ((REAL, REAL) -> REAL)

-- D1 f (x,y) = lim 0 (psi, f (x,y))


-- type psi1 :: ((REAL, REAL) -> REAL) -> (REAL, REAL) -> REAL\{0}-> REAL
-- psi1 f (x,y) h = (f(x+h, y) - f(x,y))/h
-- psi2 f (x,y) h = (f(x, y+h) - f(x,y))/h


fstFixed :: a -> (b -> (a, b))
fstFixed a = undefined 

sndFixed :: b -> (a -> (a, b))
sndFixed b = undefined 

-- Data D = D (f + g) = D f + D g
-- D (f ∗ g) = D f ∗ g + f ∗ D g
-- D id = const 1
-- D (const a) = const 0
-- D (f ◦ g) = (D f ◦ g) ∗ D g -- chain rule
-- D sin = cos
-- D cos = − sin
-- D exp = exp

-- L :: T * Q * V -> R

class C a where         foo :: a -> a
instance C Int where    foo i = -i
instance C Bool where   foo = not
instance C String where foo = reverse

