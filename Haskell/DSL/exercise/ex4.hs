{-# LANGUAGE GADTs #-}
import Distribution.Simple.Setup (InstallFlags(installCabalFilePath))
import qualified System.Win32.File as Prelude


main :: IO()
main = do
    return ()

-- 4.22

data SR v where
     Add :: SR v -> SR v -> SR v
     Mul :: SR v -> SR v -> SR v
     Zero :: SR v
     One :: SR v
     Var :: v -> SR v
    deriving (Eq, Show)

class SemiRing a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    zero :: a
    one :: a

instance SemiRing (SR v) where
    (+) = Add
    (*) = Mul
    zero = Zero
    one = One

-- (R, +, 0) is a commutative monoid with identity 0
-- (+ is associative, commutative, 0 is identity)

-- (R, *, 1) is a commutative monoid with identity 1
-- (+ is associative, commutative, 1 is identity)

instance SemiRing Integer where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    zero = 0
    one = 1

-- x || (y && z) = (x || y) && (x || z)
-- x && (y || z) = (x && y) || (x && z)


instance SemiRing Bool where
    (+) = (Prelude.||)
    (*) = (Prelude.&&)
    zero = Prelude.False
    one = Prelude.True








