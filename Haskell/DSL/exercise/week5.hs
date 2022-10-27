

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.Base (Double, id)
import qualified GHC.Num as Prelude
import GHC.Num
import Data.Eq

type REAL = Double

class Ring r where
  add   ::  r -> r -> r
  neg   ::  r -> r
  zero  ::  r
  mul   ::  r -> r -> r
  one   ::  r

type Nat = Integer -- intended for natural numbers only
newtype P a = P (Nat -> a) -- Polynomial coefficients

instance Ring a => Ring (P a) where
  add = addP
  neg = negP
  zero = zeroP

  mul = mulP
  one = oneP

addP :: Ring a => P a -> P a -> P a
addP (P p) (P q) = P (\i -> add (p i) (q i))
negP :: Ring a => P a -> P a
negP (P p) = P (neg . p)
zeroP :: Ring a => P a
zeroP = P (\_-> zero)

oneP :: Ring a => P a
oneP = P (bool2ring . (0==))

instance Num REAL => Ring REAL where
    add = (Prelude.+)
    mul = (Prelude.*)
    neg = Prelude.negate
    zero = 0
    one = 1

data FunExp = Const REAL
            | X
            | FunExp :+: FunExp
            | FunExp :*: FunExp
            | Negate FunExp

-- Excerice

-- 5.2


mulP :: Ring a => P a -> P a -> P a
mulP (P p) (P q) = P (\k -> sumTo k (\i -> mul (p i) (q (k-i))))


mulP :: (Eq a, Num a, Num [a]) => [a] -> [a] -> [a]
mulP [] p = p
mulP p [] = p
mulP [a] p = [a]
mulP p [b] = [b]
mulP (0 : as) p = mulP as p
mulP p (0 : bs) = mulP p bs
mulP (a : as) (b : bs) = [a+b] + mulP as bs