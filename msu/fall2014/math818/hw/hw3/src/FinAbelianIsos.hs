module FinAbelianIsos where
-- Finds the isomorphism classes of a finite abelian group.

import Control.Applicative
import Control.Arrow
import Data.List

coprime a b = gcd a b == 1
appendNextCoprime x curr = if all (coprime x) curr then x:curr else curr
coprimes :: Integral a => [a] -> [a]
coprimes = foldr appendNextCoprime []

factor :: Integral a => a -> [a]
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
           in (prime :) $ factor $ div n prime

decompose ps = map (partsOf ps) [1..length ps]
               where partsOf ns = uncurry (:) .
                                  (product *** id) .
                                  flip splitAt ns

components :: Integral a => a -> [[[a]]]
components = map decompose . group . factor

rawClasses :: Integral a => a -> [[a]]
rawClasses = foldl1 (liftA2 (++)) . components

computeClass :: Integral a => [a] -> [a]
computeClass [] = []
computeClass xs = product curr : computeClass (xs \\ curr)
                  where curr = coprimes xs

isoClasses :: Integral a => a -> [[a]]
isoClasses = map (computeClass . sort) . rawClasses
