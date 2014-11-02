-- Finds the isomorphism classes of a finite abelian group.

import Control.Applicative
import Control.Arrow
import Data.List

coprime a b = gcd a b == 1
coprimeSequence :: Integral a => [a] -> [a]
--coprimeSequence []     = []
--coprimeSequence (x:xs) =

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
computeClass []     = []
computeClass (x:xs) = product curr : computeClass (xs \\ curr)
                      where curr = (x : (filter (coprime x) $ nub xs))

isoClasses :: Integral a => a -> [[a]]
isoClasses = map (computeClass . sortBy (flip compare)) . rawClasses
