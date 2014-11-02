module FinAbelianIsos where
-- Finds the isomorphism classes of a finite abelian group.

import Control.Applicative
import Control.Arrow
import Data.List

import Math.NumberTheory.Factor

coprime a b = gcd a b == 1

appendNextCoprime :: Integral a => a -> [a] -> [a]
appendNextCoprime x curr = if all (coprime x) curr then x:curr else curr
coprimes = foldr appendNextCoprime []

decompose ps = map (partsOf ps) [1..length ps]
               where partsOf ns = uncurry (:) .
                                  (product *** id) .
                                  flip splitAt ns

components = map decompose . group . pfactors
rawClasses = foldl1 (liftA2 (++)) . components

computeClass :: [Integer] -> [Integer]
computeClass [] = []
computeClass xs = product curr : computeClass (xs \\ curr)
                  where curr = coprimes xs

isoClasses :: Integer -> [[Integer]]
isoClasses = map (sort . computeClass . sort) . rawClasses

printIso :: [Integer] -> String
printIso = intercalate " \\times " .
           map (\x -> "\\mathbb{Z}_{" ++ show x ++ "}")

main = do
  mapM_ (putStrLn . printIso) $ isoClasses 3600
