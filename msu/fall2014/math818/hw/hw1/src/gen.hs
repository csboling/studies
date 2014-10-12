module Generators where

import Control.Applicative
import Control.Arrow
import Control.Monad

plusMod n x y = (x + y) `mod` n
--plusModPair m n = join (plusMod m) *** join (plusMod n)
generate m n = (join $ iterate . plusMod m) *** (join $ iterate . plusMod n)

cycles m n = map (uncurry zip . generate m n) as
  where as = (,) <$> [0..m-1] <*> [0..n-1]
           
generators m n = map results $ cycles m n
                 where results []     = []
                       results (x:xs) = x : (takeWhile (/= x) xs)

main = mapM_ (putStrLn . show) $ generators 5 5

