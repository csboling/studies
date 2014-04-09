import Graphics.EasyPlot
import Data.Bits
import Control.Arrow

sarADC :: (Enum b, Ord b, Floating b) => b -> (a -> b) -> a -> (b, [Bool])
sarADC depth f t = fst . last &&& map snd . tail $ scanl comp (0, False) [1..depth] 
                   where comp (v, _) i = let next = v + bit i in
                                         (if next < x then next else v, next < x)
                         bit i    = 2**(depth-i)
                         x        = 2**depth * f t

heaviside x
  | x < 0     = 0
  | otherwise = 1

{-alternative depth f t = accum step 
              where accum term = sum $ map (\i -> term * 2**(depth-i)) [1..depth]
                    step       = heaviside (x - accum 1)
                    x          = f $! t
-}

lfsr taps bits = shiftL 1 bits .|. ones `mod` 2 
                 where ones = popCount (bits .&. taps) + 1

bitdepth = 2
freq     = 0.25
input    = (/2) . (1+) . sin . (2 * pi * freq *)
main = do
       plot X11 [ subtract 0.5 . (2**bitdepth *) . input
--                , fst . alt bitdepth input
                , fst . sarADC bitdepth input
                ]

