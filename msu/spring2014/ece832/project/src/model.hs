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
  | x <= 0    = 0
  | otherwise = 1

--decode :: RealFrac a => [a] -> Int
decode bits = sum $ zipWith ((*) . heaviside . round) bits weights
              where weights = [2^(depth-i) | i <- [1..depth]]
                    depth   = length bits

lfsr taps bits = shiftL 1 bits .|. ones `mod` 2 
                 where ones = popCount (bits .&. taps) + 1

bitdepth = 2
freq     = 0.25
input    = (/2) . (1+) . sin . (2 * pi * freq *)
main = do
       plot X11 [ subtract 0.5 . (2**bitdepth *) . input
                , fst . sarADC bitdepth input
                ]

