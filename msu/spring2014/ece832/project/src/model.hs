import Graphics.EasyPlot
import Data.Bits

sarADC :: (Enum b, Ord b, Floating b) => b -> (a -> b) -> a -> b
sarADC depth f t = foldl comp 0 [1..depth] 
                   where comp v i = if v + bit i < x then v + bit i else v
                         bit i    = 2**(depth-i)
                         x        = 2**depth * f t

heaviside x
  | x < 0     = 0
  | otherwise = 1

alt depth f t = foldr comp (0, []) [0..depth-1]
                where comp i (v, bits) = let next = v + 2**i
                                             pred = next < x in 
                                             (if pred then next else v, pred : bits)
                      x = 2**depth * f t
                      

{-alternative depth f t = accum step 
              where accum term = sum $ map (\i -> term * 2**(depth-i)) [1..depth]
                    step       = heaviside (x - accum 1)
                    x          = f $! t
-}

lfsr taps bits = shiftL 1 bits .|. ones `mod` 2 
                 where ones = popCount (bits .&. taps) + 1

bitdepth = 8
freq     = 0.25
input    = (/2) . (1+) . sin . (2 * pi * freq *)
main = do
       plot X11 [ subtract 0.5 . (2**bitdepth *) . input
                , fst . alt bitdepth input
            --    , sarADC bitdepth input
                ]

