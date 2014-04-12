{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.EasyPlot

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit
import Data.Traversable

import Data.Text (Text, pack, unpack)
import Data.Bits

import Control.Monad.IO.Class (liftIO)
import Control.Arrow


lfsr taps bits = shiftL 1 bits .|. ones `mod` 2 
                 where ones = popCount (bits .&. taps) + 1

sarADC :: (Enum b, Ord b, Floating b) => b -> (a -> b) -> a -> (b, [Bool])
sarADC depth f t = fst . last &&& map snd . tail $ scanl comp (0, False) [1..depth] 
                   where comp (v, _) i = let next = v + bit i in
                                         (if next < x then next else v, next < x)
                         bit i    = 2**(depth-i)
                         x        = 2**depth * f t

heaviside x
  | x <= 0    = 0
  | otherwise = 1

decode :: (RealFrac a, Num b) => [a] -> b
decode bits = sum $ zipWith ((*) . heaviside . round) bits weights
              where weights = [2^(depth-i) | i <- [1..depth]]
                    depth   = length bits

toPulses timescale vdd (t:vs) = (lift (timescale *) t):(map (lift (vdd*)) vs)
                              where lift f = pack . show . f . read . unpack

hz    = 10e3
scale :: Monad m => Conduit (Row Text) m (Row Text)
scale = CL.map (toPulses (1/hz * 1/20e-12) 3.0)

getColumn n = CL.mapM ((:[]) . (!! n))

split = getZipSink $ traverse (ZipSink . splitter) [0..12]
        where splitter n = getColumn n  =$ fromCSV defCSVSettings -- $$ sinkFile ((show n) ++ ".txt")
              

toPWLs = fromCSV defCSVSettings =$ toFiles
         where toFiles = getZipSink $ traverse (ZipSink . sinkFile) (map ((++".txt") . show) [0..12])



bitdepth = 2
freq     = 0.25
input    = (/2) . (1+) . sin . (2 * pi * freq *)
main = runResourceT $
         sourceFile "../example-pulses.csv" $=
         intoCSV defCSVSettings             $=
         scale                              $$
--         fromCSV defCSVSettings             $$
--         split 
         toPWLs                           
--         sinkFile "blah.pwl"         
--         intoCSV defCSVSettings $=
--       plot X11 [ subtract 0.5 . (2**bitdepth *) . input
--                , fst . sarADC bitdepth input
--                ] 

