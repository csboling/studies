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

toPulses timescale vdd (t:vs) = (lift (timescale*) t):(map (lift (vdd*)) vs)
                                where lift f = pack . show . f . read . unpack

hz        = 10e3
timescale = 20e-12
scale :: (Monad m) => Conduit (Row Text) m (Row Text)
scale = CL.map (toPulses (1/(hz * timescale)) 3.0)

getColumn :: Monad m => Int -> Conduit (Row Text) m (Row Text)
getColumn n = awaitForever get
              where get (t:vs) = CL.sourceList . return . (!! n) 
                               $ map (([t]++) . return) vs

--split :: (Show a, Monad m) => [a] -> Sink (Row Text) m [()]
split xs = getZipSink $ traverse (ZipSink . splitter) (zip [0..] xs)
           where splitter (n, name) = getColumn n            =$ 
                                      fromCSV defCSVSettings =$ 
                                      sinkFile (name ++ ".txt")

bitdepth = 2
freq     = 0.25
input    = (/2) . (1+) . sin . (2 * pi * freq *)
main = runResourceT $
         sourceFile "../example-pulses.csv" $=
         intoCSV defCSVSettings             $=
         scale                              $$
         split ["one", "two", "three"]
--       plot X11 [ subtract 0.5 . (2**bitdepth *) . input
--                , fst . sarADC bitdepth input
--                ] 

