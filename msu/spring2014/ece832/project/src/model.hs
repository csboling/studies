import System.Environment
import Graphics.EasyPlot

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
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
bitdepth = 2
freq     = 0.25
input    = (/2) . (1+) . sin . (2 * pi * freq *)
plots    = plot X11 [ subtract 0.5 . (2**bitdepth *) . input
                    , fst . sarADC bitdepth input
                    ] 

liftText :: (Read a, Show b) => (a -> b) -> Text -> Text
liftText f = (pack . show) `fmap` f `fmap` (read . unpack)

--toPulses :: (Num a, Read a, Show a, Num b, Read b, Show b) => 
--            a -> b -> Row Text -> [Row Text]
toPulses _         _   []     = []
toPulses timescale vdd (t:vs) = [ (liftText (subtract (timescale/1000) . (timescale*)) t):scaled
                                , (liftText (timescale*) t):scaled
                                ]
                                where scaled = (map (liftText (vdd*)) vs)
                                

hz :: Float
hz        = 10e3

timescale :: Float
timescale = 20

scale :: (Monad m) => Conduit (Row Text) m (Row Text)
scale = awaitForever $ CL.sourceList . toPulses (1/(timescale*hz)) 3.0

getColumn :: Monad m => Int -> Conduit (Row Text) m (Row Text)
getColumn n = awaitForever get
              where get (t:vs) = CL.sourceList . return . (!! n) 
                               $ map (([t]++) . return) vs

split xs = getZipSink $ traverse (ZipSink . splitter) (zip [0..] xs)
           where splitter (n, name) = getColumn n            =$ 
                                      fromCSV outSettings    =$ 
                                      sinkFile (name ++ ".txt")


inSettings  = CSVSettings
              { csvSep = ','
              , csvQuoteChar = Nothing
              }
outSettings = CSVSettings
              { csvSep = ' '
              , csvQuoteChar = Nothing
              }

csvToPWLs x y = sourceFile x       $=
                intoCSV inSettings $$
                scale              =$
                split y

selectCols :: [a] -> [Int] -> [a]
selectCols xs indices = map ($ xs) (map (flip (!!)) indices)

heaviside x
  | x <= 0    = 0
  | otherwise = 1

decode :: (RealFrac a, Num b) => [a] -> b
decode bits = sum $ zipWith ((*) . heaviside . round) bits weights
              where weights = [2^i | i <- [0..depth-1]]
                    depth   = length bits

select_bits :: (RealFrac a) => Int -> [a] -> [a]
select_bits depth bs = (last bs):[decode $ selectCols bs slots]
                       where slots = [2*n + 1 | n <- [0..depth-1]]

bitify :: (Monad m) => Int -> Conduit (Row Text) m (Row Text)
bitify n = CL.map $ map (pack . show) . select_bits n . map (read . unpack)

csvToVolts x y = sourceFile x        $=
                 intoCSV inSettings  $$
                 bitify 4            =$
                 fromCSV outSettings =$
                 sinkFile y


main = do
       args <- getArgs
       runResourceT $ csvToPWLs (args !! 0) outList 
       where outList = map ("data/pwls/" ++) 
                                        [ "VALID"
                                        , "SELECT_V_REF"
                                        , "SELECT_V_IN"
                                        , "SAMPLE_INPUT"
                                        , "CLOSE_FEEDBACK"
                                        , "BIT_0"
                                        , "BIT_1"
                                        , "BIT_2"
                                        , "BIT_3"
                                        , "BIT_4"
                                        , "BIT_5"
                                        , "BIT_6"
                                        , "BIT_7"              
                                        ]
                      --csvToVolts (args !! 0) (args !! 1)


