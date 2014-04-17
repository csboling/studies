import Debug.Trace
import System.Environment
import Graphics.EasyPlot

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit
import Data.Traversable

import Data.Text (Text, pack, unpack)
import Data.Bits
import Data.Maybe

import Control.Monad(liftM)
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
liftText f = pack . show . f . read . unpack 

--toPulses :: (Num a, Read a, Show a, Num b, Read b, Show b) => 
--            a -> b -> [Row Text] -> [Row Text]
toPulses timescale vdd ((t1:vs1):(t2:vs2):vs) = [ (liftText (timescale*) t1):(map (liftText (vdd*)) vs1)
                                                , (liftText (subtract (timescale/1000) . (timescale*)) t2):(map (liftText (vdd*)) vs1)
                                                ]
toPulses timescale vdd ((t:vs):[])            = [ (liftText (timescale*) t):(map (liftText (vdd*)) vs) ]
toPulses _         _   []                     = []
                                

hz :: Float
hz        = 10e3

timescale :: Float
timescale = 20

scale :: (Monad m) => Conduit (Row Text) m (Row Text)
scale = do
        row1 <- await
        row2 <- CL.peek
        case row1 of
          Nothing -> return ()
          _       -> do 
                     CL.sourceList $ toPulses (1/(timescale*hz)) 3.0 (catMaybes [row1, row2])
                     scale

getColumn :: Monad m => Int -> Conduit (Row Text) m (Row Text)
getColumn n = awaitForever get
              where get (t:vs) = CL.sourceList . return . (!! n) 
                               $ map (([t]++) . return) vs

broadcast :: (Monad m, Traversable t) =>
  (a -> Sink i m b) -> t a -> Sink i m (t b)
broadcast f = getZipSink . traverse (ZipSink . f)

split xs = broadcast splitter (zip [0..] xs)
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

maybeHeaviside :: (Num a, Ord a) => a -> a -> Maybe a
maybeHeaviside eps x
  | x < -eps  = Just 0
  | x >  eps  = Just 1
  | otherwise = Nothing

heaviside x
  | x <= 0    = 0
  | otherwise = 1

decode :: (RealFrac a, Num b, Show a) => [a] -> b
decode bits = sum $ zipWith ((*) . heaviside . subtract 1.5) bits weights
              where weights = [2^i | i <- [0..depth-1]]
                    depth   = length bits

select_bits :: (RealFrac a, Show a) => Int -> [a] -> [a]
select_bits depth bs = (bs !! 1):[decode $ selectCols bs slots]
                       where slots = [2*n + 3 | n <- [0..depth-1]]

bitify :: (Monad m) => Int -> Conduit (Row Text) m (Row Text)
bitify n = CL.map $ map (pack . show) . select_bits n . map (read . unpack)

csvToVolts x ys     = sourceFile x        $=
                      intoCSV inSettings  $$
                      bitify 8            =$
                      fromCSV outSettings =$
                      broadcast sinkFile ys

main = do
       args <- getArgs
       case args of
         ("pwls":_) -> runResourceT $ csvToPWLs (args !! 1) outList 
                       where outList = map ("data/pwls/" ++) 
                                           [ "BIT_7"
                                           , "BIT_6"
                                           , "BIT_5"
                                           , "BIT_4"
                                           , "BIT_3"
                                           , "BIT_2"
                                           , "BIT_1"
                                           , "BIT_0"              
                                           , "CLOSE_FEEDBACK"
                                           , "SAMPLE_INPUT"
                                           , "SELECT_V_REF"
                                           , "SELECT_V_IN"
                                           , "VALID"
                                           ]
         ("volts":_) -> runResourceT $ csvToVolts (args !! 1) [args !! 2]
         _           -> putStrLn usage >> return [()]
       where usage = "usage: \n\
\                            model pwls  infile \n\
\                      or    model volts infile outfile\n"


