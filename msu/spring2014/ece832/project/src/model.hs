{-# LANGUAGE TupleSections #-}
import Debug.Trace
import System.Environment

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit
import Data.Traversable (Traversable, traverse)

import Data.Text (Text, pack, unpack)
import Data.Bits
import Data.Maybe

import Control.Monad (liftM, join)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Arrow

ifTrue :: Bool -> a -> a -> a
ifTrue True  x _ = x
ifTrue False _ y = y

data SarADC  = SarADC 
              { bitDepth :: Int
              , refLo    :: Float
              , refHi    :: Float
              } deriving (Show)
range     sar = refHi sar - refLo sar
lsb       sar = (range sar) / 2^^(bitDepth sar)

offsetSar :: SarADC -> Float -> Float
offsetSar sar = subtract (refLo sar + lsb sar)

scaleSar :: SarADC -> Float -> Float
scaleSar sar = (/ range sar)

calibrate :: SarADC -> Float -> Float
calibrate sar = scaleSar sar . offsetSar sar

fitLine sar = scaleSar sar . 
              subtract (0.5 * lsb sar) .
              offsetSar sar

unsar :: SarADC -> Float -> Float
unsar = (*) . lsb

sarADC :: (Fractional b, Ord b, Integral a) => a -> (t -> b) -> t -> (b, [Bool])
sarADC depth f t = fst . last &&& map snd . tail $ scanl comp (0, False) [1..depth] 
                   where comp (v, _) i = let next = v + bit i in
                                         (if next < x then next else v, next < x)
                         bit i    = 2^(depth-i)
                         x        = 2^^(depth) * f t


lfsr taps bits = shiftL 1 bits .|. ones `mod` 2 
                 where ones = popCount (bits .&. taps) + 1

liftText :: (Read a, Show b) => (a -> b) -> Text -> Text
liftText f = pack . show . f . read . unpack 

textToCL :: (Monad m, Read a) => Conduit (Row Text) m [a]
textToCL   = CL.map $ map (read . unpack)

textFromCL :: (Monad m, Show a) => Conduit [a] m (Row Text)
textFromCL = CL.map $ map (pack . show)

broadcast :: (Monad m, Traversable t) =>
  (a -> Sink i m b) -> t a -> Sink i m (t b)
broadcast f = getZipSink . traverse (ZipSink . f)

hz, timescale :: Float
hz        = 10e3
timescale = 20
toPulses timescale vdd ((t1:vs1):(t2:vs2):vs) = [ (liftText (timescale*) t1):(map (liftText (vdd*)) vs1)
                                                , (liftText (subtract (timescale/1000) . (timescale*)) t2):(map (liftText (vdd*)) vs1)
                                                ]
toPulses timescale vdd ((t:vs):[])            = [ (liftText (timescale*) t):(map (liftText (vdd*)) vs) ]
toPulses _         _   []                     = []
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

split xs = broadcast splitter (zip [0..] xs)
           where splitter (n, name) = getColumn n            =$ 
                                      fromCSV outSettings    =$ 
                                      sinkFile (name ++ ".txt")


maybeHeaviside :: (Num a, Ord a, Num b) => a -> a -> Maybe b
maybeHeaviside eps x
  | x < -eps  = Just 0
  | x >  eps  = Just 1
  | otherwise = Nothing

decode :: (Floating a, Ord a, Show a, Num b) => [a] -> Maybe b
decode bits = sum <$> (sequence $ zipWith choose bits weights)
              where choose  = liftA2 (*) . maybeHeaviside 0.5 . subtract 1.5
                    weights = [ Just (2^i) | i <- [0..depth-1]]
                    depth   = length bits

extract :: (Num a) => SarADC -> [Float] -> Maybe (Float, (Float, a))
extract _   []       = Nothing
extract sar (v:bits) = maybe Nothing 
                             (Just . (fit,) . (ideal v,)) 
                             (decode bits)
                       where ideal = fst . sarADC (bitDepth sar) (calibrate sar)
                             fit   = (fitLine sar v)

response :: Maybe (Float, (Float, Float)) -> [Float]
response Nothing          = []
response (Just (a,(b,c))) = (a:b:[c])
                       
next_inl :: 
  SarADC -> Float -> Maybe Float -> Float
next_inl sar acc = maybe acc (max acc . new_inl)
                   where new_inl = abs . 
                                   uncurry subtract . 
                                   (id &&& (/ lsb sar) . unsar sar)
inl :: (Monad m) =>
  SarADC -> Sink (Maybe Float) m Float
inl sar = CL.fold (next_inl sar) 0

next_dnl ::
  SarADC -> (Float, Float) -> Maybe (Float, Float) -> (Float, Float)
next_dnl sar (acc, step) x = maybe stay choose x where
                             stay = (acc, step)
                             choose (this, next) = if this == step 
                                                   then stay 
                                                   else (diff this, next)
                             diff   = subtract acc
dnlSteps :: (Monad m) =>
  SarADC -> Sink (Maybe (Float, Float)) m (Float, Float)
dnlSteps sar = CL.fold (next_dnl sar) (0, 0)

results :: (Monad m) =>
  SarADC -> [Int] -> Conduit [Float] m (Maybe (Float, (Float, Float)))
results sar cols = CL.map (extract sar . selectCols cols)

selectCols :: [Int] -> [a] -> [a]
selectCols indices = (flip (!!) <$> indices <*>) <$> pure

{- processing functions -}
inSettings  = CSVSettings
              { csvSep = ','
              , csvQuoteChar = Nothing
              }
outSettings = CSVSettings
              { csvSep = ' '
              , csvQuoteChar = Nothing
              }
sarToUse    = SarADC
              { bitDepth = 8
              , refLo    = 1.2
              , refHi    = 2.5
              }
csvToPWLs x y = sourceFile x       $=
                intoCSV inSettings $$
                scale              =$
                split y
csvToVolts x ys = sourceFile x $=
                  intoCSV inSettings $=
                  textToCL           $$
                  results sarToUse [2*n + 1 | n <- [0..bitDepth sarToUse]] =$
                  CL.map response     =$
                  textFromCL          =$
                  fromCSV outSettings =$
                  broadcast sinkFile ys
csvToNonlins x  = sourceFile x       $=
                  intoCSV inSettings $=
                  textToCL           $$
                  results sarToUse [2*n + 1 | n <- [0..bitDepth sarToUse]] =$
                  CL.map (liftM (fst . snd)) =$
                  inl sarToUse
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
         ("volts":_) -> runResourceT $ csvToVolts   (args !! 1) [args !! 2]
         ("inl":_)   -> do 
                        x <- runResourceT $ csvToNonlins (args !! 1)
                        putStrLn $ show x
                        return [()]
                      
         _           -> putStrLn usage >> return [()]
       where usage = "usage: \n\
\                            model pwls  infile \n\
\                      or    model volts infile outfile\n"


