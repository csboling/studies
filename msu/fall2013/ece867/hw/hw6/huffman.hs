import Data.List
import Data.Function

code2 :: (Num a, Ord a, Num b, Ord b) => [(b, a)] -> [(b, [Char])]
code2 [] = []
code2 [(i, a)] = [(i, "0")]
code2 [(i, a), (j, b)] = [(i, "1"), (j, "0")]
code2 p  = let (i, pi):(j, pj):xs = (order snd) p in
            let (w:ws, c) = get i $ code2 ((i, pi+pj):xs) in
              order fst $ [(i, snd w ++ "0"), (j, snd w ++ "1")] ++ c
            where order f = sortBy (compare `on` f)
                  get l   = partition ((l ==) . fst)

type Codeword b = (b, [Char])

gluecode :: (Num a, Ord a, Num b, Ord b) => 
  Int -> [Char] -> [(b, a)] -> [Codeword b]
gluecode _ _ [] = []
gluecode d w ((i, p):xs) = 
  (i, w ++ show d) : gluecode (d-1) w xs

code :: (Num a, Ord a, Num b, Ord b) => 
          Int -> [(b, a)] -> [Codeword b]
code d p = let (nodes, xs)= splitAt d $ (order snd) p
               i = fst $ head nodes
               (w:ws, c) = get i . code d $ (i, sum (map snd nodes)):xs in
                 order fst $ gluecode (d-1) (snd w) nodes ++ c
             where order f = sortBy (compare `on` f)
                   get l   = partition ((l ==) . fst)
                             
expectation :: (Float -> Float) -> [Float] -> [Float] -> Float
expectation f = (sum .) . zipWith (\p x -> p * f x)

shannonH :: [Float] -> Float
shannonH ps = negate $ expectation (logBase 2) ps ps

main :: IO ()
main = let p = [(1, 0.2), 
                (2, 0.2), 
                (3, 0.1), 
                (4, 0.1), 
                (5, 0.1),
                (6, 0.1),
                (7, 0.1),
                (8, 0.1)]
           sp = sort . map snd $ p
           c  = code2 p in do
         print $ "probabilities: " ++ show sp
         print $ "entropy: " ++ (show . shannonH) sp
         print $ "avg code length: " ++ show  
           (expectation id (map snd p) 
                          (map (fromIntegral . length . snd) c))
         print $ c
--         print $ map (negate . logBase 2) sp
