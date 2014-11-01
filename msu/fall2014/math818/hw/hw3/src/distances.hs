dists pts = zipWith (\x y -> abs (x - y)) pts (map (fromIntegral . round) pts)
closeEnough l delta pts = filter ((< delta) . snd) $ zip l (dists pts)

findOffenders beta a delta = take 20 $ closeEnough (map (beta*) [1..]) delta (map (a*beta*) [1..])
