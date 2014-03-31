import Graphics.EasyPlot

sarADC depth f t = foldl comp 0 [1..depth] 
                   where comp v i = if v + bit i < x then v + bit i else v
                         bit i    = 2**(depth-i)
                         x        = (2**depth *) $ f t

bitdepth = 8
freq     = 1
input    = (/2) . (1+) . sin . (2 * pi * freq *)
main = do
       plot X11 [input, (/ (2**bitdepth)) . sarADC bitdepth input]

