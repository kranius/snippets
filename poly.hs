instance Num a => Num [a] where
  (f:fs) + (g:gs) = f+g : fs+gs
  fs + [] = fs
  [] + gs = gs
  (f:fs) * (g:gs) = f*g : [f]*gs + fs*(g:gs)
  _ * _ = []
  abs           = undefined
  signum        = map signum
  fromInteger n = [fromInteger n]
  negate        = map (\x -> -x)

