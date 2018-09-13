import Control.Monad

div2 x
  | mod x 2 /= 0 = False
  | otherwise = True

filter' _ [] = []
filter' p xs = [x|x<-xs,p x]

filterM' _ [] = return []
filterM' p (x:xs) =
  let rest = filterM' p xs in
    do b <- p x
       if b then liftM (x:) rest
       else                 rest

--filterM'' _     [] = return []
--filterM'' p (x:xs) = do take

a %? b = a + b*2
