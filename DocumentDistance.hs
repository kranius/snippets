import System.Environment

euclidianNorm v = sqrt . fromIntegral . sum . map (^2) $ v
dotProduct v1 v2 = sum $ zipWith (*) v1 v2

parseWords s = id

docDistance filename = do
  putStrLn filename
  content <- fmap lines $ readFile filename
  print $ length content
  print content

main = do
  args <- getArgs
  mapM docDistance args
