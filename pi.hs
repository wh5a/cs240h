import System.Random

guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z, g'')
    where z        = x^2 + y^2
          (x, g')  = random g
          (y, g'') = random g'

withGen :: (StdGen -> a) -> IO a
withGen f = do
  g <- getStdGen
  let (g',g'') = split g
  setStdGen g'
  return (f g'')

pie = do
    g <- getStdGen
    let randoms' = tail $ take 300000 $ iterate guess (undefined, g)
        randoms = map fst randoms'
        n = length randoms
        count = length $ filter (<=1) randoms
    setStdGen $ snd $ last randoms'
    return (4 * (fromIntegral count) / fromIntegral n)
