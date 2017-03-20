zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' f xs ys = map (applyTuple f) (zip xs ys)
  where applyTuple f (x, y) = f x y

main = (putStrLn . show) (zipWith' (+) [1,2,3] [23,1,4])
