zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' f xs ys = [ f x y | (x, y) <- zip xs ys ]

main = (putStrLn . show) (zipWith' (+) [1,2,3] [23,1,4])
