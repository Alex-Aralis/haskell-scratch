zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)
zipWith' _ [] _ = []
zipWith' _ _ [] = []

main = (putStrLn . show) (zipWith' (+) [1,2,3] [23,1,4])
