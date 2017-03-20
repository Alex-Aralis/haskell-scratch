zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' = \f -> \xs -> map (\(x, y) -> f x y) . zip xs

main = (putStrLn . show) (zipWith' (+) [1,2,3] [23,1,4])
