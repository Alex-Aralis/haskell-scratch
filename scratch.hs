zip' :: [a] -> [b] -> [(a,b)]

zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

main = (putStrLn . show) (zip' [1,2,3,4,5] ['a', 'b', 'c', 'd'])
