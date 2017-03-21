head' :: [a] -> Maybe a

head' [] = Nothing
head' xs = Just $ head xs


main = (putStrLn . show) (head' ([] :: [()]))

