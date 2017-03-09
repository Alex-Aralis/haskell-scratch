qs :: Ord a => [a] -> [a]

qs [] = []
qs (x:xs) = 
  let split [] n = ([], [])
      split (y:ys) n
        | y <= n    = (y:lt, gt)
        | otherwise = (lt, y:gt)
        where (lt, gt) = split ys n
      (lt, gt) = split xs x
  in (qs lt) ++ [x] ++ (qs gt)

main = (putStrLn . show) (qs [3, 1, 5, 34, 2312, 1, 9])
