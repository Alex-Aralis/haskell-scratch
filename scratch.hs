data Point = Point Float Float deriving (Show)

data Shape = Triangle Point Point Point | Polygon [Point] deriving (Show)

dot :: Point -> Point -> Float
(Point ax ay) `dot` (Point bx by) = ax*bx + ay*by

negate' :: Point -> Point
negate' Point x y = Point (-x) (-y)

add' :: Point -> Point -> Point
(Point ax ay) `add'` (Point bx by) = Point (ax + bx) (ay + by)

subtract' :: Point -> Point -> Point
a `subtract'` b = a `add'` negate' b

data Ray = Ray Point Point deriving (Show)

rot90 :: Point -> Point
rot90 (Point x y) = Point (-y) x

edge :: Point -> Ray -> Float
edge P (Ray S n) = n `dot` (P `subtract'` S)

rayFromPoints :: Point -> Point -> Ray
rayFromPoints S P = Ray S $ rot90 (P `subtract'` S)

pointEdgeComp :: Point -> Point -> Point -> Ordering
pointEdgeComp P p1 p2 = 
  let R = rayFromPoints p1 p2
  in (edge P R) `compare` 0

wrapList :: [a] -> [a]
wrapList xs = xs ++ head xs

find2 :: (a -> a -> Bool) -> [a] -> Maybe a
find2 _ [] = Nothing
find2 _ [x] = Nothing
find2 comp (x1:x2:xs) 
  | comp x1 x2 = x2:xs
  | otherwise = find2 comp (x2:xs)

isPointInPoly :: Polygon -> Point ->
isPointInPoly Polygon points P 
  | found == Nothing = True
  | otherwise = False
  where wrappedPoints = wrapList points
        comp p1 p2
          | (pointEdgeComp P p1 p2) == GT = False
          | otherwise = True
        found = find2 comp wrappedPoints

dropWhile3 :: (a -> a -> a -> Bool) -> [a] -> [a]
dropWhile3 comp (x1:x2:x3:xs) 
  | comp x1 x2 x3 = dropWhile2 comp (x2:x3:xs)
  | otherwise = (x2:x3:xs)

takeWhile3 :: (a -> a -> a -> Bool) -> [a] -> [a]
takeWhile3 comp (x1:x2:x3:xs) 
  | comp x1 x2 x3 = x1:(takeWhile2 comp (x2:x3:xs))
  | otherwise = [x1]

findUpEdge :: Polygon -> Point -> Polygon
findUpEdge (Polygon path) point = 
  Polygon $ take len $ dropWhile2 isUpEdge cPath 
  where e S P = edge point $ rayFromPoints S P
        isUpEdge p1 p2 p3 = (e p1 p2) <= 0 && (e p2 p3) > 0
        len = length path
        cPath = cycle path

takePosEdges :: Polygon -> Point -> [Point]
takePosEdges (Polygon path) point = 
  takeWhile2 isDownEdge path
  where e S P = edge point $ rayFromPoints S P
        isDownEdge p1 p2 p3 = (e p1 p2) > 0 && (e p2 p3) <= 0
    
mlength :: (Interal a) => a -> [b] -> Bool
mlength min []
  | min <= 0 = True
  | otherwise = False
mlength min (x:xs)
  | min <= 0 = True
  | otherwise = mlength (min-1) xs 
    
addPointToConvexPoly :: Polygon -> Point -> Polygon
addPointToConvexPoly poly@(Polygon points) P
  | 
  | isPointInPoly poly P = poly
  | let upPoly = findUpEdge poly P
        posPart = takePosEdges upPoly P

convexPolyFromPoints :: [Point] -> Polygon



main = putStrLn . show $ Polygon [(Point 1 1), (Point 2 2), (Point 3 3)]

