infixr 5 :-:
data Tree a = a :-: (Maybe (Tree a), Maybe (Tree a)) deriving (Show, Read, Eq)

type MTree a = Maybe (Tree a)

leaf :: a -> MTree a
leaf a = Just $ a:-:(Nothing,Nothing)

infixr 5 -:-
(-:-) :: (Ord a) => a -> MTree a -> MTree a
(-:-) v Nothing = leaf v
(-:-) v (Just node@(curr:-:(l,r))) = 
  Just $ case v `compare` curr of
    LT -> curr:-:(v-:-l,r)
    GT -> curr:-:(l,v-:-r)
    EQ -> node

treeElem :: (Ord a) => a -> MTree a -> Bool
treeElem v Nothing = False
treeElem v (Just (curr:-:(l,r))) =
  case v `compare` curr of 
    LT -> treeElem v l
    GT -> treeElem v r
    EQ -> True

-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f y x = f x y

fromList :: (Ord a) => [a] -> MTree a
fromList xs = foldl (flip (-:-)) Nothing xs

tree = fromList [2,4,1,5,2]

main = putStrLn . show $ tree 
