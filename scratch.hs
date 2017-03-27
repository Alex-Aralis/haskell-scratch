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


main = putStrLn . show $ 2-:-1-:-3-:-Nothing
