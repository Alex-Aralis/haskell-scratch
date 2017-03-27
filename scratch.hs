data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


leaf :: a -> Tree a
leaf a = Node a EmptyTree EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert value EmptyTree = leaf value
insert value node@(Node curr l r) = 
  case value `compare` curr of
    LT -> Node curr (insert value l) r
    GT -> Node curr l (insert value r)
    EQ -> node

main = putStrLn . show $ insert 1 $ insert 2 $ insert 3 EmptyTree
