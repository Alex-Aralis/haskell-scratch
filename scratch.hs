infixr 5 :-:
data Tree a = EmptyTree 
  | a :-: (Tree a, Tree a) 
  deriving (Read, Eq)

treePrint :: (Show a) => String -> String -> Tree a -> String
treePrint _ _ EmptyTree = ""

treePrint lpad rpad (c:-:(l,r)) = 
  (treePrint llpad lrpad l) ++ line ++ (treePrint rlpad rrpad r) 
  where cStr = "{ " ++ (show c) ++ " }"
        cLen = length cStr
        spacer = replicate cLen ' '
        llpad = lpad ++ spacer ++ " "
        lrpad = lpad ++ spacer ++ "|"
        rlpad = rpad ++ spacer ++ "|"
        rrpad = rpad ++ spacer ++ " "
        line = (take ((length rpad) - 1) rpad) ++ "|" ++ cStr ++ "|\n"
         
instance (Show a) => Show (Tree a) where
  show EmptyTree = "()"
  show tree = treePrint "" "" tree

leaf :: a -> Tree a
leaf a = a:-:(EmptyTree,EmptyTree)

infixr 5 -:-
(-:-) :: (Ord a) => a -> Tree a -> Tree a
(-:-) v EmptyTree = leaf v
(-:-) v node@(c:-:(l,r)) = 
  case v `compare` c of
    LT -> c:-:(v-:-l,r)
    GT -> c:-:(l,v-:-r)
    EQ -> node

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem v EmptyTree = False
treeElem v (c:-:(l,r)) =
  case v `compare` c of 
    LT -> treeElem v l
    GT -> treeElem v r
    EQ -> True


fromList :: (Ord a) => [a] -> Tree a
fromList xs = foldl (flip (-:-)) EmptyTree xs

tree = fromList [2,4,1,5,2,-1 ,334,144,234,3, 9, 7,5,12,34]

main = putStrLn . show $ tree 
