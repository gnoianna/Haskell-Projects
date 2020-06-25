-- define data type
data Tree a = EmptyTree | Node a (Tree a, Tree a) deriving (Show, Read, Eq)

-- create single node
treeNode :: a -> Tree a
treeNode item = Node item (EmptyTree, EmptyTree)

-- covert from/to list
treeFromList :: (Ord a) => [a] -> Tree a
treeFromList items = foldr treeInsert EmptyTree $ reverse items

treeToList :: Tree a -> [a]
treeToList EmptyTree = []
treeToList (Node item (left, right)) = (treeToList left) ++ [item] ++ (treeToList right)

-- access data
treeRoot :: Tree a -> a
treeRoot (Node item (_, _)) = item

treeLeftChild :: Tree a -> Tree a
treeLeftChild (Node _ (node, _)) = node

treeRightChild :: Tree a -> Tree a
treeRightChild (Node _ (_, node)) = node

treeHeight :: Tree a -> Int
treeHeight EmptyTree = 0
treeHeight (Node item (left, right)) = 1 + max (treeHeight left) (treeHeight right)

treeSize :: Tree a -> Int
treeSize EmptyTree = 0
treeSize (Node item (left, right)) = 1 + (treeSize left) + (treeSize right)

treeMin :: (Eq a) => Tree a -> a
treeMin (Node item (left, _)) =
    if left /= EmptyTree
        then treeMin left
        else item

treeMax :: (Eq a) => Tree a -> a
treeMax (Node item (_, right)) =
    if right /= EmptyTree
        then treeMax right
        else item

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem targetItem EmptyTree = False
treeElem targetItem (Node item (left, right))
    | targetItem < item = treeElem targetItem left
    | targetItem > item = treeElem targetItem right
    | otherwise = True

-- insert/delete tree node
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert newItem EmptyTree = treeNode newItem
treeInsert newItem (Node item (left, right)) =
    if newItem < item
        then Node item (treeInsert newItem left, right)
        else Node item (left, treeInsert newItem right)

treeDelete :: (Ord a) => a -> Tree a -> Tree a
treeDelete targetItem EmptyTree = EmptyTree
treeDelete targetItem (Node item (left, right))
    | targetItem < item = Node item (treeDelete targetItem left, right)
    | targetItem > item = Node item (left, treeDelete targetItem right)
    | left == EmptyTree = right
    | right == EmptyTree = left
    | otherwise = Node newItem (newLeft, right)
    where newItem = treeMax left
          newLeft = treeDelete newItem left

-- rotate tree
treeLeftRotate :: Tree a -> Tree a
treeLeftRotate (Node x (alpha, Node y (beta, gamma))) = Node y (Node x (alpha, beta), gamma)

treeRightRotate :: Tree a -> Tree a
treeRightRotate (Node y (Node x (alpha, beta), gamma)) = (Node x (alpha, Node y (beta, gamma)))