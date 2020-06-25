import qualified Data.Map

data Tree a = ET | Node a (Tree a, Tree a) deriving (Show, Read, Eq)

treeNode :: a -> Tree a
treeNode item = Node item (ET, ET)

--insert function--
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert newItem ET = treeNode newItem
treeInsert newItem (Node item (left, right)) =
    if newItem < item
        then Node item (treeInsert newItem left, right)
        else Node item (left, treeInsert newItem right)

--empty function--
treeEmpty :: (Ord a) => Tree a -> Bool
treeEmpty tree =
    if tree == ET
        then True
        else False

--isBinary function--
treeIsBinary :: (Ord a) => Tree a -> Bool 
treeIsBinary ET = True
treeIsBinary (Node item (left, right)) = fun (item >) left && fun (item <=) right && treeIsBinary left && treeIsBinary right
    where
        fun _ ET = True
        fun x (Node i (l, r)) = x i && fun x l && fun x r

--search function--
treeSearch :: (Ord a) => a -> Tree a -> Bool
treeSearch targetItem ET = False
treeSearch targetItem (Node item (left, right))
    | targetItem < item = treeSearch targetItem left
    | targetItem > item = treeSearch targetItem right
    | otherwise = True

--isBalanced--
treeIsBalanced :: (Ord a) => Tree a -> Bool
treeIsBalanced ET = True
treeIsBalanced (Node item(left, right)) = (treeHeight left == treeHeight right ||
                                    (treeHeight left) + 1 == treeHeight right ||
                                    treeHeight left == (treeHeight right) + 1) &&
                                    treeIsBalanced left && treeIsBalanced right

treeHeight :: Tree a -> Int
treeHeight ET = 0
treeHeight (Node item (left, right)) = 1 + max (treeHeight left) (treeHeight right)

--traverse functions--
treeInorder :: (Ord a) => Tree a -> [a]
treeInorder ET = []
treeInorder (Node item(left, right)) = treeInorder left ++ [item] ++ treeInorder right

treePreorder :: (Ord a) => Tree a -> [a]
treePreorder ET = []
treePreorder (Node item(left, right)) = [item] ++ treePreorder left ++ treePreorder right

treePostorder :: (Ord a) => Tree a -> [a]
treePostorder ET = []
treePostorder (Node item(left, right)) = treePostorder left ++ treePostorder right ++ [item]

treeOrder_VRL :: (Ord a) => Tree a -> [a]
treeOrder_VRL ET = []
treeOrder_VRL (Node item(left, right)) = [item] ++ treePostorder right ++ treePostorder left

treeOrder_RVL :: (Ord a) => Tree a -> [a]
treeOrder_RVL ET = []
treeOrder_RVL (Node item(left, right)) = treePostorder right ++ [item] ++ treePostorder left

treeOrder_RLV :: (Ord a) => Tree a -> [a]
treeOrder_RLV ET = []
treeOrder_RLV (Node item(left, right)) = treePostorder right ++ treePostorder left ++ [item]

--toString function--
toString :: (Ord a) => [a] -> Tree a
toString list = (treeFromList list)

--treeLeaves--
treeLeaves :: (Ord a) => Tree a -> [a]
treeLeaves tree = case tree of
    ET                              -> []
    (Node item(ET,ET))              -> item:[]
    (Node ite(left, ET))            -> treeLeaves left
    (Node it(ET, right))            -> treeLeaves right
    (Node i(l, r))                  -> treeLeaves l ++ treeLeaves r

--nnodes function--
treenNodes :: Tree a -> Int
treenNodes ET = 0
treenNodes (Node item (left, right)) = 1 + (treenNodes left) + (treenNodes right)

--nsum function--
treenSum :: Num a => Tree a -> a
treenSum ET = 0
treenSum (Node item (left, right)) = item + (treenSum left) + (treenSum right)

--tmap function--
treeMap x tree = treeFromList(map x (treeToList tree)) 

--remove function--
treeDelete :: (Ord a) => a -> Tree a -> Tree a
treeDelete targetItem ET = ET
treeDelete targetItem (Node item (left, right))
    | targetItem < item = Node item (treeDelete targetItem left, right)
    | targetItem > item = Node item (left, treeDelete targetItem right)
    | left == ET = right
    | right == ET = left
    | otherwise = Node newItem (newLeft, right)
    where newItem = treeMax left
          newLeft = treeDelete newItem left

treeMin :: (Eq a) => Tree a -> a
treeMin (Node item (left, _)) =
    if left /= ET
        then treeMin left
        else item

treeMax :: (Eq a) => Tree a -> a
treeMax (Node item (_, right)) =
    if right /= ET
        then treeMax right
        else item

--merge function--
treeMerge :: (Ord a) => Tree a -> Tree a -> Tree a
treeMerge tree1 tree2 = treeFromList(treeToList tree1 ++ treeToList tree2)

treeMerge2 :: (Ord a) => Tree a -> Tree a -> Tree a
treeMerge2 tree1 tree2 = foldl (treeInsertReverse) tree1 (treeToList tree2)

--additional functions--
treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldl treeInsertReverse ET

treeToList :: Tree a -> [a]
treeToList ET = []
treeToList (Node item (left, right)) = (treeToList left) ++ [item] ++ (treeToList right)

treeInsertReverse :: (Ord a) => Tree a -> a -> Tree a
treeInsertReverse x y = treeInsert y x