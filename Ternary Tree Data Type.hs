data TernaryTree a = EmptyNode 
              | Leaf a
              | FilledNode a (TernaryTree a) (TernaryTree a) (TernaryTree a)
    deriving (Show)

--a = Empty
--b = Branch 42 Empty Empty 
--c = Branch 50 b b 
--treeLook 4 b returns False 
--treeLook 42 b returns True
--treeLook 50 c returns True
-- d = FilledNode 50 b b b
-- isEle 42 d returns true
treeLook :: (Eq a) => a -> TernaryTree a -> Bool
treeLook a EmptyNode = False
--treeLook a (FilledNode c left middle right)
  --  | a == c    = True
    -- | otherwise = (isEle a left || isEle a middle) || isEle a right
treeLook a (FilledNode c left middle right) = a == c || treeLook a left || treeLook a middle || treeLook a right

--collapse (FilledNode 2 (Leaf 4) (Leaf 3) (Leaf 5))
collapse :: (Ord a) => TernaryTree a -> [a]
collapse (Leaf v) = [v]
collapse (FilledNode root left middle right) = [root] ++ collapse left ++ collapse middle ++ collapse right 

treeInsert :: (Ord a) => a -> TernaryTree a -> TernaryTree a
--treeInsert 4 EmptyNode
-- returns Leaf 4
treeInsert x EmptyNode = Leaf x
treeInsert x (FilledNode a left middle right)
   | x == a = FilledNode a left (treeInsert x middle) right
   | x < a  = FilledNode a (treeInsert x left) middle right
   | x > a  = FilledNode a left middle (treeInsert x right)

-- treeInsert 4 (FilledNode 4 (Leaf 3) (EmptyNode) (Leaf 5))
-- returns FilledNode 4 (Leaf 3) (Leaf 4) (Leaf 5)
-- treeInsert 4 (FilledNode 2 (Leaf 1)(Leaf 2)EmptyNode)
--returns FilledNode 2 (Leaf 1) (Leaf 2) (Leaf 4)

instance Eq a => Eq (TernaryTree a) where
    EmptyNode == EmptyNode = True
    EmptyNode == _     = False
    _ == EmptyNode     = False
    FilledNode a aleft amiddle aright == FilledNode b bl bm br = a == b && aleft == bl && amiddle == bm && aright == br 
