import Dbg (db)

lvl :: Int
lvl = 4

f :: (Show a) => a -> a
f x = x `db` lvl $ "f x:=" ++ show x

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: (Show a) => a -> Tree a
singleton x = Node x EmptyTree EmptyTree `db` lvl $ "singleton x:=" ++ show x

treeInsert :: (Ord a, Show a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x `db` lvl $ "create singleton x:=" ++ show x
treeInsert x (Node a left right)
        | x == a = Node x left right `db` lvl $ "replace x:=" ++ show x ++ " == a:=" ++ show a
        | x < a  = Node a (treeInsert x left) right `db` lvl $ "insert left x:=" ++ show x ++ " < a:=" ++ show a
        | x > a  = (Node a left (treeInsert x right)) `db` lvl $ "insert right x:=" ++ show x ++ " > a:=" ++ show a
