-- Define a binary tree data type
data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

-- 1. Map function for BinaryTree
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Node value left right) = Node (f value) (treeMap f left) (treeMap f right)

-- 2. Count elements in the BinaryTree
countElements :: BinaryTree a -> Int
countElements Empty = 0
countElements (Node _ left right) = 1 + countElements left + countElements right

-- 3. Depth-first traversal
treeTraverseD :: (a -> b -> b) -> b -> BinaryTree a -> b
treeTraverseD _ acc Empty = acc
treeTraverseD f acc (Node value left right) =
  let leftAcc = treeTraverseD f acc left
      rightAcc = treeTraverseD f leftAcc right
  in f value rightAcc

-- 4. Breadth-first traversal
treeTraverseW :: (a -> b -> b) -> b -> BinaryTree a -> b
treeTraverseW f acc tree = go [tree] acc
  where
    go [] acc = acc
    go (Empty:xs) acc = go xs acc
    go (Node value left right:xs) acc =
      let newAcc = f value acc
      in go (xs ++ [left, right]) newAcc

-- Example tree for testing
exampleTree :: BinaryTree Int
exampleTree = Node 1 (Node 2 Empty (Node 4 Empty Empty)) (Node 3 Empty Empty)

-- Test the functions
main :: IO ()
main = do
  putStrLn "Original tree:"
  print exampleTree

  putStrLn "Tree with elements doubled:"
  print $ treeMap (*2) exampleTree

  putStrLn "Count of elements in the tree:"
  print $ countElements exampleTree

  putStrLn "Depth-first traversal (sum of elements):"
  print $ treeTraverseD (+) 0 exampleTree

  putStrLn "Breadth-first traversal (sum of elements):"
  print $ treeTraverseW (+) 0 exampleTree