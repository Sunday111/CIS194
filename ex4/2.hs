-- Each node stores an extra Integer representing the height at that node.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

tree_depth_after_insert_balanced :: Tree a -> Integer
tree_depth_after_insert_balanced Leaf = 0
tree_depth_after_insert_balanced (Node depth l _ r) =
    (minimum [tree_depth_after_insert_balanced l, tree_depth_after_insert_balanced r]) + 1

tree_elements_count :: Tree a -> Integer
tree_elements_count Leaf = 0
tree_elements_count (Node _ l _ r) = 1 + (tree_elements_count l) + (tree_elements_count r)

tree_depth :: Tree a -> Integer
tree_depth Leaf = -1
tree_depth (Node depth left v right) = depth

-- inserts node in three in balanced way
tree_insert_balanced :: Tree a -> a -> Tree a
tree_insert_balanced Leaf value = Node 0 Leaf value Leaf
tree_insert_balanced tree@(Node depth l v r) value
    | (tree_elements_count l) >= (tree_elements_count r) =
        let new_right = tree_insert_balanced r value
        in Node (((maximum [(tree_depth new_right), (tree_depth l)]))+1) l v (tree_insert_balanced r value)
    | otherwise = tree_insert_balanced (Node depth r v l) value

tree_is_valid :: Tree a -> Bool
tree_is_valid Leaf = True
tree_is_valid tree@(Node depth left v right) =
    (tree_is_valid left) && (tree_is_valid right) &&
    (tree_depth tree) == (maximum [tree_depth left, tree_depth right]) + 1

tree_is_balanced :: Tree a -> Bool
tree_is_balanced Leaf = True
tree_is_balanced tree@(Node depth left v right) =
    ((abs ((tree_depth left) - (tree_depth right))) < 2)

tree_from_list :: [a] -> Tree a
tree_from_list = foldl tree_insert_balanced Leaf

test_tree_depth_after_insert_balanced :: Bool
test_tree_depth_after_insert_balanced =
    ((tree_depth_after_insert_balanced Leaf) == 0) &&
    ((tree_depth_after_insert_balanced (Node 0 Leaf 10 Leaf)) == 1) &&
    ((tree_depth_after_insert_balanced (Node 1 (Node 0 Leaf 10 Leaf) 10 Leaf)) == 1) &&
    ((tree_depth_after_insert_balanced (Node 1 (Node 0 Leaf 10 Leaf) 10 (Node 0 Leaf 10 Leaf))) == 2)

test_tree_insert_balanced :: Bool
test_tree_insert_balanced =
    (let t = tree_insert_balanced Leaf 10 in (tree_is_valid t) && (tree_is_balanced t)) &&
    (let t = tree_insert_balanced (Node 0 Leaf 10 Leaf) 10 in (tree_is_valid t) && (tree_is_balanced t)) &&
    (let t = tree_insert_balanced (Node 1 (Node 0 Leaf 10 Leaf) 10 Leaf) 10 in (tree_is_valid t) && (tree_is_balanced t)) &&
    True

everything_is_fine :: Bool
everything_is_fine = test_tree_depth_after_insert_balanced && test_tree_insert_balanced