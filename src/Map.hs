module Map
    (Map,
    empty,
    insert,
    maplookup,
    toList
    ) where

-- Map data structure using binary search trees
data Map k a = Node k a (Map k a) (Map k a)
        | Empty deriving Show

-- Creates an empty Map
empty :: Map k a
empty = Empty

-- Inserts a key-value pair into a Map (if key already exists, value is updated)
insert :: Ord k => k -> a -> Map k a -> Map k a
insert key value Empty = Node key value (Empty) (Empty)
insert key value (Node k v left right)
    | key == k      = Node k value left right
    | key < k       = Node k v (insert key value left) right
    | otherwise     = Node k v left (insert key value right)

-- Looks up a key in a Map
maplookup :: Ord k => k -> Map k a -> Maybe a
maplookup _ Empty = Nothing
maplookup key (Node k v left right)
    | key == k  = Just v
    | key < k   = maplookup key left
    | otherwise = maplookup key right

-- Converts a Map to a list of key-value pairs
toList :: (Show k, Show a) => Map k a -> [(k, a)]
toList Empty = []
toList (Node k v left right) = toList left ++ [(k, v)] ++ toList right
