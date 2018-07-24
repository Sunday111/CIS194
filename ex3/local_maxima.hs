local_maxima :: (Ord a) => [a] -> [a]
local_maxima (x1:x2:x3:xs)
    | (x2 > x1) && (x2 > x3) = x2 : (local_maxima xs)
    | otherwise = local_maxima (x2 : x3 : xs)
local_maxima _ = []