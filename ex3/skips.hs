split_offset :: Int -> [a] -> [[a]]
split_offset _ [] = []
split_offset n l = let (h, t) = (splitAt n l)
                   in [h] ++ (split_offset n t)

length_equals_to :: Int -> [a] -> Bool
length_equals_to n l = (length l) == n

extract_series :: Int -> [a] -> [[a]]
extract_series n l = filter (length_equals_to n) (split_offset n l)

skips_i :: Int -> [a] -> [[a]]
skips_i i l = let k = map head (map reverse (extract_series i l))
              in
                case k of
                    [] -> []
                    w -> [w] ++ (skips_i (i + 1) l)

skips :: [a] -> [[a]]
skips l = skips_i 1 l
