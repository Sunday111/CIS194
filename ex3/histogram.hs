import Data.List

entries_count :: Eq a => [a] -> [a] -> [Int]
entries_count entries dataset = map (\e -> length (elemIndices e dataset)) entries

make_hist_string_column :: Int -> Int -> Char
make_hist_string_column row count =
    if row <= count then '*' else ' '

make_hist_string_i :: Int -> [Int] -> [Char]
make_hist_string_i row counts = map (\count -> make_hist_string_column row count) counts

make_hist :: Eq a => [a] -> [a] -> [String]
make_hist e d = let counts = (entries_count e d)
             in let rows = reverse [1..(maximum counts)]
             in map (\row -> make_hist_string_i row counts) rows
