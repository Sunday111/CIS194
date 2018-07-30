import Data.List

make_sieve :: Int -> [Int]
make_sieve n =
    let max_i = ceiling ((sqrt((fromIntegral (n + 1)))) / 2) :: Int in
    let max_j = (\i -> div (n - 1) (2 * i + 1)) in
    map (\(i, j) -> 2 * i * j + i + j)
    (filter (\(i, j) -> (i <= max_i) && (j <= max_j i))
    [(i, j) | i <- [1..n], j <- [1..n]])

sieveSundaram :: Int -> [Int]
sieveSundaram n = map ((+1) . (*2)) ([1..n] \\ (make_sieve n))