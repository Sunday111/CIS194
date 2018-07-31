import Data.List

isqrt = floor . sqrt . fromIntegral

make_sieve :: Integer -> [Integer]
make_sieve n =
    let max_i = ceiling ((sqrt((fromIntegral (2 * n + 1)) - 1)) / 2) in
    map (\(i, j) -> 2 * i * j + i + j)
    $ filter (\(i, j) -> j <= div (n - i) (2 * i + 1))
    [(i, j) | i <- [1..max_i], j <- [1..div (n - 1) 3]]

sieve_sundaram :: Integer -> [Integer]
sieve_sundaram n = map ((+1) . (*2)) ([1..n] \\ (make_sieve n))

isPrime k = null [ x | x <- [2..isqrt k], k `mod`x  == 0]
sieve_sundaram_test :: Bool
sieve_sundaram_test =
    let n = 10000
    in (filter isPrime [3..2 * n + 2]) == (sieve_sundaram n)