list_reverse [] = []
list_reverse (x:xs) = list_reverse xs ++ [x]

num_digits :: Integer -> Integer
num_digits x
    | x < 10 = 1
    | otherwise = 1 + num_digits (div x 10)

get_digit :: Integer -> Integer -> Integer
get_digit value digit_index =
    let k = (10 ^ digit_index)
    in mod (div value k) 10

get_digits_i :: Integer -> Integer -> [Integer]
get_digits_i _ (-1) = []
get_digits_i x i = (get_digit x i) : (get_digits_i x (i - 1))

get_digits :: Integer -> [Integer]
get_digits x
    | x < 10    = [x]
    | otherwise =
        let k = (num_digits x) - 1
        in get_digits_i x k

list_sum [] = 0
list_sum (head:tail) = head + (list_sum tail)

sum_digits :: Integer -> Integer
sum_digits numbers = list_sum (get_digits numbers)

sum_digits_list numbers = list_sum (map (sum_digits) numbers)

muL_if :: Bool -> Integer -> Integer -> Integer
muL_if True a b = a * b
muL_if False a b = a

double_every_other_i :: Bool -> [Integer] -> [Integer]
double_every_other_i _ [] = []
double_every_other_i start_from_head (head:tail) =
    (muL_if start_from_head head 2) : double_every_other_i (not start_from_head) tail

double_every_other list = double_every_other_i False list

validate :: Integer -> Bool
validate card_number = (mod (sum_digits_list (double_every_other (list_reverse (get_digits card_number)))) 10) == 0