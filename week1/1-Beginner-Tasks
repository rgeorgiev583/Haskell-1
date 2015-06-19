even' x = x `mod` 2 == 0

odd' x = x `mod` 2 /= 0

bmi height weight = weight / (height ^ 2)

deg2Rad deg = deg * pi / 180

rad2Deg rad = rad * 180 / pi

isTriangle a b c = a > 0 && b > 0 && c > 0 && a + b > c && b + c > a && a + c > b

perimeter a b c
    | not (isTriangle a b c) = error "This is not a triangle!"
    | otherwise = a + b + c

area a b c
    | not (isTriangle a b c) = error "This is not a triangle!"
    | otherwise = sqrt (p * (p - a) * (p - b) * (p - c))
        where p = (perimeter a b c) / 2

calculate '+' a b = a + b
calculate '-' a b = a - b
calculate '*' a b = a * b
calculate '/' a b = a / b

convert "usd" "eur" sum = 0.882752 * sum
convert "usd" "bgn" sum = 1.72655 * sum
convert "eur" "usd" sum = 1.13268 * sum
convert "eur" "bgn" sum = 1.95589 * sum
convert "bgn" "usd" sum = 0.579191 * sum
convert "bgn" "eur" sum = 0.511265 * sum
