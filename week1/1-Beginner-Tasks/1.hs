import Data.Char

even' :: Integral a => a -> Bool
even' x = x `mod` 2 == 0

odd' :: Integral a => a -> Bool
odd' x = x `mod` 2 /= 0

bmi :: RealFloat a => a -> a -> a
bmi height weight = weight / (height ^ 2)

deg2Rad :: RealFloat a => a -> a
deg2Rad deg = deg * pi / 180

rad2Deg :: RealFloat a => a -> a
rad2Deg rad = rad * 180 / pi

isTriangle :: RealFloat a => a -> a -> a -> Bool
isTriangle a b c = a > 0 && b > 0 && c > 0 && a + b > c && b + c > a && a + c > b

perimeter :: RealFloat a => a -> a -> a -> a
perimeter a b c
    | not (isTriangle a b c) = error "This is not a triangle!"
    | otherwise = a + b + c

area :: RealFloat a => a -> a -> a -> a
area a b c
    | not (isTriangle a b c) = error "This is not a triangle!"
    | otherwise = sqrt (p * (p - a) * (p - b) * (p - c))
        where p = (perimeter a b c) / 2

calculate :: RealFloat a => Char -> a -> a -> a
calculate '+' a b = a + b
calculate '-' a b = a - b
calculate '*' a b = a * b
calculate '/' a b = a / b

convert :: RealFloat a => String -> String -> a -> a
convert "usd" "eur" sum = 0.882752 * sum
convert "usd" "bgn" sum = 1.72655 * sum
convert "eur" "usd" sum = 1.13268 * sum
convert "eur" "bgn" sum = 1.95589 * sum
convert "bgn" "usd" sum = 0.579191 * sum
convert "bgn" "eur" sum = 0.511265 * sum


isTriangleList :: RealFloat a => [a] -> Bool
isTriangleList [] = False
isTriangleList [a] = False
isTriangleList [a, b] = False
isTriangleList [a, b, c] = a > 0 && b > 0 && c > 0 && a + b > c && b + c > a && a + c > b

perimeterList :: RealFloat a => [a] -> a
perimeterList [] = 0
perimeterList (x : xs) = x + perimeterList xs

areaList :: RealFloat a => [a] -> a
areaList [a, b, c] = sqrt (p * (p - a) * (p - b) * (p - c))
    where p = (perimeter a b c) / 2


head' [] = error "Cannot get the head of an empty list!"
head' (x : _) = x

tail' [] = error "Cannot get the tail of an empty list!"
tail' [x] = []
tail' (_ : xs) = xs

last' [] = error "Cannot get the last element of an empty list!"
last' [x] = x
last' (_ : xs) = last' xs

double :: Num a => [a] -> [a]
double [] = []
double (x : xs) = 2 * x : double xs

mult :: Num a => a -> [a] -> [a]
mult n [] = []
mult n (x : xs) = n * x : mult n xs

nth :: Int -> [a] -> a
nth n [] = error "Cannot get the n-th element of an empty list!"
nth 0 (x : _) = x
nth n (_ : xs) = nth (n - 1) xs

member :: Eq a => a -> [a] -> Bool
member n [] = False
member n (x : xs)
    | n == x = True
    | otherwise = member n xs

isFib :: Integral a => [a] -> Bool
isFib [] = False
isFib [a] = False
isFib [a, b] = True
isFib (a : b : c : xs) = a + b == c && isFib (b : c : xs)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

multLists :: Num a => [a] -> [a] -> [a]
multLists [] [] = []
multLists l [] = []
multLists [] l = []
multLists (x : xs) (y : ys) = x * y : multLists xs ys

number2string :: Int -> String
number2string 0 = "0"
number2string n
    | n < 0 = "-" ++ number2string (-n)
    | n >= 1 && n <= 9 = [charDigit n]
    | otherwise = number2string (n `quot` 10) ++ [charDigit (n `mod` 10)]
        where charDigit n = chr (ord '0' + n)

string2number :: String -> Int
string2number "" = 0
string2number ('-' : cs) = -(string2number cs)
string2number s = digitChar (last s) + 10 * (string2number (init s))
    where digitChar c = ord c - ord '0'
