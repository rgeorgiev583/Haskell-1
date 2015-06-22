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
    | otherwise              = a + b + c

area :: RealFloat a => a -> a -> a -> a
area a b c
    | not (isTriangle a b c) = error "This is not a triangle!"
    | otherwise              = sqrt (p * (p - a) * (p - b) * (p - c))
        where p = (perimeter a b c) / 2

calculate :: RealFloat a => Char -> a -> a -> a
calculate '+' a b = a + b
calculate '-' a b = a - b
calculate '*' a b = a * b
calculate '/' a b = a / b
calculate  _  _ _ = error "Unknown operation!"

convert :: RealFloat a => String -> String -> a -> a
convert "usd" "eur" sum = 0.882752 * sum
convert "usd" "bgn" sum = 1.72655 * sum
convert "eur" "usd" sum = 1.13268 * sum
convert "eur" "bgn" sum = 1.95589 * sum
convert "bgn" "usd" sum = 0.579191 * sum
convert "bgn" "eur" sum = 0.511265 * sum
convert   _     _    _  = error "Unknown currency!"


isTriangleList :: RealFloat a => [a] -> Bool
isTriangleList []        = False
isTriangleList [a]       = False
isTriangleList [a, b]    = False
isTriangleList [a, b, c] = a > 0 && b > 0 && c > 0 && a + b > c && b + c > a && a + c > b
isTriangleList     _     = False

perimeterList :: RealFloat a => [a] -> a
perimeterList []       = 0
perimeterList (x : xs) = x + perimeterList xs

areaList :: RealFloat a => [a] -> a
areaList [a, b, c] = sqrt (p * (p - a) * (p - b) * (p - c))
    where p = (perimeter a b c) / 2
areaList     _     = error "Unknown polygon type!"


head' :: [a] -> a
head' []      = error "Cannot get the head of an empty list!"
head' (x : _) = x

tail' :: [a] -> [a]
tail' []       = error "Cannot get the tail of an empty list!"
tail' (_ : xs) = xs


last' :: [a] -> a
last' []       = error "Cannot get the last element of an empty list!"
last' [x]      = x
last' (_ : xs) = last' xs

double :: Num a => [a] -> [a]
double []       = []
double (x : xs) = 2 * x : double xs

mult :: Num a => a -> [a] -> [a]
mult _ []       = []
mult n (x : xs) = n * x : mult n xs

nth :: Int -> [a] -> a
nth _ []       = error "Cannot get the n-th element of an empty list!"
nth 0 (x : _)  = x
nth n (_ : xs) = nth (n - 1) xs

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member n (x : xs)
    | n == x    = True
    | otherwise = member n xs

isFib :: Integral a => [a] -> Bool
isFib []               = False
isFib [_]              = False
isFib [_, _]           = True
isFib (x : y : z : xs) = x + y == z && isFib (y : z : xs)

sum' :: Num a => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

product' :: Num a => [a] -> a
product' []       = 1
product' (x : xs) = x * product' xs

multLists :: Num a => [a] -> [a] -> [a]
multLists _        []       = []
multLists []       _        = []
multLists (x : xs) (y : ys) = x * y : multLists xs ys


number2string :: Int -> String
number2string 0 = "0"
number2string n
    | n < 0            = "-" ++ number2string (-n)
    | n >= 1 && n <= 9 = [charDigit n]
    | otherwise        = number2string (n `quot` 10) ++ [charDigit (n `mod` 10)]
        where charDigit n = chr (ord '0' + n)

string2number :: String -> Int
string2number ""         = 0
string2number ('-' : cs) = -(string2number cs)
string2number s          = digitChar (last s) + 10 * string2number (init s)
    where digitChar c = ord c - ord '0'


isValidId :: String -> Bool
isValidId [y1, y2, m1, m2, d1, d2, r1, r2, r3, cd] = digitChar cd == ckdigit &&
        (d >=  1 && d <= 28 && m >= 1 && m <= 12 ||
         d == 29 && (m == 1 || m >= 3 && m <= 12 || m ==  2  && isLeapYear) ||
         d == 30 && (m == 4 || m == 6 || m ==  9 || m == 11) ||
         d == 31 && (m == 1 || m == 3 || m ==  5 || m ==  7  || m == 8      || m == 10 || m == 12))
    where
        digitChar c = ord c - ord '0'
        cksum = (2  * digitChar y1 + 4 * digitChar y2 +
                 8  * digitChar m1 + 5 * digitChar m2 +
                 10 * digitChar d1 + 9 * digitChar d2 +
                 7  * digitChar r1 + 3 * digitChar r2 + 6 * digitChar r3) `mod` 11
        ckdigit = if cksum < 10 then cksum else 0
        yr = digitChar y1 * 10 + digitChar y2
        mr = digitChar m1 * 10 + digitChar m2
        y
            | mr >= 20 && mr <= 32 = 1800 + yr
            | mr >= 40 && mr <= 42 = 2000 + yr
            | otherwise = 1900 + yr
        m
            | mr >= 20 && mr <= 32 = mr - 20
            | mr >= 40 && mr <= 42 = mr - 40
            | otherwise = mr
        d = digitChar d1 * 10 + digitChar d2
        isLeapYear = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)
isValidId _ = False

whatZodiacSignIs :: String -> String
whatZodiacSignIs [_, _, m1, m2, d1, d2, _, _, _, _]
    | d >= 21 && d <= 31 && m ==  3 || d >= 1 && d <= 20 && m ==  4 = "Aries"
    | d >= 21 && d <= 30 && m ==  4 || d >= 1 && d <= 21 && m ==  5 = "Taurus"
    | d >= 22 && d <= 31 && m ==  5 || d >= 1 && d <= 21 && m ==  6 = "Gemini"
    | d >= 22 && d <= 30 && m ==  6 || d >= 1 && d <= 22 && m ==  7 = "Cancer"
    | d >= 23 && d <= 31 && m ==  7 || d >= 1 && d <= 22 && m ==  8 = "Leo"
    | d >= 23 && d <= 31 && m ==  8 || d >= 1 && d <= 23 && m ==  9 = "Virgo"
    | d >= 24 && d <= 30 && m ==  9 || d >= 1 && d <= 23 && m == 10 = "Libra"
    | d >= 24 && d <= 31 && m == 10 || d >= 1 && d <= 22 && m == 11 = "Scorpio"
    | d >= 23 && d <= 30 && m == 11 || d >= 1 && d <= 21 && m == 12 = "Sagittarius"
    | d >= 22 && d <= 31 && m == 12 || d >= 1 && d <= 20 && m ==  1 = "Capricorn"
    | d >= 21 && d <= 31 && m ==  1 || d >= 1 && d <= 19 && m ==  2 = "Aquarius"
    | d >= 20 && d <= 29 && m ==  2 || d >= 1 && d <= 20 && m ==  3 = "Pisces"
    | otherwise = error "Invalid date: there is no such month/day number!"
        where
            digitChar c = ord c - ord '0'
            mr = digitChar m1 * 10 + digitChar m2
            m
                | mr >= 20 && mr <= 32 = mr - 20
                | mr >= 40 && mr <= 42 = mr - 40
                | otherwise = mr
            d = digitChar d1 * 10 + digitChar d2
whatZodiacSignIs _ = error "Invalid string format: there is no such personal ID number!"


concatenateLists :: [a] -> [a] -> [a]
concatenateLists []       l = l
concatenateLists (x : xs) l = x : concatenateLists xs l

init' :: [a] -> [a]
init' []       = error "Cannot get the init of an empty list!"
init' [_]      = []
init' (x : xs) = x : init' xs

take' :: Int -> [a] -> [a]
take' _ []       = []
take' 0 _        = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []       = []
drop' 0 l        = l
drop' n (_ : xs) = drop' (n - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _        []       = []
zip' []       _        = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' l = (unzipLeft l, unzipRight l)
    where
        unzipLeft  []            = []
        unzipLeft  ((x, _) : xs) = x : unzipLeft xs
        unzipRight []            = []
        unzipRight ((_, x) : xs) = x : unzipRight xs

-- TODO: Add support for returning a flat list (do not know how to return more than one type.)
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' l  = take erl l : group' (drop erl l)
    where
        eqRunLen []  = 0
        eqRunLen [_] = 1
        eqRunLen (x : y : xs)
            | x /= y    = 1
            | otherwise = 1 + eqRunLen (y : xs)
        erl = eqRunLen l


pyths :: Integer -> Integer -> [(Integer, Integer, Integer)]
pyths from to
    | truncate (sqrt (fromInteger (from ^ 2 + (from + 1) ^ 2))) > to || from == to = []
    | otherwise = helper from (from + 1) ++ pyths (from + 1) to
        where
            helper a b
                | c > to    = []
                | isPyth    = (a, b, c) : helper a (b + 1)
                | otherwise = helper a (b + 1)
                    where
                        sqrtSum = sqrt (fromInteger (a ^ 2 + b ^ 2))
                        c       = truncate sqrtSum
                        isPyth  = fromInteger c == sqrtSum


multiplyBy :: Num a => a -> (a -> a)
multiplyBy n = (* n)


lastDigits :: Integral a => [a] -> [a]
lastDigits []       = []
lastDigits (x : xs) = x `mod` 10 : lastDigits xs

stringsToIntegers :: [String] -> [Int]
stringsToIntegers []       = []
stringsToIntegers (x : xs) = string2number x : stringsToIntegers xs

stringsToIntegers' :: [String] -> [Int]
stringsToIntegers' []       = []
stringsToIntegers' (x : xs) = (read x :: Int) : stringsToIntegers' xs

fibonaccis :: Integral a => [a] -> [a]
fibonaccis []       = []
fibonaccis (x : xs) = fib x 0 1 : fibonaccis xs
    where
        fib n a b
            | n == 0    = a
            | n == 1    = b
            | otherwise = fib (n - 1) b (a + b)


applyToAll :: (a -> a) -> [a] -> [a]
applyToAll f []       = []
applyToAll f (x : xs) = f x : applyToAll f xs
