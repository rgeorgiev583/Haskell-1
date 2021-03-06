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
    | isTriangle a b c = a + b + c
    | otherwise        = error "This is not a triangle!"

area :: RealFloat a => a -> a -> a -> a
area a b c
    | isTriangle a b c = sqrt (p * (p - a) * (p - b) * (p - c))
    | otherwise        = error "This is not a triangle!"
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
isTriangleList [a, b, c] = a > 0 && b > 0 && c > 0 && a + b > c && b + c > a && a + c > b
isTriangleList     _     = False

perimeterList :: RealFloat a => [a] -> a
perimeterList (x : xs) = x + perimeterList xs
perimeterList    _     = 0

areaList :: RealFloat a => [a] -> a
areaList [a, b, c] = sqrt (p * (p - a) * (p - b) * (p - c))
    where p = (perimeter a b c) / 2
areaList    _      = error "Unknown polygon type!"


head' :: [a] -> a
head' (x : _) = x
head'    _    = error "Cannot get the head of an empty list!"

tail' :: [a] -> [a]
tail' (_ : xs) = xs
tail'    _     = error "Cannot get the tail of an empty list!"


last' :: [a] -> a
last' [x]      = x
last' (_ : xs) = last' xs
last'    _     = error "Cannot get the last element of an empty list!"

double :: Num a => [a] -> [a]
double (x : xs) = 2 * x : double xs
double    _     = []

mult :: Num a => a -> [a] -> [a]
mult n (x : xs) = n * x : mult n xs
mult _    _     = []

nth :: Int -> [a] -> a
nth 0 (x : _)  = x
nth n (_ : xs) = nth (n - 1) xs
nth _    _     = error "Cannot get the n-th element of an empty list!"

member :: Eq a => a -> [a] -> Bool
member n (x : xs) = n == x || member n xs
member _    _     = False

isFib :: Integral a => [a] -> Bool
isFib (x : y : z : xs) = x + y == z && isFib (y : z : xs)
isFib [_, _]           = True
isFib   _              = False

sum' :: Num a => [a] -> a
sum' (x : xs) = x + sum' xs
sum'    _     = 0

product' :: Num a => [a] -> a
product' (x : xs) = x * product' xs
product'    _     = 1

multLists :: Num a => [a] -> [a] -> [a]
multLists (x : xs) (y : ys) = x * y : multLists xs ys
multLists    _        _     = []


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
string2number s          = digitChar (last' s) + 10 * string2number (init' s)
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
concatenateLists (x : xs) l = x : concatenateLists xs l
concatenateLists    _     l = l

init' :: [a] -> [a]
init' [_]      = []
init' (x : xs) = x : init' xs
init'    _     = error "Cannot get the init of an empty list!"

take' :: Int -> [a] -> [a]
take' 0 _        = []
take' n (x : xs) = x : take' (n - 1) xs
take' _    _     = []

drop' :: Int -> [a] -> [a]
drop' 0 l        = l
drop' n (_ : xs) = drop' (n - 1) xs
drop' _    _     = []

zip' :: [a] -> [b] -> [(a, b)]
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
zip'    _        _     = []

unzip' :: [(a, b)] -> ([a], [b])
unzip' l = (unzipLeft l, unzipRight l)
    where
        unzipLeft  ((x, _) : xs) = x : unzipLeft xs
        unzipLeft          _     = []
        unzipRight ((_, x) : xs) = x : unzipRight xs
        unzipRight         _     = []

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' l  = take' erl l : group' (drop' erl l)
    where
        eqRunLen [_] = 1
        eqRunLen (x : y : xs)
            | x /= y    = 1
            | otherwise = 1 + eqRunLen (y : xs)
        eqRunLen  _  = 0
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
lastDigits (x : xs) = x `mod` 10 : lastDigits xs
lastDigits    _     = []

stringsToIntegers :: [String] -> [Int]
stringsToIntegers (x : xs) = string2number x : stringsToIntegers xs
stringsToIntegers    _     = []

stringsToIntegers' :: [String] -> [Int]
stringsToIntegers' (x : xs) = (read x :: Int) : stringsToIntegers' xs
stringsToIntegers'    _     = []

fibonaccis :: Integral a => [a] -> [a]
fibonaccis (x : xs) = fib x 0 1 : fibonaccis xs
    where
        fib n a b
            | n == 0    = a
            | n == 1    = b
            | otherwise = fib (n - 1) b (a + b)
fibonaccis    _     = []


applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f (x : xs) = f x : applyToAll f xs
applyToAll f    _     = []

lastDigitsUsingMap :: Integral a => [a] -> [a]
lastDigitsUsingMap l = applyToAll (`mod` 10) l

stringsToIntegersUsingMap :: [String] -> [Int]
stringsToIntegersUsingMap l = applyToAll string2number l

stringsToIntegersUsingMap' :: [String] -> [Int]
stringsToIntegersUsingMap' l = applyToAll (\x -> read x :: Int) l

fibonaccisUsingMap :: Integral a => [a] -> [a]
fibonaccisUsingMap l = applyToAll (\x -> fib x 0 1) l
    where
        fib n a b
            | n == 0    = a
            | n == 1    = b
            | otherwise = fib (n - 1) b (a + b)


odds :: Integral a => [a] -> [a]
odds (x : xs)
    | odd' x     = x : odds xs
    | otherwise  =     odds xs
odds    _ = []

divisibles :: Integral a => a -> [a] -> [a]
divisibles n (x : xs)
    | x `mod` n == 0 = x : divisibles n xs
    | otherwise      =     divisibles n xs
divisibles n    _ = []

filterBy :: (a -> Bool) -> [a] -> [a]
filterBy f (x : xs)
    | f x       = x : filterBy f xs
    | otherwise =     filterBy f xs
filterBy f    _ = []


oddsUsingFilter :: Integral a => [a] -> [a]
oddsUsingFilter l = filterBy odd' l

divisiblesUsingFilter :: Integral a => a -> [a] -> [a]
divisiblesUsingFilter n l = filterBy (\x -> x `mod` n == 0) l

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce f s (x : xs) = reduce f (f s x) xs
reduce f s    _     = s

productUsingApply :: Num a => [a] -> a
productUsingApply l = reduce (*) 1 l

concat' :: [[a]] -> [a]
concat' l = reduce concatenateLists [] l

reduce' :: (a -> a -> a) -> a -> [a] -> a
reduce' f s (x : xs) = reduce' f (f x s) xs
reduce' f s    _     = s

-- TODO: Fix it to match the sample tests!
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f l1 l2 = applyToAll (\x -> f (fst x) (snd x)) (zip' l1 l2)
