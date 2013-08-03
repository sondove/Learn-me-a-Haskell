import Data.Char
import Data.List

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x 
    | x < 0     = []
    | otherwise = map (toInteger.digitToInt) $ show x

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

merge :: [[a]] -> [a]
merge = concat . transpose

filterEveryOther :: [Integer] -> [Integer]
filterEveryOther []         = []
filterEveryOther (x:[])     = [x]
filterEveryOther (x:(_:xs)) = x : filterEveryOther xs

doubleInteger :: Integer -> Integer
doubleInteger x = 2*x

reverseDoubleEveryOther :: [Integer] -> [Integer]
reverseDoubleEveryOther []         = []
reverseDoubleEveryOther (x:[])     = [x]
reverseDoubleEveryOther (x:xs)     =  merge [
                            filterEveryOther (x:xs), 
                            map doubleInteger $ filterEveryOther xs]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther (x:[])     = [x]
doubleEveryOther (x:xs)     =  reverse $ reverseDoubleEveryOther (reverse (x:xs)) 

sumDigits :: [Integer] -> Integer 
sumDigits []    = 0
sumDigits x     = toInteger $ sum $ map digitToInt $ concat $ map show x

validate :: Integer -> Bool
validate 0 = False
validate x  = 0 == ( sumDigits $ doubleEveryOther $ toDigits x) `mod` 10

main = do
    putStr "\nExercise 1\n"
    print (toDigits 1234 == [1,2,3,4])
    print (toDigitsRev 1234 == [4,3,2,1])
    print (toDigits 0 == [])
    print (toDigits (-17) == [])

    putStr "\nExercise 2\n"
    print (doubleEveryOther [8,7,6,5] == [16,7,12,5])
    print (doubleEveryOther [1,2,3] == [1,4,3])

    putStr "\nExercise 3\n"
    print (sumDigits [16,7,12,5] == 22 )
    print (sumDigits [] == 0)

    putStr "\nExercise 4\n"
    print ( validate 4012888888881881 == True)
    print ( validate 4012888888881882 == False)


