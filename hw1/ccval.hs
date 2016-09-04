
-- Excercise 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
  | x > 0 = getDigit x: toDigitsRev (shiftDecimalPlace  x)
  | otherwise = []

shiftDecimalPlace :: (Integral a) => a -> a 
shiftDecimalPlace x = floor $ fromIntegral x /10

getDigit x = x `mod` 10


-- Excercise 2

doubleEveryOther :: (Integral a) => [a] -> [a]
doubleEveryOther [] = [] 
doubleEveryOther (x:xs) 
  | odd . length $ xs = x*2 : doubleEveryOther xs
  | otherwise = x : (doubleEveryOther xs)


-- Excercise 3

sumDigits :: [Integer] -> Integer
sumDigits x = foldl (\a b -> a + sumOneDigits b) 0 x

sumOneDigits :: Integer -> Integer
sumOneDigits x
  | x < 10 && x > 0 = x
  | x <= 0 = 0 
  | otherwise = x `mod` 10 + (sumOneDigits . shiftDecimalPlace) x

-- Excercise 4

validate :: Integer -> Bool
validate x 
  | (sumDigits . doubleEveryOther . toDigits ) x `mod` 10 == 0 = True
  | otherwise = False


 
