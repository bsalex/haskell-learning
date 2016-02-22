module FizzBuzz where

modZero :: Integral a => a -> a -> Bool
modZero d x = x `mod` d == 0

handleNumber :: Integral a => a -> (a, String)
handleNumber 0 = (0, "")
handleNumber x
  | modZero 3 x && modZero 5 x = (x, "fizzbuzz")
  | modZero 3 x = (x, "fizz")
  | modZero 5 x = (x, "buzz")
  | otherwise = (x, "")

handleNumbersFromTo :: Integer -> Integer -> [(Integer, String)]
handleNumbersFromTo x y = filter (not . null . snd) [handleNumber n | n <- [x..y]]
