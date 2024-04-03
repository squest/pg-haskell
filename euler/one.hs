{- This is a multi-line comment in haskell -}
-- This is a single line comment in haskell
import Data.List
import System.IO

square x = x * x

-- this is euler problem 1

eulerOneA lim = sum [x | x <- [1..lim-1], mod x 3 == 0 || mod x 5 == 0]
eulerOneB a b lim = sum [a, a+a..lim-1] + sum [b, b+b..lim-1] - sum [a*b, a*b+a*b..lim-1]

-- good ole recursive way to solve euler problem 1

eulerOneC a b lim = eulerOneC' 1 0
  where eulerOneC' n suma
  | n == lim = suma
  | mod n a == 0 || mod n b == 0 = eulerOneC' (n+1) (suma + n)
  | otherwise = eulerOneC' (n+1) suma

-- this is euler problem 2


