{- This is a multi-line comment in haskell -}
-- This is a single line comment in haskell
import Data.List
import System.IO

square x = x * x

-- this is euler problem 1

eulerOneA lim = sum [x | x <- [1..lim-1], mod x 3 == 0 || mod x 5 == 0]
eulerOneB a b lim = sum [a, a+a..lim-1] + sum [b, b+b..lim-1] - sum [a*b, a*b+a*b..lim-1]

-- good ole recursive way to solve euler problem 1
solOne a b lim = solOne 1 0
  where 
    solOne n suma
      | n == lim = suma
      | mod n a == 0 || mod n b == 0 = solOne (n+1) (suma + n)
      | otherwise = solOne (n+1) suma

-- this is euler problem 2

eulerTwoA lim = sum [x | x <- takeWhile (<= lim) fibs, even x]
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

  -- coba jelasin masing2x function itu ngapain? biar bisa dijelasin ke orang lain
  -- fibs itu apa? fibs itu list of fibonacci number, lazy evaluation
  -- zipWith ngapain? zipWith itu buat ngelakuin operasi pada 2 list
  -- takeWhile ngapain? takeWhile itu buat ngambil elemen dari list selama kondisi terpenuhi

-- good ole recursive way to solve euler problem 2

solTwo a b lim = solTwo 0 1 0
  where
    solTwo a b suma
      | a > lim = suma
      | mod a 2 == 0 = solTwo b (a+b) (suma + a)
      | otherwise = solTwo b (a+b) suma

-- this is euler problem 3

isOddPrime n = all (\x -> mod n x /= 0) [3,5..(floor . sqrt . fromIntegral) n]

nextPrime n
  | isOddPrime (n+2) = n+2
  | otherwise = nextPrime (n+2)

eulerThreeA n = eulerThreeA 3 n
  where
    eulerThreeA p n
      | n == 1 = p
      | mod n p == 0 = eulerThreeA (nextPrime p) (div n p)
      | otherwise = eulerThreeA (nextPrime p) n

