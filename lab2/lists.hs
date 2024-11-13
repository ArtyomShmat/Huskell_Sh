import Data.Bits (shiftR, (.&.))
import Data.List (unfoldr)

-- Function to generate an infinite list of prime numbers
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- 1) Развернуть натуральное число n в список всех чисел, меньших его.
lessThanN :: Int -> [Int]
lessThanN n = [0..n-1]

-- 2) Развернуть число в список разрядов его двоичного представления.
binaryDigits :: Int -> [Int]
binaryDigits 0 = [0]
binaryDigits n = reverse (unfoldr f n)
  where
    f 0 = Nothing
    f x = Just (x .&. 1, x `shiftR` 1)

-- 3) Список разрядов преобразовать свёрткой в значение числа.
foldBinaryDigits :: [Int] -> Int
foldBinaryDigits = foldl (\acc x -> acc * 2 + x) 0

-- 4) Развернуть число в список его простых делителей.
primeFactors :: Int -> [Int]
primeFactors n = factorize n primes
  where
    factorize 1 _ = []
    factorize m (p:ps)
      | m < p * p = [m]
      | m `mod` p == 0 = p : factorize (m `div` p) (p:ps)
      | otherwise = factorize m ps

-- 5) Выразить список первых n чисел Фибоначчи через развёртку.
fibonacci :: Int -> [Int]
fibonacci n = take n $ unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

-- Модификация: бесконечный список чисел Фибоначчи
infiniteFibonacci :: [Int]
infiniteFibonacci = unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)

-- 6) Развернуть число в сиракузскую последовательность.
syracuse :: Int -> [Int]
syracuse 1 = [1]
syracuse n = n : syracuse (if even n then n `div` 2 else 3 * n + 1)

-- 7) Выразить список простых чисел, не превышающих n, через развёртку
-- с помощью решета Эратосфена.
sieveEratosthenes :: Int -> [Int]
sieveEratosthenes n = sieve [2..n]
  where
    sieve [] = []
    sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

-- Модификация: бесконечный список всех простых чисел
infinitePrimes :: [Int]
infinitePrimes = primes

main :: IO ()
main = do
  putStrLn "Список чисел меньше 10:"
  print $ lessThanN 10

  putStrLn "Двоичное представление числа 10:"
  print $ binaryDigits 10

  putStrLn "Число из его двоичного представления [1, 0, 1, 0]:"
  print $ foldBinaryDigits [1, 0, 1, 0]

  putStrLn "Простые делители числа 28:"
  print $ primeFactors 28

  putStrLn "Первые 10 чисел Фибоначчи:"
  print $ fibonacci 10

  putStrLn "Первые 10 чисел бесконечного ряда Фибоначчи:"
  print $ take 10 infiniteFibonacci

  putStrLn "Сиракузская последовательность для числа 10:"
  print $ syracuse 10

  putStrLn "Простые числа, не превышающие 30, через решето Эратосфена:"
  print $ sieveEratosthenes 30

  putStrLn "Первые 10 простых чисел:"
  print $ take 10 infinitePrimes