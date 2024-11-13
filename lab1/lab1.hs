-- 1) Алгоритм Евклида для нахождения наибольшего общего делителя
gcdEuclid :: Integer -> Integer -> Integer
gcdEuclid a b
  | a == b    = a
  | a > b     = gcdEuclid (a - b) b
  | otherwise = gcdEuclid a (b - a)

-- 2) Возведение в степень за O(log n)
power :: Integer -> Integer -> Integer
power x n
  | n == 0    = 1
  | even n    = halfPower * halfPower
  | otherwise = x * power x (n - 1)
  where
    halfPower = power x (n `div` 2)

-- 3) Совершенные числа
isPerfect :: Integer -> Bool
isPerfect n = n > 1 && sum (divisors n) == n
  where
    divisors x = [i | i <- [1 .. x `div` 2], x `mod` i == 0]

-- 4) Сиракузская последовательность
collatzLength :: Integer -> Integer
collatzLength n = collatz n 0
  where
    collatz 1 len = len
    collatz x len
      | even x    = collatz (x `div` 2) (len + 1)
      | otherwise = collatz (3 * x + 1) (len + 1)
-- 5) Числа Деланнуа
delannoy :: Integer -> Integer -> Integer
delannoy 0 _ = 1
delannoy _ 0 = 1
delannoy m n = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)

-- 6) Вычисление многочлена
evalPolynomial :: [Integer] -> Integer -> Integer
evalPolynomial coeffs x = sum $ zipWith (*) coeffs powers
  where
    powers = [x^i | i <- [0..]]

-- 7) Клонирование элементов списка
clone :: Int -> [a] -> [a]
clone n = concatMap (replicate n)





-- 8) Сшивание списков бинарной операцией
xZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys

-- 9) Список чисел Фибоначчи
fibonacci :: Int -> [Integer]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

infiniteFibonacci :: [Integer]
infiniteFibonacci = 0 : 1 : zipWith (+) infiniteFibonacci (tail infiniteFibonacci)

generalizedFibonacci :: [Integer] -> [Integer]
generalizedFibonacci seeds = seeds ++ genFib seeds
  where
    genFib xs = let next = sum (take (length seeds) xs) in next : genFib (tail xs ++ [next])

-- Задача 10: Системы счисления

-- Функция для вычисления значения числа из его разрядов в n-ичной системе счисления
fromDigits :: Int -> [Int] -> Int
fromDigits n = foldl (\acc x -> acc * n + x) 0

-- Функция для построения списка разрядов представления числа в системе счисления с основанием n
toDigits :: Int -> Int -> [Int]
toDigits n x = reverse $ helper x
    where helper 0 = []
          helper k = k `mod` n : helper (k `div` n)

-- Функция для сложения чисел, представленных поразрядно в n-ичной системе счисления
addDigitwise :: Int -> [Int] -> [Int] -> [Int]
addDigitwise n xs ys = reverse $ helper (reverse xs) (reverse ys) 0
    where helper [] [] 0 = []
          helper [] [] carry = [carry]
          helper (x:xs) [] carry = let sum = x + carry
                                    in sum `mod` n : helper xs [] (sum `div` n)
          helper [] (y:ys) carry = let sum = y + carry
                                    in sum `mod` n : helper [] ys (sum `div` n)
          helper (x:xs) (y:ys) carry = let sum = x + y + carry
                                       in sum `mod` n : helper xs ys (sum `div` n)

-- Задача 11: Перечисление путей в решётке

-- Функция для генерации всех возможных маршрутов в решетке a×b
delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths a b = helper a b 0 0
    where helper x y i j
            | i == x && j == y = [[]]
            | i == x = map (0:) (helper x y i (j+1))
            | j == y = map (2:) (helper x y (i+1) j)
            | otherwise = map (0:) (helper x y i (j+1)) ++
                          map (1:) (helper x y (i+1) (j+1)) ++
                          map (2:) (helper x y (i+1) j)

main :: IO ()
main = do
  print (gcdEuclid 24 18)
  print (power 2 10)
  print (isPerfect 28)
  print (collatzLength 13)
  print $ delannoy 2 2  -- Ожидается 13

  -- Вычисление многочлена
  print $ evalPolynomial [2, 1, 5] 3  -- Ожидается 26

  -- Клонирование элементов списка
  print $ clone 3 [1, 2, 3]  -- Ожидается [1, 1, 1, 2, 2, 2, 3, 3, 3]
  
  print $ clone 1 [1, 2, 3]  -- Ожидается [1, 2, 3]
  print $ clone 0 [1, 2, 3]  -- Ожидается []
  
  print $ xZipWith (+) [10, 20, 30] [9, 8, 7]  -- Ожидается [19, 28, 37]
  print $ fibonacci 10  -- Ожидается [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
  print $ take 10 infiniteFibonacci  -- Ожидается [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
  print $ take 10 $ generalizedFibonacci [7, 3, 10, 0]  -- Ожидается [7, 3, 10, 0, 20, 33, 63, 106, 222, 424]

  -- Проверка задачи 11
  print $ fromDigits 2 [1, 0, 1, 1, 0, 1] -- 45
  print $ toDigits 2 45 -- [1, 0, 1, 1, 0, 1]
  print $ addDigitwise 2 [1, 0, 1, 1, 0, 1] [1, 1, 1] -- [1, 1, 0, 1, 0, 0]

  -- Проверка задачи 12
  print $ delannoyPaths 2 2