{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- Можно писать лямбды в своё удовольствие.
{- HLINT ignore "Redundant lambda" -}

-- | Домашка 4. Введение в Haskell: уровень 1, обязательные задачи.
--
-- Условие каждой задачи — комментарий перед её кодом; решение пишется на месте заглушки @todo@,
-- а в задаче 1.6 — на месте заглушки 'Todo' в типе.
module Level1 where

import MetaUtils (todo)
import TypeCheck (Todo)


-- 1.1. Пары из λ-исчисления
--
-- Оттранслируйте в Haskell пары в стиле чистого λ-исчисления: термы pair, fst и snd.
-- Имена fst и snd в Haskell заняты стандартными функциями для обычных пар, поэтому
-- ваши называются fstChurch и sndChurch.
-- Ознакомьтесь с тем, как это задание тестируется в test/SpecLevel1.hs.

pair = \ x y f -> f x y
fstChurch =  \p -> p  ( \ x _ -> x )
sndChurch =  \p -> p ( \ _ y -> y )


-- 1.2. Взаимная рекурсия
--
-- Реализуйте взаимно-рекурсивную пару функций:
-- isEven(n) = True, если n = 0
--             isOdd(n + 1), если n < 0
--             isOdd(n - 1), если n > 0
-- isOdd(n) = False, если n = 0
--            isEven(n + 1), если n < 0
--            isEven(n - 1), если n > 0
-- При реализации используйте охранные выражения (guards, см. лекцию).

isEven :: Integer -> Bool
isEven n
 | n < 0 = isOdd (n + 1)
 | n > 0 = isOdd (n - 1)
 | otherwise = True



isOdd :: Integer -> Bool
isOdd n
  | n < 0 = isEven (n + 1)
  | n > 0 = isEven (n - 1)
  | otherwise  = False


-- 1.3. Найдите ошибку
--
-- Перед вами факториал с аккумулятором. На одном аргументе от 0 до 1000 он отвечает неверно.
-- Найдите этот аргумент и запишите его в counterexample. Сначала попробуйте обойтись
-- без запуска: проредуцируйте несколько вызовов руками, как на паре.

facBuggy :: Integer -> Integer
facBuggy n = go n (n - 1)
  where
    go acc n'
      | n' <= 0 = acc
      | otherwise = go (acc * n') (n' - 1)

counterexample :: Integer
counterexample = 0


-- 1.4. Рекуррентная последовательность
--
-- Реализуйте функцию, находящую элементы следующей рекуррентной последовательности:
-- b[0] = 1; b[1] = 2; b[2] = 3; b[k + 3] = b[k + 2] - 2 * b[k + 1] + 3 * b[k]
-- Постарайтесь сделать так, чтобы ваша функция работала за линейное время.

itemAt :: Integer -> Integer
itemAt n
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | otherwise = go 2 1 2 3
  where
    go k b0 b1 b2
      | k == n = b2
      | otherwise =
          let next = b2 - 2 * b1 + 3 * b0
          in go (k + 1) b1 b2 next


-- 1.5. Цифры числа
--
-- Реализуйте функцию, которая по числу возвращает количество цифр в его модуле (abs),
-- а также их сумму. Вычисление должно быть хвостово-рекурсивным: рекурсивный вызов стоит
-- последним действием. Используйте параметры-аккумуляторы.

nSumDigits :: Integer -> (Integer, Integer)
nSumDigits n = go (abs n) 0 0
  where
    go number count total
      | number == 0 && count == 0 = (1, 0)
      | number == 0 = (count, total)
      | otherwise =
          go
            (number `div` 10)
            (count + 1)
            (total + number `mod` 10)


-- 1.6. Предскажите тип
--
-- Над каждой заглушкой в комментарии записано выражение. Замените Todo его типом —
-- наиболее общим, имена типовых переменных выбирайте любые. Тело оставьте заглушкой:
-- проверяется только записанный вами тип.
-- Сначала запишите ответ, и только потом сверьтесь с интерпретатором командой :t.

-- uncurry const
typeOfUncurryConst :: (a, b) -> a
typeOfUncurryConst = undefined

-- curry fst
typeOfCurryFst :: a -> b -> a 
typeOfCurryFst = undefined

-- flip (,)
typeOfFlipPair :: b -> a -> (a, b)
typeOfFlipPair = undefined
