-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms sec = (sec `div` 3600, sec `div` 60 - sec `div` 3600 * 60, sec `mod` 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000] --верно

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- triangle :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (p, s)
  where
    p = distance (x1, y1) (x2, y2) + distance (x1, y1) (x3, y3) + distance (x3, y3) (x2, y2)
    s = sqrt $ p / 2 * (p / 2 - distance (x1, y1) (x2, y2)) * (p / 2 - distance (x1, y1) (x3, y3)) * (p / 2 - distance (x3, y3) (x2, y2)) 

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
	| even x = 1 + nEven xs
	| otherwise = nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = (x * 2) : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	| odd x = x : fltOdd xs
	| otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
deleteNeg :: (Ord t, Num t) => [t] -> [t]
deleteNeg [] = []
deleteNeg (x:xs)
	| x < 0 = deleteNeg xs	
	| otherwise = x : deleteNeg xs
	
doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
	| even x = (x * 2) : doubleEven xs
	| otherwise = x : doubleEven xs

reverseNear :: [t] -> [t]
reverseNear [] = []
reverseNear (x1:x2:xs) = x2 : x1 : reverseNear xs
reverseNear (x1) = []
-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x + y) : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_to_list_pairs :: [t] -> [t1] -> [(t,t1)]
combine_to_list_pairs [] _ = []
combine_to_list_pairs _ [] = []
combine_to_list_pairs (x:xs) (y:ys) = (x, y) : combine_to_list_pairs xs ys
-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstNatDesc :: (Ord a, Num a) => a -> [a]
firstNatDesc n
	| n > 0 = n : firstNatDesc (n - 1)
	| otherwise = []
-- б) в порядке возрастания.
firstNat :: (Ord a, Num a) => a -> [a]
firstNat n = first_nat 1
	where 
		first_nat k
			| k <= n = k : first_nat (k+1)
			| otherwise = []


-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertBetween :: a -> [a] -> [a]
insertBetween a (x1:x2:xs) = x1 : a : insertBetween a (x2:xs)
insertBetween a (x) = x
-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
divideList :: Eq t => [t] -> ([t],[t])
divideList (x:xs) = divide_on_two ([x], xs)
	where 
		divide_on_two (xs, []) = (xs, [])
		divide_on_two ((x:xs), (y:ys))
			| x == y = divide_on_two (x:y:xs, ys)
			| otherwise = (x:xs, y:ys)

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- Возвращает i-ый элемент
getIElem :: [a] -> Int -> a
getIElem [] _ = error "Index out"
getIElem (x:xs) i
	| i == 0 = x
	| otherwise = getIElem xs (i - 1)

-- б) Eq a => [a] -> a -> Bool
-- содержит ли список данный элемент
containceElem :: Eq a => [a] -> a -> Bool
containceElem [] _ = False
containceElem (x:xs) e
	| x == e = True
	| otherwise = containceElem xs e
-- в) [a] -> Int -> [a]
-- пропускает первые n элементов
dropFirstN :: [a] -> Int -> [a]
dropFirstN (x:xs) n 
	| n > 0 = x : dropFirstN xs (n - 1)
	| otherwise = []
-- г) a -> Int -> [a]
-- возвращает список из n a
repeatAN :: a -> Int -> [a]
repeatAN a n
	| n > 0 = a : repeatAN a (n - 1)
	| otherwise = []
-- д) [a] -> [a] -> [a]
-- соединяет два списка
boundList :: [a] -> [a] -> [a]
boundList [] ys = ys
boundList xs [] = xs
boundList (x:xs) (y:ys) = x : y : boundList xs ys

-- е) Eq a => [a] -> [[a]]
-- ж) [a] -> [(Int, a)]
-- з) Eq a => [a] -> [a]
