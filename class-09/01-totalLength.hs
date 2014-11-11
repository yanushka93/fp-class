import System.Environment
import Data.Functor

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength = foldl (\len x -> len + length x) 0

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 c n
	| n == 0 = Nothing
	| otherwise = fmap (map (\count -> replicate count c)) (Just [1..n])
{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 c n
	| n == 0 = Left "n = 0"
	| n > 100 = Left "n > 100"
	| c == 'c' = Left "CPS prohibits create lines of an 'x'"
	| otherwise = fmap (map (\count -> replicate count c)) (Right [1..n])

{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

main = do
	t1 <- fmap totalLength getArgs
	putStrLn $ "Task 1 (total length of args) = " ++ show t1
	(filename : c : n : _) <- getArgs
	t2 <- fmap (totalLength . words) (readFile filename)
	putStrLn $ "Task 2 (total length of file) = " ++ show t2
	
	putStrLn $ "Task3.1 (total length of build1) = " ++ show (fmap totalLength $ build1 (head c) (read n))
	
	putStrLn $ "Task3.2 (total length of build2) = " ++ show (fmap totalLength $ build2 (head c) (read n))
