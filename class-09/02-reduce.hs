import System.Environment
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
	| a `mod` 3 == 0 = 0
	| odd a = a * a
	| otherwise = a * a * a

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n f = foldl (\val _ -> fmap reduce val) f [1..n]

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = map (\(x, y) -> y)

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe list = Just (head $ toList list)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "empty"
toEither list = Right (head $ toList list)

-- воспользуйтесь в этой функции случайными числами
toIO :: (Random a, Integral a) => [(a, a)]  -> IO a
toIO [] = return 0
toIO list = do
	gen <- newStdGen
	let x = fst $ randomR (1, 99) gen
	return $ max x (head $ toList list)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs (filename : n : _ )= (filename, read n)

toPair (x : y : _) = (read x :: Int, read y :: Int)
			
readData :: FilePath -> IO [(Int, Int)]
readData filename = do
	content <- readFile filename
	return $ map (toPair . words) (lines content)
		

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print ps
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
  
	text1.txt 4
	[(-8,5),(9,6),(2,1)]
	[152587890625,0,1]
	Just 152587890625
	Right 152587890625
	4808171226659346945
  
	test2.txt 4
	[]
	[]
	Nothing
	Left "empty"
	0
	
	test3.txt 3
	[(3,0),(1,8),(0,0)]
	[0,0,0]
	Just 0
	Right 0
	390625
-}
