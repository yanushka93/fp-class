import System.Environment
import Data.Monoid
import Data.Maybe

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData content = map (\val -> if val == "-" then Nothing else (Just (read val))) (lines content)

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay sdata = (take 5 sdata) : (dataByDay $ drop 5 sdata)

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 True sdata = minimum $ map (fromJust . getFirst . mconcat . map First) $ filter (any isJust) sdata
minData1 False sdata = minimum $ map (fromJust . getLast . mconcat . map Last) $ filter (any isJust) sdata

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}

minData2 :: Bool -> [SensorData] -> Int
minData2 True sdata = minimum $ map (\vals -> getSum . mconcat . map Sum $ map (fromMaybe 0) vals) $ filter (any isJust) sdata
minData2 False sdata = minimum $ map (\vals -> getProduct . mconcat . map Product $ map (fromMaybe 1) vals) $ filter (any isJust) sdata

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct

minData :: SensorTask -> [SensorData] -> Int
minData st  sdata = minimum $ getResult st
	where
		getResult NeedFirst = map (fromJust . getFirst . mconcat . map First) $ filter (any isJust) sdata
		getResult NeedLast = map (fromJust . getLast . mconcat . map Last) $ filter (any isJust) sdata
		getResult NeedSum = map (\vals -> getSum . mconcat . map Sum $ map (fromMaybe 0) vals) $ filter (any isJust) sdata
		getResult NeedProduct = map (\vals -> getProduct . mconcat . map Product $ map (fromMaybe 1) vals) $ filter (any isJust) sdata

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}

getSensorInfo 1 sData _ = length $ filter (== True) $ map (getAll . mconcat . map (All . isNothing)) sData
getSensorInfo 2 sData _ = length $ filter (== True) $ map (getAll . mconcat . map (All . isJust)) sData
getSensorInfo 3 sData _ = length $ filter (== True) $ map (getAny . mconcat . map (Any . isJust)) sData
getSensorInfo 4 sData n = length $ filter (> n) $ map (getSum . mconcat . map (Sum . fromJust) . filter isJust) sData
getSensorInfo 5 sData n = length $ filter (> n) $ map (getProduct . mconcat . map (Product . fromJust) . filter isJust) sData
getSensorInfo 6 sData n = length $ filter (>n) $ map (fromJust. getFirst . mconcat . map First $) $ filter (any isJust) sData
getSensorInfo 7 sData n = length $ filter (>n) $ map (fromJust. getLast . mconcat . map Last $) $ filter (any isJust) sData

main = do
	fname <- head `fmap` getArgs
	sData <- fmap (dataByDay . getData) (readFile fname)
	putStrLn $ "No data = " ++ (show $ getSensorInfo 1 sData 0)
	putStrLn $ "All data = " ++ (show $ getSensorInfo 2 sData 0)
	putStrLn $ "Any data = " ++ (show $ getSensorInfo 3 sData 0)
	putStrLn $ "Sum > 50 = " ++ (show $ getSensorInfo 4 sData 50)
	putStrLn $ "Product > 1000 = " ++ (show $ getSensorInfo 5 sData 1000)
	putStrLn $ "First > 10 = " ++ (show $ getSensorInfo 6 sData 10)
	putStrLn $ "Last > 1000 = " ++ (show $ getSensorInfo 7 sData 5)
