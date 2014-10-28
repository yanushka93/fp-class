{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile filename = do
	content <- readFile filename
	return $ map read $ concatMap words $ lines content

solve lists = (length list, sum list)
	where list = Set.toList $ foldl1 Set.intersection $ map Set.fromList lists

main = getArgs >>= mapM readNumFile >>= print.solve
