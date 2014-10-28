{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = length . group . sort

deleteElem e s
    | Seq.length xs == 0 = if x == e then Seq.empty else Seq.singleton x 
    | otherwise = if x == e then deleteElem e xs else x Seq.<| (deleteElem e xs)
    where
      (x Seq.:< xs) = Seq.viewl s

nub_seq :: Seq.Seq Int -> Int
nub_seq s = Seq.length $ deleteRepeated s
  where
    deleteRepeated s
      | Seq.length s == 0 = Seq.empty 
      | Seq.length s == 1 = s
      | otherwise = let (x Seq.:< xs) = Seq.viewl s in x Seq.<| (deleteRepeated (deleteElem x xs))

nub_arr :: Array Int Int -> Int
nub_arr = nub_list . Data.Array.IArray.elems

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
