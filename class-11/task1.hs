{-
1. Написать программу, работа которой управляется конфигурационным файлом, содержащим строки следующего формата:
имя поля=значение
Возможными именами полей являются summand (слагаемое), multiplier (множитель), divisor (делитель). Все значения
являются целыми числами. В качестве параметров командной строки программе подаются имя конфигурационного файла
и имя текстового файла с целочисленными данными. Над каждым целым числом из второго файла выполняются операции,
указанные в конфигурационном файле, то есть число складывается, умножается и делится соответственно.
Если какое-либо поле отсутствует, то действие не выполняется. Результаты вычислений выводятся на консоль.
Организовать доступ к параметрам конфигурационного файла средствами монады Reader.
-}

import System.Environment
import Control.Monad.Reader
import Data.Maybe

data Config = Config Int Int Int
	deriving Show
	
readConfig content = do
	let xs = map (\str -> span (/= '=') str) $ lines content
	let s = read $ tail $ fromMaybe "=0" (lookup "summand" xs)
	let m = read $ tail $ fromMaybe "=1" (lookup "multiplier" xs)
	let d = read $ tail $ fromMaybe "=1" (lookup "divisor" xs)
	return (Config s m d)
	
doOperation numbs = do
	(Config s m d) <- ask
	return $ map (\acc -> div ((acc + s) * m) d) numbs
	
getNumbs content = map read $ concat $ map words $ lines content
	
main = do
	(file1 : file2 : _) <- getArgs
	content <- readFile file2
	let numbs = getNumbs content
	confcontent <- readFile file1
	readConfig confcontent >>= print . runReader (doOperation numbs)