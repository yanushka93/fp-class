import Data.List
import System.Environment
import Data.Monoid

{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

type Name = String
type Age = Int
type Group = Int

data Student = Student Name Age Group
	deriving (Show, Eq)

formedStudents content = map formedStudent $ chunksOf' 3 content

chunksOf' _ [] = []
chunksOf' n content = (take n content) : (chunksOf' n $ drop n content)

formedStudent (name : age : group : _) = Student name (read age :: Int) (read group :: Int)

studentToList students = unlines $ map (\(Student name age group) -> unlines [name, (show age), (show group)]) students

--main = (head `fmap` getArgs) >>= readFile >>= print . formedStudents . lines

main = do
	filename <- head `fmap` getArgs
	t1 <- readFile filename
	putStrLn $ studentToList $ formedStudents $ lines t1
