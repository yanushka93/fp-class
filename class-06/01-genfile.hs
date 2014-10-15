{-
  В параметрах командной строки заданы целое число N, текстовая строка и имя файла.
  Создать файл с заданным именем, содержащий N-кратное повторение заданной строки.

  Совет: для запуска функции main в ghci с заданием параметров командной строки удобно
  использовать команду интерпретатора :main, например:

     ghci> :main 1000 "Привет, мир" hello.txt
-}
import System.Environment
import Control.Monad
import System.IO

createFile :: Int -> String -> FilePath -> IO ()
createFile n s fname = withFile fname WriteMode (\handle -> do
	writeLines n s handle)

writeLines 1 s handle = hPutStrLn handle s
writeLines n s handle = do
	hPutStrLn handle s
	writeLines (n - 1) s handle

main = do
  [n_str, text, fname] <- getArgs
  createFile (read n_str) text fname
