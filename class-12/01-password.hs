{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Environment
import Data.Char
import Control.Monad.Writer
import Control.Monad.Reader

isValid :: String -> [Int] -> Bool
isValid s [len, alpha, number, punctuation] = length s >= len && 
                if alpha == 1 then any isAlpha s else True && 
                if number == 1 then any isNumber s else True && 
                if punctuation == 1 then any isPunctuation s else True
	
parseArgs :: [String] -> [Int]	
parseArgs xs = map read xs

getValidPassword :: MaybeT (ReaderT [Int] (WriterT [String] IO)) String
getValidPassword = do
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  params <- lift ask
  guard (isValid s params)
  return s

askPassword :: MaybeT (ReaderT [Int] (WriterT [String] IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Сохранение в базе данных..."

main = do
	args <- getArgs
	res <- runWriterT (runReaderT (runMaybeT askPassword) (parseArgs args))
	return res
