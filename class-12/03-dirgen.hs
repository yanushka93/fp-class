{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import System.Random
import System.IO.Unsafe

data AppConfig = AppConfig {
		cfgMaxDepth :: Int,
		cfgMinDepth :: Int,
		cfgMaxWidth :: Int,
		cfgMinWidth :: Int,
		cfgMinCountFiles :: Int,
		cfgMaxCountFiles :: Int,
		cfgFileSize :: Int
    } deriving (Show)

data AppState = AppState {
		stCurCountFiles :: Int,
		stCurDepth :: Int,
		stCurPath :: FilePath
    } deriving (Show)

newtype MyApp a = MyA {
		runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppConfig,
                MonadState AppState)

runMyApp :: MyApp a -> [Int] -> FilePath -> IO a
runMyApp app [minD, maxD, minW, maxW, minC, maxC, size] path =
    let config = AppConfig minD maxD minW maxW minC maxC size
        state = AppState 0 0 path
    in evalStateT (runReaderT (runA app) config) state

randomName :: Bool -> IO String
randomName dir
	| dir == True = do
				gen <- newStdGen
				return $ take 5 $ randomRs ('a', 'z') gen
	| otherwise = do
					name <- randomName True
					return $ name ++ ".txt"

randomNumber :: Int -> Int -> IO Int
randomNumber begin end = do
	gen <- newStdGen
	return $ head $ randomRs (begin, end) gen

generateRandomFile :: Int -> String -> IO ()
generateRandomFile size name = do
	gen <- newStdGen
	let text = take size $ randomRs ('a', 'z') gen
	writeFile name text

getParamsForGenerate (minD, maxD, minW, maxW, minC, maxC) = do
	depth <- liftIO $ randomNumber minD maxD
	width <- liftIO $ randomNumber minW maxW
	count <- liftIO $ randomNumber minC maxC
	return (depth, width, count)
	
generateFiles :: MyApp ()	
generateFiles = do
	minD <- cfgMinDepth `liftM` ask
	maxD <- cfgMaxDepth `liftM` ask
	minW <- cfgMinWidth `liftM` ask
	maxW <- cfgMaxWidth `liftM` ask
	minC <- cfgMinCountFiles `liftM` ask
	maxC <- cfgMaxCountFiles `liftM` ask
	(depth, width, count) <- getParamsForGenerate (minD, maxD, minW, maxW, minC, maxC)
	size <- cfgFileSize `liftM` ask
	state <- get
	let currentCount = stCurCountFiles state
	let currentDepth = stCurDepth state
	let currentPath = stCurPath state
	when (currentDepth < depth) $ do
		forM_ [1..count] $ \a -> do
			randName <- liftIO $ randomName False
			let filePath = currentPath </> randName
			liftIO $ generateRandomFile size filePath
		forM_ [1..width] $ \a -> do
			dirName <- liftIO $ randomName True
			let newPath = currentPath </> dirName
			liftIO $ createDirectory newPath
			put $ state {stCurDepth = currentDepth + 1, stCurPath = newPath, stCurCountFiles = currentCount + count}
			generateFiles

main = do
	args <- getArgs
	createDirectory (head args)
	runMyApp generateFiles (map read (tail args)) (head args)