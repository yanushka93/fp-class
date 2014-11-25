{-
	4. Пользуясь средствами монады ST, запрограммировать сортировку массива тремя любыми методами.
-}

import Data.STRef
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray

swap' :: Ix i => i -> i -> STArray s i e -> ST s ()
swap' i j arr = do
	xi <- readArray arr i
	xj <- readArray arr j
	writeArray arr i xj
	writeArray arr j xi
	
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort arr = elems $ runSTArray $ do
	let len = length arr
	newarr <- newListArray (0, len - 1) arr
	forM_ [0..len - 1] $ \i ->
		forM_ [0..len - i - 2] $ \j -> do
			x1 <- readArray newarr j
			x2 <- readArray newarr (j+1)
			when (x1 > x2) (swap' j (j+1) newarr)
	return newarr
	
selectingSort :: (Ord a) => [a] -> [a]
selectingSort arr = elems $ runSTArray $ do
	let len = length arr
	newarr <- newListArray (0, len - 1) arr
	forM_ [0..len - 2] $ \i ->
		forM_ [i + 1..len - 1] $ \j -> do
			x1 <- readArray newarr i
			x2 <- readArray newarr j
			when (x1 > x2) (swap' i j newarr)
	return newarr

insertingSort :: (Ord a) => [a] -> [a]
insertingSort arr = elems $ runSTArray $ do
	let len = length arr
	newarr <- newListArray (0, len - 1) arr
	forM_ [1..len - 1] $ \i -> do
		forM_ [1..i] $ \j -> do
			xj <- readArray newarr (i - j + 1)
			xjj <- readArray newarr (i - j)
			if (xj < xjj) then swap' (i - j + 1) (i - j) newarr else return ()
	return newarr
	
testlist = [7,4,9,61,11,2,14,3,1]
test = (bubbleSort testlist == [1,2,3,4,7,9,11,14,61]) && (selectingSort testlist == [1,2,3,4,7,9,11,14,61]) && (insertingSort testlist == [1,2,3,4,7,9,11,14,61])