import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SequenceQueue as SQ
import System.Environment
import System.Random

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'

generateList n = do
	let count = getCountElemsForTask n
	gen <- newStdGen
	return $ take count (randomRs (0, 9999) gen :: [Int])

getCountElemsForTask 0 = 0
getCountElemsForTask n = n + (getCountElemsForTask (n - 1))
	
addNElems queue elems count = foldl enqueue queue (take count elems)

deleteNElems queue n = foldl (\acc _ -> snd $ dequeue acc) queue [1..n]

toList queue = if (isEmpty queue) then [] else (e : toList queue')
	where
		(e, queue') = dequeue queue
			 	 
testingQueue queue n elems = testingQueue' queue 1 elems
	where
		testingQueue' q c e
			| c == n = deleteNElems (addNElems q e c) (c - 1)
			| otherwise = testingQueue' (deleteNElems (addNElems q e c) (c - 1)) (c + 1) (drop c e)
	

main = do
	(arg0 : args) <- getArgs
	n <- readIO arg0
	elems <- generateList n
	let l1 = toList $ testingQueue (empty :: Q.Queue Int) n elems
	let l2 = toList $ testingQueue (empty :: FQ.Queue Int) n elems
	let l3 = toList $ testingQueue (empty :: SQ.Queue Int) n elems
	putStrLn $ show (l1 == l2 && l2 == l3 && length l1 == n)