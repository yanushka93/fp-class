{-
	3. Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue.
-}

import Control.Monad.State

newtype Queue t = Queue [t]

empty = Queue []

isEmpty :: State (Queue t) Bool
isEmpty = do
	(Queue q) <- get
	put (Queue q)
	return $ null q

enqueue :: t -> State (Queue t) ()
enqueue x = do
	(Queue q) <- get
	put (Queue (q ++ [x]))
	
dequeue :: State (Queue t) t	
dequeue = do
	(Queue (x : q)) <- get
	put (Queue q)
	return x
	
queueManip = do
	enqueue 0
	enqueue 1
	enqueue 2
	a <- dequeue
	enqueue a
	dequeue
	
	
test = (fst $ runState queueManip empty) == 2