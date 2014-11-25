{-
2. Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell.
-}

import Control.Monad.Writer

eps = 0.000000001

myTeylor val ssum x n = tell [val] >>
		if (abs (ssum - nextVal) < eps) then return val
		else myTeylor (val + nextVal) nextVal x (n + 2)
	where
		nextVal = (-1) * ssum * x * x / ((n + 1) * (n + 2))
		
mysin x = fst $ runWriter $ myTeylor x x x 1

mycos x = fst $ runWriter $ myTeylor 1 1 x 0