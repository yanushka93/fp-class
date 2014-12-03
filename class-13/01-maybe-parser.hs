import Control.Monad
import Data.Maybe
{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
	return x = Parser(\s -> Just (x, s))
	p >>= q = Parser (\s -> apply p s >>= \(x, s') -> apply (q x) s')
	fail _ = Parser (\_ -> Nothing)

instance MonadPlus Parser where
	mzero = Parser(\_ -> Nothing)
	p `mplus` q = Parser (\s ->
		let res = apply p s in
			if isNothing res then apply q s else res)
