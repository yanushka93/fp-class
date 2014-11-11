import Control.Applicative
import System.Random

{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: (Ord a, Applicative f) => f a -> f a -> f a
maxApp2 x y = max <$> x <*> y

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 x y z = maxApp2 x $ maxApp2 y z

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp = foldl1 maxApp2

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

main =do
	print $ maxApp2 (Just 4) (Just 5)
	print $ maxApp3 (Just 4) (Just 5) Nothing
	print $ maxApp [Just 1, Just 3, Just 5, Just 45]
	print $ maxApp2 (Right 6 :: Either String Int) (Right 5 :: Either String Int)
	print $ maxApp3 (Right 4 :: Either String Int) (Left "empty" :: Either String Int) (Right 6 :: Either String Int)
	print $ maxApp [(Right 4 :: Either String Int), (Right 45 :: Either String Int), (Right 6 :: Either String Int), (Right 788 :: Either String Int)]
	print $ maxApp [(Right 4 :: Either String Int), (Left "error" :: Either String Int), (Right 6 :: Either String Int), (Right 788 :: Either String Int)]
	maxApp2 (randomIO :: IO Int) (randomIO :: IO Int) >>= print
	maxApp3 (randomIO :: IO Int) (randomIO :: IO Int) (randomIO :: IO Int) >>= print
	maxApp [randomIO :: IO Int, randomIO :: IO Int, randomIO :: IO Int, randomIO :: IO Int] >>= print

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
