import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)

{- Напишите парсер для вещественных чисел. -}

float :: Parser Float
float = ((*) <$> minus <*> floatNumber) <|> fromIntegral <$> integer
	where
		floatNumber = do
			x <- natural
			char '.'
			y <- natural
			return $ fromIntegral x + (fromIntegral y / 10 ^ countDigits y)
		minus = (char '-' >> return (-1)) <|> return 1
		countDigits 0 = 0
		countDigits x = 1 + countDigits (x `div` 10)

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
	a <- token float
	char ','
	i <- token float
	return (a, i)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token complex <|> complexFromFloat) (symbol ";")
	where
		complexFromFloat = do
			val <- token float
			return (val, 0)

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy (token complex <|> complexFromFloat) (symbol ",")
	where
		complexFromFloat = do
			val <- token float
			return (val, 0)


