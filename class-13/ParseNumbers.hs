module ParseNumbers where

import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad


addition' = do
  n <- digit
  char '+'
  m <- digit
  return $ n + m

addition = digit >>= rest
  where
    rest m = (liftM (+m) $ char '+' >> digit) <|> return m


natural = foldl1 (\m n -> m *10 + n) `liftM` many1 digit


integer :: Parser Int
integer = (*) <$> minus <*> natural
  where
    minus = (char '-' >> return (-1)) <|> return 1


intList = bracket "[" "]" $ sepBy (token integer) (symbol ",")

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

complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
	a <- token float
	char ','
	i <- token float
	return (a, i)