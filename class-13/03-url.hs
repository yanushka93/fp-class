import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.List
import Data.Maybe

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL‐путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show

type Login = String
type Password = String
type Server = String
type Port = String
type Path = String
type Params = String
type Anchor = String

data URL = URL Scheme (Login, Password) Server Port Path Params Anchor
           deriving Show

scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

user = userWithPass <|> userWithoutPass
	
userWithoutPass = do
	login <- many1 (sat (/= '@'))
	char '@'
	return (login, "")
	
userWithPass = do
	login <- many1 (sat (/= ':'))
	char ':'
	password <- many1 (sat (/= '@'))
	char '@'
	return (login, password)
	
server :: Parser Server
server = many1 (sat (\c -> isNothing $ elemIndex c ":/"))

port :: Parser Port
port = char ':' >> many (sat (/= '/'))

path :: Parser Path
path = char '/' >> many1 (sat (\c -> isNothing $ elemIndex c "?#"))

params :: Parser Params
params = char '?' >> many1 (sat (/= '#'))

anchor :: Parser Anchor
anchor = char '#' >> many (sat $ const True)

url = URL <$>
	scheme <*>
	(string "://" >> optional ("", "") user) <*>
	server <*>
	(optional "" port) <*>
	(optional "" path) <*>
	(optional "" params) <*>
	(optional "" anchor)
	  
      
