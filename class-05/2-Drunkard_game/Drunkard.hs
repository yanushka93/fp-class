{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades 
	| Clubs 
	| Diamonds 
	| Hearts
	deriving (Show, Eq, Ord)


data Value = Two
	| Three
	| Four
	| Five
	| Six
	| Seven 
	| Eight 
	| Nine 
	| Ten 
	| Jack 
	| Queen 
	| King 
	| Ace
	deriving (Show, Eq, Ord)


data Card = Card Value Suit
	deriving (Show, Eq)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) = s1 == s2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _) = compare v1 v2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round (deck1, deck2) = play_round (deck1, deck2) []
	where
		play_round (x:xs, y:ys) deck
			| beats x y == GT = (xs ++ deck ++ [x, y], ys)
			| beats x y == LT = (xs, ys ++ deck ++ [x, y])
			| otherwise = play_round (xs, ys) (deck ++ [x, y])

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
	deriving (Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game (deck1, deck2) = play_game 0 (deck1, deck2)
	where
		play_game rounds (_, []) = (First, rounds)
		play_game rounds ([], _) = (Second, rounds)
		play_game rounds (d1, d2) = play_game (rounds + 1) $ game_round (d1, d2)
		

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

test_game_1 = game ([(Card Ace Clubs), (Card Ace Hearts), (Card King Spades), (Card King Diamonds), (Card Six Spades), (Card Seven Hearts), (Card Jack Spades), (Card Queen Clubs), (Card Ten Hearts), (Card Ace Spades)], [(Card Three Diamonds), (Card Four Clubs), (Card Three Spades),(Card Two Diamonds),(Card Four Diamonds),(Card Five Diamonds),(Card Two Spades),(Card Five Hearts),(Card Five Spades),(Card Five Diamonds)]) == (First, 10)
test_game_2 = game ([(Card Ten Clubs), (Card King Hearts), (Card King Spades), (Card Ace Diamonds), (Card Six Spades), (Card Seven Hearts), (Card Jack Spades), (Card Queen Clubs), (Card Ten Hearts), (Card Ace Spades)], [(Card Ten Diamonds), (Card Four Clubs), (Card Three Spades),(Card Eight Diamonds),(Card Four Diamonds),(Card Five Diamonds),(Card Three Spades),(Card Five Hearts),(Card Five Spades),(Card Two Diamonds)]) == (First, 9)
test_game_3 = game ([(Card Eight Diamonds), (Card Four Clubs), (Card Three Spades),(Card King Diamonds),(Card Ten Diamonds),(Card Five Diamonds),(Card Three Spades),(Card Jack Hearts),(Card Nine Spades),(Card Queen Diamonds)], [(Card Nine Clubs), (Card Five Hearts), (Card Four Spades), (Card Ace Diamonds), (Card Jack Spades), (Card Six Hearts), (Card Jack Spades), (Card Queen Clubs), (Card Ten Hearts), (Card King Spades)]) == (Second, 10)
test_game_4 = game ([(Card Eight Diamonds), (Card Four Clubs), (Card Three Spades),(Card King Diamonds),(Card Ten Diamonds),(Card Five Diamonds),(Card Three Spades),(Card Jack Hearts),(Card Nine Spades),(Card Queen Diamonds)], [(Card Eight Clubs), (Card Five Hearts), (Card Four Spades), (Card Ace Diamonds), (Card Jack Spades), (Card Five Hearts), (Card Jack Spades), (Card Queen Clubs), (Card Ten Hearts), (Card King Spades)]) == (Second, 8)
test_game_5 = game ([(Card Eight Diamonds), (Card Four Clubs), (Card Three Spades),(Card King Diamonds),(Card Ten Diamonds),(Card Five Diamonds),(Card Three Spades),(Card Jack Hearts),(Card Nine Spades),(Card Queen Diamonds)], [(Card Eight Clubs), (Card Four Hearts), (Card Three Spades), (Card Ace Diamonds), (Card Jack Spades), (Card Five Hearts), (Card Jack Spades), (Card Queen Clubs), (Card Ten Hearts), (Card King Spades)]) == (Second, 6)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
