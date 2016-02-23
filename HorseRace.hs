{-
Horse race card game
Anton Eklund, Axel Nygårds, David Smeds

-}

data Suit = None | Spade | Heart | Clove | Diamond 
	deriving(Show,Eq)
data Value = King | Queen | Knight | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two 
	deriving(Show,Eq)

type Card = (Suit,Value)

type Player = (String,Int,Suit)

values = [King, Queen, Knight, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]
{- 	createDeck
	s is suit, v is value and r is result
	POST: A deck with all four suits and one card for each value in that suit.

-}
createDeck :: [Card]
createDeck = createDeckAux [Clove, Diamond, Heart, Spade] values values []

createDeckAux :: [Suit] -> [Value] -> [Value] -> [Card] -> [Card]
createDeckAux (x:xs) [] v r | x == Clove = createDeckAux xs v v r 
createDeckAux (x:xs) (l:ls) v r | x == Clove = createDeckAux (x:xs) ls v ((x,l):r)
createDeckAux (x:xs) [] v r | x == Diamond = createDeckAux xs v v r 
createDeckAux (x:xs) (l:ls) v r | x == Diamond = createDeckAux (x:xs) ls v ((x,l):r)
createDeckAux (x:xs) [] v r | x == Heart = createDeckAux xs v v r 
createDeckAux (x:xs) (l:ls) v r | x == Heart = createDeckAux (x:xs) ls v ((x,l):r)
createDeckAux (x:xs) [] v r | x == Spade = r
createDeckAux (x:xs) (l:ls) v r | x == Spade = createDeckAux (x:xs) ls v ((x,l):r)
{--


{-	shuffle d
	POST: List of cards where the position of the cards has been randomized. 

-}
shuffle :: [Card] -> [Card]
shuffle = undefined

{-	createPlayers ui
	POST: A list with all players playing 

-}
createPlayers :: String(user input??) -> [Player]
createPlayers = undefined

How many players? 1-8
Input 
Player 1 name = 
Player 2 name = etc


{-	placeBets p
	POST: List modified so Player now has a number and a suit which represents the bet.

-}
placeBets :: [Player] -> [Player]
placeBets = undefined

{-	createBoard d 
	POST: 7 face down cards on the side and all aces represents the different suits

-}
createBoard :: [Card] -> String/Graphics -- här väljer vi om vi ska köra i konsolen eller om vi ska ha ngt grafiskt
createBoard = undefined

{-	printWinners players suit
	PURPOSE: Checks which players that betted on the right suit and prints them as winners.
	POST: Prints the winners as string in console or as graphics.
-}
printWinners :: [Player]-> Suit -> String/Graphics
printWinners = undefined

{-	newCard d
	PURPOSE: Draws one card from the deck, checks the suit and moves the same suit Ace.
	POST: Winner suit  
-}
newCard :: [Card] -> Suit
newCard = undefined

playAgain :: userinput -> Bool
playAgain = undefined


playGame :: ?? 
players = createPlayers
while?? sålängemanvillspela
	deck = shuffle createDeck
	placeBets players
	createBoard deck 
	printWinners players (newCard deck) 
	playAgain


--}






