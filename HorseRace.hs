{-
Horse race card game
Anton Eklund, Axel Nygårds, David Smeds

-}
--import System.Random
import Data.Array.IO
import Control.Monad
import Prelude hiding(catch)
import Control.Exception

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
{- 
createDeck :: [Card]
createDeck = (addValue Spade) ++ (addValue Heart) ++ (addValue Club) ++ (addValue Diamond)

addValue :: Suit -> [Card]
addValue s = [(s,King)] ++ [(s,Queen)] ++ [(s,Knight)] ++ [(s,Ten)] ++ [(s,Nine)] ++ [(s,Eight)] ++ 
		[(s,Seven)] ++ [(s,Six)] ++ [(s,Five)] ++ [(s,Four)] ++ [(s,Three)] ++ [(s,Two)]
-}
{--


{-	shuffle d
	POST: List of cards where the position of the cards has been randomized. 
	COMMENT: copied from https://wiki.haskell.org/Random_shuffle
-}
shuffle :: [a] -> IO [a]
shuffle d = do
        ar <- newArray n d
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length d
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n d =  newListArray (1,n) d

{-	createPlayers ui
	POST: A list with all players playing 
-}
-}
createPlayers :: IO [Player]
{-
examples: createPlayers
How many players?
3
Player 1, please input your name
Alex
Player 2, please input your name
Berra
Player 3, please input your name
Carro
[("Alex",0,None),("Berra",0,None),("Carro",0,None)]
--}
createPlayers = do 
 putStrLn "How many players?"
 number <- getLine
 players <- forM [1..(read number :: Int)] (\p -> do
  putStrLn ("Player " ++ (show p) ++ ", please input your name")
  player <- getLine
  return player)
 return (inputPlayerNames players [])

inputPlayerNames [] ls = ls
inputPlayerNames (x:xs) ls = inputPlayerNames xs (ls ++ [(x,0,None)])

{-	placeBets p
	POST: List modified so Player now has a number and a suit which represents the bet.

-}

placeBets :: [Player] -> [Player]
placeBets pl = placeBets' pl []
	where 
		placeBets' [] save = save
		placeBets' ((p,b,s):pl) save = placeBets pl (s ++ [(p,readBet,Spade)])


readBet :: IO Int
readBet = do
  catch (do
    line <- getLine 
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
       putStrLn "Invalid input. Correct format: amount"
       readBet) :: SomeException -> IO Int)

readSuit :: IO String
readSuit = undefined

{-	createBoard d 
	POST: 7 face down cards on the side and all aces represents the different suits

-}
createBoard :: [Card] -> String/Graphics -- här väljer vi om vi ska köra i konsolen eller om vi ska ha ngt grafiskt
createBoard q w e r = do
	putStrLn "_______"
	putStrLn (line1 q)
	putStrLn (line2 w)
	putStrLn (line3 e)
	putStrLn (line4 r)
	putStrLn "│???????"
	putStrLn "_______"

line1 q 
	| q==0 = "│H"
	| q==1 = "│ H"
	| q==2 = "│  H"
	| q==3 = "│   H"
	| q==4 = "│    H"
	| q==5 = "│     H"
	| q==6 = "│      H"
	| q==7 = "│       H"
	| otherwise = "|H"

line2 w 
	| w==0 = "│D"
	| w==1 = "│ D"
	| w==2 = "│  D"
	| w==3 = "│   D"
	| w==4 = "│    D"
	| w==5 = "│     D"
	| w==6 = "│      D"
	| w==7 = "│       D"
	| otherwise = "|D"

line3 e 
	| e==0 = "│S"
	| e==1 = "│ S"
	| e==2 = "│  S"
	| e==3 = "│   S"
	| e==4 = "│    S"
	| e==5 = "│     S"
	| e==6 = "│      S"
	| e==7 = "│       S"
	| otherwise = "|S"

line4 r 
	| r==0 = "│C"
	| r==1 = "│ C"
	| r==2 = "│  C"
	| r==3 = "│   C"
	| r==4 = "│    C"
	| r==5 = "│     C"
	| r==6 = "│      C"
	| r==7 = "│       C"
	| otherwise = "|C"

{-	printWinners players suit
	PURPOSE: Checks which players that betted on the right suit and prints them as winners.
	POST: Prints the winners as string in console or as graphics.
-}
--printWinners :: [Player]-> Suit -> String/Graphics
printWinners t = putStrLn (t)

{-	newCard d
	PURPOSE: Draws one card from the deck, checks the suit and moves the same suit Ace.
	POST: Winner suit  
-}
--newCard :: [Card] -> Suit
newCard ((x,v):xs) q w e r
    | q==8 = printWinners ("Hearts has won!")
    | w==8 = printWinners ("Diamonds has won!")
    | e==8 = printWinners ("Spades has won!")
    | r==8 = printWinners ("Clubs has won!")
	| x == Hearts = newCard xs (q+1) w e r        {- --$ createBoard (q+1) w e r -}
	| x == Diamonds = newCard xs q (w+1) e r       {- --$ createBoard q (w+1) e r  -}
	| x == Spades = newCard xs q w (e+1) r        {- --$ createBoard q w (e+1) r  -}
	| x == Clubs = newCard xs q w e (r+1)     {- --$ createBoard q w e (r+1)-}
	| otherwise = newCard xs q w e r

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






