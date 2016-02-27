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


-Alternativ placeBets för klunkar
pre: List of players (true)
examples:
placeBets [("Kalle",0,None),("Gurle",0,None)]
Player 1, how many klunks?
3
Player 2, how many klunks?
5
[("Kalle",3,None),("Gurle",5,None)]
-}

placeBets :: [Player] -> IO [Player]
placeBets plist = do 
 bets <- forM [1..(length plist)] (\p -> do
  putStrLn ("Player " ++ (show p) ++ ", how many klunks?")
  klunks <- getLine
  putStrLn ("Which suit? c for Clubs, d for Diamonds, h for Hearts, s for Spades")
  bet <- getLine
  return ((read klunks :: Int),(returnSuit bet)))
 return (placeBets' plist [] bets)

placeBets' [] save [] = save
placeBets' ((p,b,s):pl) save ((x,y):xs) = placeBets' pl (save ++ [(p,x,y)]) xs

returnSuit bet
 | bet == "c" = Clove
 | bet == "d" = Diamond
 | bet == "h" = Heart
 | bet == "s" = Spade
 | otherwise = None
{-
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
-}
readSuit :: IO String
readSuit = undefined

{-	createBoard d 
	POST: 7 face down cards on the side and all aces represents the different suits

-}
--createBoard :: [Card] -> String/Graphics -- här väljer vi om vi ska köra i konsolen eller om vi ska ha ngt grafiskt
createBoard ((x,v):xs) q w e r p = do
	putStrLn "_______"
	putStrLn (line1 q)
	putStrLn (line2 w)
	putStrLn (line3 e)
	putStrLn (line4 r)
	putStrLn "│???????"
	putStrLn "_______"

line1 q 
	| q==(-1) = line1 0
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
	| w==(-1) = line2 0
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
	| e==(-1) = line3 0
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
	| r==(-1) = line4 0
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
newCard ((x,v):xs) q w e r p
    | q==8 = printWinners ("Hearts has won!")
    | w==8 = printWinners ("Diamonds has won!")
    | e==8 = printWinners ("Spades has won!")
    | r==8 = printWinners ("Clubs has won!")
    | q >= 1 && w >= 1 && e >= 1 && r >= 1 && p==0 = newCardMinus ((x,v):xs) q w e r (p+1)
    | q >= 2 && w >= 2 && e >= 2 && r >= 2 && p==1 = newCardMinus ((x,v):xs) q w e r (p+1)
    | q >= 3 && w >= 3 && e >= 3 && r >= 3 && p==2 = newCardMinus ((x,v):xs) q w e r (p+1)
    | q >= 4 && w >= 4 && e >= 4 && r >= 4 && p==3 = newCardMinus ((x,v):xs) q w e r (p+1)
    | q >= 5 && w >= 5 && e >= 5 && r >= 5 && p==4 = newCardMinus ((x,v):xs) q w e r (p+1)
    | q >= 6 && w >= 6 && e >= 6 && r >= 6 && p==5 = newCardMinus ((x,v):xs) q w e r (p+1)
    | q >= 7 && w >= 7 && e >= 7 && r >= 7 && p==6 = newCardMinus ((x,v):xs) q w e r (p+1)
	| x == Heart = createBoard xs (q+1) w e r p 
	| x == Diamond = createBoard xs q (w+1) e r p  
	| x == Spade = createBoard xs q w (e+1) r p  
	| x == Clove = createBoard xs q w e (r+1) p
	| otherwise = createBoard xs q w e r p

newCardMinus ((x,v):xs) q w e r p
	| x == Heart = createBoard xs (q-1) w e r p 
	| x == Diamond = createBoard xs q (w-1) e r p  
	| x == Spade = createBoard xs q w (e-1) r p  
	| x == Clove = createBoard xs q w e (r-1) p
	| otherwise = createBoard xs q w e r p

--playAgain :: userinput -> Bool
--playAgain = undefined


main = do
 putStrLn "HorseRace, by Anton, Axel & David"
 players <- createPlayers
 playGame players

playGame players = do
	playerbets <- placeBets players
	return playerbets
	newCard createDeck 0 0 0 0 -- Add shuffle
	putStrLn "Do you want to play again?"
	a <- getLine
	if a == "yes" then do
		putStrLn "With the same players?"
		b <- getLine
		if b == "yes" then playGame players else main
	else putStrLn "Okay, bye!"
	return ()
	--createBoard deck 
	--printWinners players (newCard deck) 
	--playAgain


--}






