{-
Horse race card game
Anton Eklund, Axel Nygårds, David Smeds
-}
import System.Random
import Data.Array.IO
import Control.Monad
import Control.Exception

{- REPRESENTATION CONVENTION: Represents a playing card's suit if it isn't None. 
   REPRESENTATION INVARIANT: A suit is only None if a player has no bet placed. -}
data Suit = None | Spade | Heart | Club | Diamond deriving(Show,Eq)

{- REPRESENTATION CONVENTION: Represents a playing card's value. Does not contain aces as the game uses them as horses.
   REPRESENTATION INVARIANT: -}
data Value = King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving(Show,Eq)

{- REPRESENTATION CONVENTION: Represents a playing card by its suit and value.
   REPRESENTATION INVARIANT:  Must contain a suit and value. There can only be unique combinations of suit and value in a deck. -}
type Card = (Suit,Value)
{- REPRESENTATION CONVENTION: The players name, bet amount and betted suit is represented in that order.
   REPRESENTATION INVARIANT: String should not be empty (no name), The Int and Suit should not be 0 & None if a bet is placed. -}
type Player = (String,Int,Suit)

{- REPRESENTATION CONVENTION: The current state of the game is represented by five integers, the first four represents the amount of
steps that the horses have taken and the last one represents how far the last 
   REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ... -}
type GameState = (Int,Int,Int,Int,Int)

newgame = (0,0,0,0,0)
values = [King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]
{- 	createDeck
	s is suit, v is value and r is result
	POST: A deck with all four suits and one card for each value in that suit.
-}
createDeck :: [Card]
createDeck = createDeckAux [Club, Diamond, Heart, Spade] values values []

createDeckAux :: [Suit] -> [Value] -> [Value] -> [Card] -> [Card]
createDeckAux (x:xs) [] v r | x == Club = createDeckAux xs v v r 
createDeckAux (x:xs) (l:ls) v r | x == Club = createDeckAux (x:xs) ls v ((x,l):r)
createDeckAux (x:xs) [] v r | x == Diamond = createDeckAux xs v v r 
createDeckAux (x:xs) (l:ls) v r | x == Diamond = createDeckAux (x:xs) ls v ((x,l):r)
createDeckAux (x:xs) [] v r | x == Heart = createDeckAux xs v v r 
createDeckAux (x:xs) (l:ls) v r | x == Heart = createDeckAux (x:xs) ls v ((x,l):r)
createDeckAux (x:xs) [] v r | x == Spade = r
createDeckAux (x:xs) (l:ls) v r | x == Spade = createDeckAux (x:xs) ls v ((x,l):r)


{-	shuffle d
	POST: List of cards where the position of the cards has been randomized. 
	COMMENT: copied from https://wiki.haskell.org/Random_shuffle
	-}

translateSuit :: Suit -> String
translateSuit s
  | s == Spade = "Spades"
  | s == Heart = "Hearts"
  | s == Diamond = "Diamonds"
  | otherwise = "Clubs"

translateValue :: Value -> String
translateValue v
  | v == King = "King"
  | v == Queen = "Queen"
  | v == Jack = "Jack"
  | v == Ten = "Ten"
  | v == Nine = "Nine"
  | v == Eight = "Eight"
  | v == Seven = "Seven"
  | v == Six = "Six"
  | v == Five = "Five"
  | v == Four = "Four"
  | v == Three = "Three"
  | otherwise = "Two"

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

	--createPlayers 
	--POST: A list with all players playing


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
createPlayers :: IO [Player]
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
placeBets :: [Player] -> IO [Player]
placeBets plist = do 
 bets <- forM [1..(length plist)] (\p -> do
  putStrLn ("Player " ++ (show p) ++ ", how many sips?")
  sips <- getLine
  putStrLn ("Which suit? c for Clubs, d for Diamonds, h for Hearts, s for Spades")
  bet <- getLine
  return ((read sips :: Int),(returnSuit bet))) -- Ändra till lista av (ints, suits) och uppdatera från båda
 return (placeBets' plist [] bets)

placeBets' [] save [] = save
placeBets' ((p,b,s):pl) save ((x,y):xs) = placeBets' pl (save ++ [(p,x,y)]) xs

returnSuit bet
 | bet == "c" = Club
 | bet == "d" = Diamond
 | bet == "h" = Heart
 | bet == "s" = Spade
 | otherwise = None



createBoard (q,w,e,r,p) = do
 putStrLn " _________"
 putStrLn (line1 q)
 putStrLn (line2 w)
 putStrLn (line3 e)
 putStrLn (line4 r)
 putStrLn (line5 p)
 return (q,w,e,r,p)
line1 q 
 | q==0 = "│H       │"
 | q==1 = "│ H      │"
 | q==2 = "│  H     │"
 | q==3 = "│   H    │"
 | q==4 = "│    H   │"
 | q==5 = "│     H  │"
 | q==6 = "│      H │"
 | q==7 = "│       H│"
 | q==8 = "│        │H"
 | otherwise = "│H"
line2 w 
 | w==0 = "│D       │"
 | w==1 = "│ D      │"
 | w==2 = "│  D     │"
 | w==3 = "│   D    │"
 | w==4 = "│    D   │"
 | w==5 = "│     D  │"
 | w==6 = "│      D │"
 | w==7 = "│       D│"
 | w==8 = "│        │D"
 | otherwise = "│D"
line3 e 
 | e==0 = "│S       │"
 | e==1 = "│ S      │"
 | e==2 = "│  S     │"
 | e==3 = "│   S    │"
 | e==4 = "│    S   │"
 | e==5 = "│     S  │"
 | e==6 = "│      S │"
 | e==7 = "│       S│"
 | e==8 = "│        │S"
 | otherwise = "│S"
line4 r 
 | r==0 = "│C       │"
 | r==1 = "│ C      │"
 | r==2 = "│  C     │"
 | r==3 = "│   C    │"
 | r==4 = "│    C   │"
 | r==5 = "│     C  │"
 | r==6 = "│      C │"
 | r==7 = "│       C│"
 | r==8 = "│        │C"
 | otherwise = "│C"
line5 p 
 | p==0 = "│ ???????"
 | p==1 = "│ x??????"
 | p==2 = "│ xx?????"
 | p==3 = "│ xxx????"
 | p==4 = "│ xxxx???"
 | p==5 = "│ xxxxx??"
 | p==6 = "│ xxxxxx?"
 | p==7 = "│ xxxxxxx"
 | otherwise = "│?"
 {-	printWinners players suit
	PURPOSE: Checks which players that betted on the right suit and prints them as winners.
	POST: Prints the winners as string in console or as graphics.
-}

printWinners :: Suit-> GameState -> IO Suit
printWinners t state = do
 k <- createBoard state
 putStrLn ((show t) ++ " has won!")
 return t
{-	newCard d
	PURPOSE: Draws one card from the deck, checks the suit and moves the same suit Ace.
	POST: Winner suit  
-}
newCard :: [Card] -> GameState -> GameState
newCard ((x,v):xs) (q,w,e,r,p)
 | q >= 1 && w >= 1 && e >= 1 && r >= 1 && p==0 = newCardMinus ((x,v):xs) q w e r (p+1)
 | q >= 2 && w >= 2 && e >= 2 && r >= 2 && p==1 = newCardMinus ((x,v):xs) q w e r (p+1)
 | q >= 3 && w >= 3 && e >= 3 && r >= 3 && p==2 = newCardMinus ((x,v):xs) q w e r (p+1)
 | q >= 4 && w >= 4 && e >= 4 && r >= 4 && p==3 = newCardMinus ((x,v):xs) q w e r (p+1)
 | q >= 5 && w >= 5 && e >= 5 && r >= 5 && p==4 = newCardMinus ((x,v):xs) q w e r (p+1)
 | q >= 6 && w >= 6 && e >= 6 && r >= 6 && p==5 = newCardMinus ((x,v):xs) q w e r (p+1)
 | q >= 7 && w >= 7 && e >= 7 && r >= 7 && p==6 = newCardMinus ((x,v):xs) q w e r (p+1)
 | x == Heart = ((q+1),w,e,r,p)    
 | x == Diamond = (q,(w+1),e,r,p)      
 | x == Spade =  (q,w,(e+1),r,p)       
 | x == Club = (q,w,e,(r+1),p)   
 | otherwise = (q,w,e,r,p)

newCardMinus :: [Card] -> Int -> Int -> Int -> Int -> Int -> GameState
newCardMinus ((x,v):xs) q w e r p
	| x == Heart = ((q-1),w,e,r,p) 
	| x == Diamond = (q,(w-1),e,r,p)  
	| x == Spade = (q,w,(e-1),r,p)  
	| x == Club = (q,w,e,(r-1),p)
	| otherwise = (q,w,e,r,p)


horseRace :: [Card] -> GameState -> IO Suit
horseRace cards (q,w,e,r,p)  
 | q==8 = printWinners Heart (q,w,e,r,p)
 | w==8 = printWinners Diamond (q,w,e,r,p)
 | e==8 = printWinners Spade (q,w,e,r,p)
 | r==8 = printWinners Club (q,w,e,r,p)
horseRace ((x,v):xs) gamestate = do
	t <- (createBoard gamestate)
	putStrLn "Press any button to draw the next card."
	k <- getLine
	putStrLn ("The card was: " ++ (translateValue v)  ++ " of "  ++ (translateSuit x))
 	horseRace xs (newCard ((x,v):xs) gamestate)


showWinners :: [Player] -> Suit -> String
showWinners pl s = showWinners' pl s ""
	where 
		showWinners' [] s winners = "The winners are: " ++ winners
		showWinners' ((p,b,s1):pl) s winners | s1 == s = showWinners' pl s (winners ++ p ++" with " ++ show (b*2)++" sips, ") 
											   | otherwise = showWinners' pl s winners

main :: IO ()
main = do
 putStrLn "HorseRace, by Anton, Axel & David"
 players <- createPlayers
 playGame players

playGame :: [Player] -> IO ()
playGame players = do
	playerbets <- placeBets players
	thedeck <- shuffle createDeck
	win <- horseRace thedeck newgame
	putStrLn (showWinners playerbets win)
	putStrLn "Do you want to play again?"
	a <- getLine
	if a == "yes" then do
		putStrLn "With the same players?"
		b <- getLine
		if b == "yes" then playGame players else main
	else putStrLn "Okay, bye!"
	return ()

