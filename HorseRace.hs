{-
Horse race card game
Anton Eklund, Axel Nygårds, David Smeds
-}
import System.Random
import Data.Array.IO
import Control.Monad
import Prelude hiding(catch)
import Control.Exception

data Suit = None | Spade | Heart | Club | Diamond deriving(Show,Eq)
data Value = King | Queen | Knight | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving(Show,Eq)

type Card = (Suit,Value)

type Player = (String,Int,Suit)

type GameState = (Int,Int,Int,Int,Int)

newgame = (0,0,0,0,0)
values = [King, Queen, Knight, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]
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
{- 
createDeck :: [Card]
createDeck = (addValue Spade) ++ (addValue Heart) ++ (addValue Club) ++ (addValue Diamond)
addValue :: Suit -> [Card]
addValue s = [(s,King)] ++ [(s,Queen)] ++ [(s,Knight)] ++ [(s,Ten)] ++ [(s,Nine)] ++ [(s,Eight)] ++ 
		[(s,Seven)] ++ [(s,Six)] ++ [(s,Five)] ++ [(s,Four)] ++ [(s,Three)] ++ [(s,Two)]
-}

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



---inputPlayerNames (read players :: Int) 1 []
{--
inputPlayerNames i acc plist | acc == i = return plist
 | otherwise = do 
 	putStrLn ("Player " ++ (show acc) ++ ", please input your name")
 	p <- getLine
    inputPlayerNames i (acc+1) ((p,0,None):plist)
--}
--How many players? 1-8
--Input 
--Player 1 name = 
--Player 2 name = etc
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

 --(inputPlayerNames players plist)


--inputPlayerNames [] plist = plist
--inputPlayerNames (x:xs) ([(a,b,c):ps]) = inputPlayerNames xs ([(a,x,c)]:ps)

--placeBets :: [Player] -> [Player]



{--

placeBets :: [Player] -> [Player]
placeBets pl = placeBets' pl []
	where 
		placeBets' [] save = save
		placeBets' ((p,b,s):pl) save = placeBets' pl (s ++ do
		i <- readBet
		return [(p,i,Spade)])
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
-}
--createBoard :: [Card] -> String/Graphics -- här väljer vi om vi ska köra i konsolen eller om vi ska ha ngt grafiskt
createBoard (q,w,e,r,p) = do
	putStrLn "_______"
	putStrLn (line1 q)
	putStrLn (line2 w)
	putStrLn (line3 e)
	putStrLn (line4 r)
	putStrLn "│???????"
	putStrLn "_______"
	return (q,w,e,r)
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
 	horseRace xs (newCard ((x,v):xs) gamestate)

printWinners1 :: [Player] -> Suit -> String
printWinners1 pl s = printWinners1' pl s ""
	where 
		printWinners1' [] s winners = "The winners are: " ++ winners
		printWinners1' ((p,b,s1):pl) s winners | s1 == s = printWinners1' pl s (winners ++ p ++" with " ++ show (b*2)++" klunks, ") 
											   | otherwise = printWinners1' pl s winners

{- Alternativ vinnarfunktion
showWinner p w = theWinners (won p w [])

theWinners [] = return []
theWinners ((x,y,z):xs) = do
	putStrLn (x ++ ", du får dela ut " ++ (show (y*2)) ++ " klunkar.")
	theWinners xs

won [] b ls = ls
won ((x,y,z):xs) b ls
 | z == b = won xs b ((x,y,z):ls)
 | otherwise = won xs b ls -}

main = do
 putStrLn "HorseRace, by Anton, Axel & David"
 players <- createPlayers
 playGame players


playGame players = do
	playerbets <- placeBets players
	thedeck <- shuffle createDeck
	win <- horseRace thedeck newgame
	putStrLn (printWinners1 playerbets win)
	--showWinner playerbets win
	putStrLn "Do you want to play again?"
	a <- getLine
	if a == "yes" then do
		putStrLn "With the same players?"
		b <- getLine
		if b == "yes" then playGame players else main
	else putStrLn "Okay, bye!"
	return ()

