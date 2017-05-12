import Data.List
import Data.List.Split
import System.Random


-- types

type Die = Int
type Score = Int


-- dice roll

rollDie :: IO Die
rollDie = randomRIO (1, 6)

rollDices :: IO [Die]
rollDices = rollnDices 5

rollnDices :: Int -> IO [Die]
rollnDices n = sequence (replicate n rollDie)

-- categories

--checkDices :: [Die] -> Maybe [(String, [Die])]

equalsOf :: Int -> [Die] -> Maybe [Die]
equalsOf n dices = if equals == [] then Nothing else Just equals
	where equals = filter (== n) dices

aces :: [Die] -> Maybe [Die]
aces = equalsOf 1

twos :: [Die] -> Maybe [Die]
twos = equalsOf 2

threes :: [Die] -> Maybe [Die]
threes = equalsOf 3

fours :: [Die] -> Maybe [Die]
fours = equalsOf 4

fives :: [Die] -> Maybe [Die]
fives = equalsOf 5

sixes :: [Die] -> Maybe [Die]
sixes = equalsOf 6

bonus = undefined

threeOfAKind :: [Die] -> Maybe [Die]
threeOfAKind dices = find (\xs -> length xs == 3) . group $ sort dices

fourOfAKind :: [Die] -> Maybe [Die]
fourOfAKind dices = find (\xs -> length xs == 4) . group $ sort dices

onePairs :: [Die] -> Maybe [[Die]]
onePairs dices = if pairs == [] then Nothing else Just pairs  
	where pairs = filter (\xs -> length xs == 2) . concatMap (chunksOf 2) . group $ sort dices

--twoPairs :: [Die] -> Maybe [[Die]]
--twoPairs dices = if pairs == [] then Nothing else Just pairs  
--	where pairs = filter (\xs -> length xs == 2) . concatMap (chunksOf 2) . group $ sort dices

fullHouse :: [Die] -> Maybe [Die]
fullHouse dices =
	if length groupedDices > 2 
		then Nothing
		else if any (\xs -> length xs == 1) groupedDices
				then Nothing
				else Just dices
	where groupedDices = group $ sort dices

smallStraight :: [Die] -> Maybe [Die]
smallStraight dices = if sort dices == smallStraight then Just smallStraight else Nothing 
	where smallStraight = [1..5]

largeStraight :: [Die] -> Maybe [Die]
largeStraight dices = if sort dices == largeStraight then Just largeStraight else Nothing 
	where largeStraight = [2..6]

yatzy :: [Die] -> Maybe [Die]
yatzy dices = if all (== head dices) dices then Just dices else Nothing


-- protocol

emptyProtocol :: [(String, Maybe Int)]
emptyProtocol =	[("Aces", 			 Nothing)
			 	,("Twos", 			 Nothing)
				,("Threes",			 Nothing)
				,("Fours",			 Nothing)
				,("Fives", 			 Nothing)
				,("Sixes", 			 Nothing)
				,("Bonus", 			 Nothing)
				,("Three-Of-A-Kind", Nothing)
				,("Four-Of-A-Kind",	 Nothing)
				,("Full House", 	 Nothing)
				,("Small Straight",  Nothing)
				,("Large Straight",  Nothing)]



--data Protocol = 

--newGame :: [String] -> Protocol
--newGame [] = error "It has to be atleast 1 or more players in Yatzy!"
--newGame names = 