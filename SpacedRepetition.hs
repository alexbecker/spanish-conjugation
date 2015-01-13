-- Implements bucket-based spaced repetition
module SpacedRepetition where

import Data.Tuple
import Data.Maybe
import Data.List (findIndex, (!!))
import System.Random
import Control.Monad

type Card = (Int, IO Bool)	-- (Id, Question)

-- Just an ordered tree
data Bucket = Empty
			| Leaf Card
			| Node Int Int Int Bucket Bucket	-- (numChildren, leftMax, rightMin, left, right)

instance Show Bucket where
	show Empty = "Empty"
	show (Leaf a) = show $ getId a
	show (Node i j k a b) = "(" ++ show a ++ ") (" ++ show b ++ ")"

type Probability = Double
type GameState = ([Bucket], [Probability])

getId :: Card -> Int
getId (a, b) = a

isEmpty :: Bucket -> Bool
isEmpty Empty = True
isEmpty _ = False

isLeaf :: Bucket -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

numChildren :: Bucket -> Int
numChildren Empty = 0
numChildren (Leaf _) = 1
numChildren (Node i _ _ _ _) = i

left :: Bucket -> Bucket
left (Node _ _ _ a _) = a

right :: Bucket -> Bucket
right (Node _ _ _ _ b) = b

bmax :: Bucket -> Card
bmax (Leaf a) = a
bmax (Node _ _ _ _ b) = bmax b

secondMaxId :: Bucket -> Int
secondMaxId (Node _ _ _ a (Leaf b)) = getId $ bmax a
secondMaxId (Node _ _ _ _ b) = secondMaxId b

bmin :: Bucket -> Card
bmin (Leaf a) = a
bmin (Node _ _ _ a _) = bmin a

secondMinId :: Bucket -> Int
secondMinId (Node _ _ _ (Leaf a) b) = getId $ bmax b
secondMinId (Node _ _ _ a _) = secondMinId a

idList :: Bucket -> [Int]
idList Empty = []
idList (Leaf a) = [getId a]
idList (Node _ _ _ a b) = idList a ++ idList b

-- Average time O(log n)
insert :: Card -> Bucket -> IO Bucket
insert a Empty = return $ Leaf a
insert a (Leaf x) = return $ if getId a < getId x
	then Node 2 (getId a) (getId x) (Leaf a) (Leaf x)
	else Node 2 (getId x) (getId a) (Leaf x) (Leaf a)
insert a (Node i j k x y) = if getId a < j
	then do
		leftSide <- insert a x
		return $ Node (i + 1) j k leftSide y
	else if getId a > k
		then do
			rightSide <- insert a y
			return $ Node (i + 1) j k x rightSide
		else do
			randInt <- getStdRandom $ randomR (1, 2) :: IO Int
			if randInt == 1
				then do
					leftSide <- insert a x
					return $ Node (i + 1) (getId a) k leftSide y
				else do
					rightSide <- insert a y
					return $ Node (i + 1) j (getId a) x rightSide

testInsert :: IO Bucket
testInsert = do
	foldM (flip insert) Empty $ map (\x -> (x, return True)) [1,8,7,4,2,10,5,6,3,9]

-- Average time O(log n)
delete :: Card -> Bucket -> Bucket
delete a (Leaf x) = if getId a == getId x
	then Empty
	else undefined
delete a (Node _ _ _ (Leaf x) (Leaf y)) = if getId a == getId x
	then Leaf y
	else if getId a == getId y
		then Leaf x
		else undefined
delete a (Node i j k x (Leaf y)) = if getId a == getId y
	then x
	else if getId a == j
		then Node (i - 1) (secondMaxId x) k (delete a x) (Leaf y)
		else Node (i - 1) j k (delete a x) (Leaf y)
delete a (Node i j k (Leaf x) y) = if getId a == getId x
	then y
	else if getId a == k
		then Node (i - 1) j (secondMinId y) (Leaf x) (delete a y)
		else Node (i - 1) j k (Leaf x) (delete a y)
delete a (Node i j k x y) = if getId a == j
	then Node (i - 1) (secondMaxId x) k (delete a x) y
	else if getId a == k
		then Node (i - 1) j (secondMinId y) x (delete a y)
		else if getId a < j
			then Node (i - 1) j k (delete a x) y
			else Node (i - 1) j k x (delete a y)

testDelete :: Int -> IO Bucket
testDelete n = do 
	x <- testInsert
	return $ delete (n, return True) x

-- Draw random element
drawRand :: Bucket -> IO Card
drawRand (Leaf a) = return a
drawRand (Node i _ _ x y) = do
	randInt <- getStdRandom $ randomR (1, i) :: IO Int
	if randInt <= numChildren x
		then drawRand x
		else drawRand y

listGameState :: GameState -> ([[Int]], [Probability])
listGameState gs = (map idList $ fst gs, snd gs)

tryCard :: GameState -> IO GameState
tryCard gs = do
	let buckets = fst gs
	let probs = snd gs
	-- get bucket
	randDouble <- getStdRandom $ randomR (0.0, 1.0) :: IO Double
	let cumulativeProbs = scanl1 (+) probs
	let index = fromJust $ findIndex (> randDouble) cumulativeProbs
	let bucket = buckets !! index
	if isEmpty bucket
		then tryCard gs
		else do
			-- test user
			randCard <- drawRand bucket
			correct <- snd randCard
			-- change buckets
			if correct && index + 1 < length buckets
				then do
					let oldBucket = delete randCard bucket
					newBucket <- insert randCard $ buckets !! (index + 1)
					return $ (take index buckets ++ [oldBucket, newBucket] ++ drop (index + 2) buckets, probs)
				else if not correct && index > 0
					then do
						let oldBucket = delete randCard bucket
						newBucket <- insert randCard $ buckets !! 0
						return $ ([newBucket] ++ tail (take index buckets) ++ [oldBucket] ++ drop (index + 1) buckets, probs)
					else
						return gs

playGame :: GameState -> IO ()
playGame gs = do
	gs' <- tryCard gs
	playGame gs'

playGamePrompt :: GameState -> IO GameState
playGamePrompt gs = do
	gs' <- tryCard gs
	putStr "Coninue? "
	cont <- getChar
	putStrLn ""
	if cont == 'y'
		then playGamePrompt gs'
		else return gs'

buildGameState :: [Card] -> [Probability] -> IO GameState
buildGameState cs ps = do
	firstBucket <- foldM (flip insert) Empty cs
	return ([firstBucket] ++ replicate (length ps - 1) Empty, ps)

testCard :: Int -> Card
testCard n = (n, do
	putStr $ "Card " ++ show n ++ ", Enter 'y': "
	response <- getChar
	putStrLn ""
	return $ response == 'y')

testGame :: Int -> IO GameState
testGame n = buildGameState (map testCard [1..n]) [0.75, 0.2, 0.05]
