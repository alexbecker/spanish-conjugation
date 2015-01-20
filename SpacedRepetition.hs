-- Implements bucket-based spaced repetition
module SpacedRepetition where

import Data.Maybe
import Data.List (find, findIndex, (!!))
import Data.Function
import System.Random
import Control.Monad
import ImmediateIO

class (Read a, Show a) => Card a where
	getId :: a -> Int
	getAction :: a -> IO Bool
	
-- Just an ordered tree
data Bucket a = Empty
			  | Leaf a
			  | Node Int Int Int (Bucket a) (Bucket a)	-- (numChildren, leftMax, rightMin, left, right)

type Probability = Double
type GameState a = ([Bucket a], [Probability])

instance (Show a) => Show (Bucket a) where
	show = show . flatten

-- Basic functions for Bucket type

isEmpty :: Bucket a -> Bool
isEmpty Empty = True
isEmpty _ = False

isLeaf :: Bucket a -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

numChildren :: Bucket a -> Int
numChildren Empty = 0
numChildren (Leaf _) = 1
numChildren (Node i _ _ _ _) = i

left :: Bucket a -> Bucket a
left (Node _ _ _ a _) = a

right :: Bucket a -> Bucket a
right (Node _ _ _ _ b) = b

bmax :: Bucket a -> a
bmax (Leaf a) = a
bmax (Node _ _ _ _ b) = bmax b

secondMaxId :: (Card a) => Bucket a -> Int
secondMaxId = getId . (!! 1) . reverse . flatten

bmin :: Bucket a -> a
bmin (Leaf a) = a
bmin (Node _ _ _ a _) = bmin a

secondMinId :: (Card a) => Bucket a -> Int
secondMinId = getId . (!! 1) . flatten

flatten :: Bucket a -> [a]
flatten Empty = []
flatten (Leaf a) = [a]
flatten (Node _ _ _ a b) = flatten a ++ flatten b

-- More complex functions for Bucket type
-- All average time O(log n)

insert :: (Card a) => a -> Bucket a -> IO (Bucket a)
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

delete :: (Card a) => a -> Bucket a -> Bucket a
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

drawRand :: (Card a) => Bucket a -> IO a
drawRand (Leaf a) = return a
drawRand (Node i _ _ x y) = do
	randInt <- getStdRandom $ randomR (1, i) :: IO Int
	if randInt <= numChildren x
		then drawRand x
		else drawRand y

-- Build a perfect bucket from a sorted list of cards

buildBucket :: (Card a) => [a] -> Bucket a
buildBucket [] = Empty
buildBucket [c] = Leaf c
buildBucket cs = Node (numChildren leftSide + numChildren rightSide) (getId $ bmax leftSide) (getId $ bmin rightSide) leftSide rightSide
	where
		splitcs = splitAt (quot (length cs) 2) cs
		leftSide = buildBucket $ fst splitcs
		rightSide = buildBucket $ snd splitcs

-- Functions for playing the game

tryCard :: (Card a) => GameState a -> IO (GameState a)
tryCard gs = do
	let buckets = fst gs
	-- renormalize probabilities
	let origProbs = snd gs
	let probsDenormalized = map (uncurry (*)) $ zip (map (fromIntegral . numChildren) buckets) origProbs
	let normFactor = sum probsDenormalized
	let probs = map (/ normFactor) probsDenormalized
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
			correct <- getAction randCard
			-- change buckets
			if correct && index + 1 < length buckets
				then do
					let oldBucket = delete randCard bucket
					newBucket <- insert randCard $ buckets !! (index + 1)
					return $ (take index buckets ++ [oldBucket, newBucket] ++ drop (index + 2) buckets, origProbs)
				else if not correct && index > 0
					then do
						let oldBucket = delete randCard bucket
						newBucket <- insert randCard $ buckets !! 0
						return $ ([newBucket] ++ tail (take index buckets) ++ [oldBucket] ++ drop (index + 1) buckets, origProbs)
					else
						return gs

playGame :: (Card a) => GameState a -> IO ()
playGame gs = tryCard gs >>= playGame

playGamePrompt :: (Card a) => GameState a -> IO ()
playGamePrompt gs = do
	gs' <- tryCard gs
	putStrImmediate "Coninue (y/n)? "
	cont <- getCharImmediate
	if cont == 'y'
		then playGamePrompt gs'
		else saveState gs'

buildGameState :: (Card a) => [a] -> [Probability] -> GameState a
buildGameState cs ps = (buildBucket cs : replicate (length ps - 1) Empty, ps)

-- Saving state

saveState :: (Card a) => GameState a -> IO ()
saveState gs = do
	putStrImmediate "Enter savefile, or blank to skip: "
	filename <- getLine
	if filename == ""
		then return ()
		else writeFile filename $ show gs

loadState :: (Read a, Card a) => FilePath -> IO (GameState a)
loadState fp = do
	fileContents <- readFile fp
	let partialRead = read fileContents :: (Read a) => ([[a]], [Probability])
	return (map buildBucket $ fst partialRead, snd partialRead)

promptProbabilities :: IO [Probability]
promptProbabilities = do
	putStrImmediate "Enter probabilities, seperated by spaces: "
	probStr <- getLine
	return $ map read $ words probStr

-- prompts user for savefile, or none
maybeLoadState :: (Card a) => IO (Maybe (GameState a))
maybeLoadState = do
	putStrImmediate "Enter savefile, or blank to skip: "
	savefile <- getLine
	if savefile /= ""
		then do
			state <- loadState savefile
			return $ Just state
		else return Nothing

-- update GameState with more cards for the first bucket
updateState :: (Card a) => GameState a -> [a] -> GameState a
updateState gs newCards = (buildBucket (flatten (head buckets) ++ newCards) : tail buckets, probs) where
	buckets = fst gs
	probs = snd gs

-- Test functions

data TestCard = TestCard Int
	deriving (Show, Read)

instance Card TestCard where
	getId (TestCard i) = i
	getAction (TestCard i) = do
		putStrImmediate $ "Card " ++ show i ++ ", (y/n): "
		response <- getCharImmediate
		return $ response == 'y'

testInsert :: IO (Bucket TestCard)
testInsert = foldM (flip insert) Empty $ map TestCard [1,8,7,4,2,10,5,6,3,9]

testDelete :: Int -> IO (Bucket TestCard)
testDelete n = testInsert >>= return . delete (TestCard n)

testGame :: Int -> GameState TestCard
testGame n = buildGameState (map TestCard [1..n]) [0.75, 0.2, 0.05]

verbosePlayGame :: (Card a) => GameState a -> IO ()
verbosePlayGame gs = putStrLn (show gs) >> tryCard gs >>= verbosePlayGame

-- Deletes have been causing a bug

data RandCard = RandCard Int
	deriving (Show, Read)

instance Card RandCard where
	getId (RandCard i) = i
	getAction _ = do
		randInt <- getStdRandom $ randomR (0, 1)	:: IO Int
		return $ if randInt == 0
			then True
			else False

testDeleteBug :: Int -> IO ()
testDeleteBug n = verbosePlayGame $ buildGameState (map RandCard [1..n]) [0.5, 0.5]
