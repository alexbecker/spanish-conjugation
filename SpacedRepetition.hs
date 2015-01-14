-- Implements bucket-based spaced repetition
module SpacedRepetition where

import Data.Tuple
import Data.Maybe
import Data.List (find, findIndex, (!!))
import System.Random
import System.IO
import Control.Monad

class (Read a, Show a) => Card a where
	getId :: a -> Int
	getAction :: a -> IO Bool
	
-- Just an ordered tree
data Bucket a = Empty
			  | Leaf a
			  | Node Int Int Int (Bucket a) (Bucket a)	-- (numChildren, leftMax, rightMin, left, right)
	deriving (Show, Read)

type Probability = Double
type GameState a = ([Bucket a], [Probability])

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
secondMaxId (Node _ _ _ a (Leaf b)) = getId $ bmax a
secondMaxId (Node _ _ _ _ b) = secondMaxId b

bmin :: Bucket a -> a
bmin (Leaf a) = a
bmin (Node _ _ _ a _) = bmin a

secondMinId :: (Card a) => Bucket a -> Int
secondMinId (Node _ _ _ (Leaf a) b) = getId $ bmax b
secondMinId (Node _ _ _ a _) = secondMinId a

idList :: (Card a) => Bucket a -> [Int]
idList Empty = []
idList (Leaf a) = [getId a]
idList (Node _ _ _ a b) = idList a ++ idList b

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
buildBucket cs = Node (numChildren left + numChildren right) (getId $ bmax left) (getId $ bmin right) left right
	where
		splitcs = splitAt (quot (length cs) 2) cs
		left = buildBucket $ fst splitcs
		right = buildBucket $ snd splitcs

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
playGame gs = do
	gs' <- tryCard gs
	playGame gs'

playGamePrompt :: (Card a) => GameState a -> IO ()
playGamePrompt gs = do
	gs' <- tryCard gs
	putStr "Coninue (y/n)? "
	hFlush stdout
	cont <- getChar
	getLine	-- chew newline
	if cont == 'y'
		then playGamePrompt gs'
		else saveState gs'

buildGameState :: (Card a) => [a] -> [Probability] -> GameState a
buildGameState cs ps = (buildBucket cs : replicate (length ps - 1) Empty, ps)

-- Saving state

saveState :: (Card a) => GameState a -> IO ()
saveState gs = do
	putStrLn "Enter savefile, or blank to skip: "
	filename <- getLine
	if filename == ""
		then return ()
		else writeFile filename $ show gs

loadState :: (Card a) => FilePath -> IO (GameState a)
loadState fp = do
	fileContents <- readFile fp
	return $ read fileContents

promptProbabilities :: IO [Probability]
promptProbabilities = do
	putStrLn "Enter probabilities, seperated by spaces: "
	probStr <- getLine
	return $ map read $ words probStr

-- prompts user for savefile, or none
maybeLoadState :: (Card a) => IO (Maybe (GameState a))
maybeLoadState = do
	putStrLn "Enter savefile, or blank to skip: "
	savefile <- getLine
	if savefile /= ""
		then do
			state <- loadState savefile
			return $ Just state
		else return Nothing

-- Test functions

data TestCard = TestCard Int
	deriving (Show, Read)

instance Card TestCard where
	getId (TestCard i) = i
	getAction (TestCard i) = do
		putStr $ "Card " ++ show i ++ ", Enter 'y': "
		response <- getChar
		putStrLn ""
		return $ response == 'y'

testInsert :: IO (Bucket TestCard)
testInsert = do
	foldM (flip insert) Empty $ map TestCard [1,8,7,4,2,10,5,6,3,9]

testDelete :: Int -> IO (Bucket TestCard)
testDelete n = do 
	x <- testInsert
	return $ delete (TestCard n) x

testGame :: Int -> GameState TestCard
testGame n = buildGameState (map TestCard [1..n]) [0.75, 0.2, 0.05]
