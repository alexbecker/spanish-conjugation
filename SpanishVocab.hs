module Main where

import Data.Maybe
import Data.Tuple
import System.IO
import SpacedRepetition
import SpanishParse

-- Ordering is ID, prompt, answer
data VocabCard = VocabCard Int String String
	deriving (Show, Read)

instance Card VocabCard where
	getId (VocabCard i _ _) = i
	getAction (VocabCard _ prompt ans) = do
		putStr prompt
		hFlush stdout
		getLine
		putStrLn ans
		putStr "Correct (y/n)? "
		hFlush stdout
		answer <- getChar
		getLine	-- chew newline
		if answer == 'y'
			then return True
			else return False

-- Load vocabulary from a file
-- Each line of file consists of:
-- spanish; english
loadVocab :: FilePath -> IO [VocabCard]
loadVocab fp = do
	fileContents <- readFile fp
	let spEnPairs = map ((\(x,y) -> (x, drop 2 y)) . (break (== ';')) . parseSpanish) $ lines fileContents
	let allPairs = spEnPairs ++ map swap spEnPairs
	return $ map (\(n,p) -> VocabCard n (fst p) (snd p)) $ zip [1..length allPairs] allPairs

main :: IO ()
main = do
	maybegs <- maybeLoadState
	gs <- if isJust maybegs
		then return $ fromJust maybegs
		else do
			probs <- promptProbabilities
			putStrLn "Enter vocab file: "
			vocabfile <- getLine
			cards <- loadVocab vocabfile
			return $ buildGameState cards probs
	playGamePrompt gs
