module Main where

import Data.Maybe
import Data.Tuple
import Data.List
import SpacedRepetition
import SpanishParse
import ImmediateIO

-- Ordering is ID, prompt, answer
data VocabCard = VocabCard Int String String
	deriving (Show, Read, Eq)

instance Card VocabCard where
	getId (VocabCard i _ _) = i
	getAction (VocabCard _ prompt ans) = do
		putStrImmediate prompt
		getLine
		putStrLn ans
		putStrImmediate "Correct (y/n)? "
		answer <- getCharImmediate
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
	let allPairs = concat $ map (\x -> [x, swap x]) $ spEnPairs
	return $ map (\(n,p) -> VocabCard n (fst p) (snd p)) $ zip [1..length allPairs] allPairs

main :: IO ()
main = do
	maybegs <- maybeLoadState
	gs <- if isJust maybegs
		then do
			putStrImmediate "Enter vocab file, or leave blank if unchanged: "
			vocabfile <- getLine
			if vocabfile == ""
				then return $ fromJust maybegs
				else do
					newVocab <- loadVocab vocabfile 
					let oldVocab = concat $ map flatten $ fst $ fromJust maybegs
					return $ updateState (fromJust maybegs) $ newVocab \\ oldVocab
		else do
			probs <- promptProbabilities
			putStrImmediate "Enter vocab file: "
			vocabfile <- getLine
			cards <- loadVocab vocabfile
			return $ buildGameState cards probs
	playGamePrompt gs
