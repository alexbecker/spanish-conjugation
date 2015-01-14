module Main where

import System.Random
import System.IO
import System.Exit
import SpacedRepetition
import SpanishParse

-- Ordering is ID, spanish, english
data VocabCard = VocabCard Int String String
	deriving (Show, Read)

instance Card VocabCard where
	getId (VocabCard i _ _) = i
	getAction (VocabCard _ sp en) = do
		-- randomly select spanish->english or english->spanish
		randInt <- getStdRandom $ randomR (0, 1) :: IO Int
		if randInt == 0
			then do
				putStr sp
				hFlush stdout
				getLine
				putStrLn en
			else do
				putStr en
				hFlush stdout
				getLine
				putStrLn sp
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
	let pairs = map ((break (== ';')) . parseSpanish) $ lines fileContents
	return $ map (\(n,p) -> VocabCard n (fst p) $ drop 2 $ snd p) $ zip [1..length pairs] pairs

main :: IO ()
main = do
	putStrLn "Enter savefile, or blank to skip: "
	savefile <- getLine
	gs <- if savefile /= ""
		then loadState savefile
		else do
			putStrLn "Enter vocab file: "
			vocabfile <- getLine
			cards <- loadVocab vocabfile
			putStrLn "Enter probabilities, seperated by spaces: "
			probStr <- getLine
			let probs = map read $ words probStr
			return $ buildGameState cards probs
	playGamePrompt gs
