module Main where

import Data.Maybe
import Data.List
import SpacedRepetition
import SpanishParse
import ImmediateIO
import SpanishConjugation
import SpanishConjugationTypes
import SpanishConjugationData

data VerbCard = VerbCard Int Verb Tense
	deriving (Show, Read)

instance Card VerbCard where
	getId (VerbCard i _ _) = i
	getAction (VerbCard i v t) = do
		putStrLn $ "Conjugate '" ++ v ++ "' in " ++ show t
		attempts <- sequence $ map (testSinglePerson (VerbCard i v t)) knownPersons
		if and attempts
			then return True
			else return False

getVerb :: VerbCard -> Verb
getVerb (VerbCard _ v _) = v

getTense :: VerbCard -> Tense
getTense (VerbCard _ _ t) = t

makeVerbCards :: Int -> [Verb] -> Tense -> [VerbCard]
makeVerbCards minId verbs tense = [VerbCard i v tense | (i, v) <- zip [minId .. minId + length verbs - 1] verbs]

testSinglePerson :: VerbCard -> Person -> IO Bool
testSinglePerson (VerbCard _ v t) p = do
	putStrImmediate $ show p ++ ": "
	attempt <- getLine
	let correct = conjugate v t p
	if parseSpanish attempt == correct
		then return True
		else putStrLn correct >> return False

main :: IO ()
main = do
	maybegs <- maybeLoadState
	putStrLn "Known Verbs: " >> sequence (map putStrLn knownVerbs)
	gs <- if isJust maybegs
		then do 
			putStrImmediate "Update with all known verbs? (y/n): "
			update <- getCharImmediate
			if update == 'y'
				then do
					let cards = concat $ map flatten $ fst $ fromJust maybegs
					let tense = getTense $ head cards
					let oldMaxId = maximum $ map getId cards
					let newVerbs = knownVerbs \\ map getVerb cards
					return $ updateState (fromJust maybegs) $ makeVerbCards (oldMaxId + 1) newVerbs tense
				else return $ fromJust maybegs
		else do
			probs <- promptProbabilities
			putStrImmediate "Enter verbs, or leave blank for all known: "
			verbStr <- getLine
			verbs <- if verbStr == ""
				then return knownVerbs
				else return $ words $ parseSpanish verbStr
			putStrImmediate "Enter tense: "
			tenseStr <- getLine
			let tense = read tenseStr
			return $ buildGameState (makeVerbCards 1 verbs tense) probs
	playGamePrompt gs
