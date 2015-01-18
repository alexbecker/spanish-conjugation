module Main where

import Data.Tuple
import Data.Maybe
import Data.List
import Control.Monad
import SpacedRepetition
import SpanishParse
import ImmediateIO
import SpanishConjugationData
import SpanishConjugationTypes

data VerbCard = VerbCard Int Verb Tense
	deriving (Show, Read, Eq)

instance Card VerbCard where
	getId (VerbCard i _ _) = i
	getAction (VerbCard i v t) = do
		putStrLn $ "Conjugate '" ++ v ++ "' in " ++ show t
		attempts <- sequence $ map (testSinglePerson (VerbCard i v t)) knownPersons
		if and attempts
			then return True
			else return False

---- User Interaction ----

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
	gs <- if isJust maybegs
		then return $ fromJust maybegs
		else do
			probs <- promptProbabilities
			putStrImmediate "Enter verbs, or leave blank for all known: "
			verbStr <- getLine
			verbs <- if verbStr == ""
				then putStrLn "Known Verbs: " >> sequence (map putStrLn knownVerbs) >> return knownVerbs
				else return $ words $ parseSpanish verbStr
			putStrImmediate "Enter tense: "
			tenseStr <- getLine
			let tense = read tenseStr
			return $ buildGameState [VerbCard i v tense | (i, v) <- zip [1..length verbs] verbs] probs
	playGamePrompt gs

---- Final conjugation ----

getStem :: Verb -> Tense -> Person -> String
getStem v t p = if isStemChange v
	then regularStem $ applyStemChange v t p
	else regularStem v

getEnding :: Verb -> Tense -> Person -> String
getEnding v t p = regularEnding v t p

conjugate :: Verb -> Tense -> Person -> Verb
conjugate v t p = parseSpanish $ if irregular v t
	then irregularConjugate v t p
	else if t == Present && p == Yo && isGoVerb v
		then goVerbStem v ++ getEnding v t p
		else if t == Preterite && isPreteriteSemiregular v
			then preteriteSemiregularStem v ++ preteriteSemiregularEnding p
			else lastLetterRule v (getStem v t p) $ getEnding v t p

---- Universal rules ----

-- Regular verb handling

regularStem :: Verb -> Verb
regularStem = init . init

regularEnding :: Verb -> Tense -> Person -> String
regularEnding v t = case t of 
	Present -> case ending of
		"ar" -> unpack ["o", "as", "a", "amos", "áis", "an"]
		"er" -> unpack ["o", "es", "e", "emos", "éis", "en"]
		"ir" -> unpack ["o", "es", "e", "imos", "ís", "en"]
	Preterite -> if ending == "ar"
		then unpack ["é", "aste", "ó", "amos", "asteis", "aron"]
		else unpack ["í", "iste", "ió", "imos", "isteis", "ieron"]
	Imperfect -> if ending == "ar"
		then unpack ["aba", "abas", "aba", "ábamos", "abais", "aban"]
		else unpack ["ía", "ías", "ía", "íamos", "íais", "ían"]
	where
		ending = drop (length v - 2) v

-- Go verbs

isGoVerb :: Verb -> Bool
isGoVerb v = v !! (length v - 3) == 'n' && v !! (length v - 2) /= 'a'

goVerbStem :: Verb -> Verb
goVerbStem = (++ "g") . regularStem

-- Stem changers

vowelChanged :: StemChange -> Char
vowelChanged s = case s of
	EtoIE -> 'e'
	OtoUE -> 'o'
	UtoUE -> 'u'
	EtoI -> 'e'

buildChanger :: Char -> String -> Char -> String
buildChanger a b c = if a == c
	then b
	else [c]

isStemChange :: Verb -> Bool
isStemChange = isJust . getStemChange

-- Gives the actual transformation Char -> String for the specific tense, person and ending
-- Ending argument is ending of verb
stemChangeAsApplied :: Tense -> Person -> String -> StemChange -> Char -> String
stemChangeAsApplied t p ending stemchange = case t of
	Present -> if p /= Nosotros && p /= Vosotros
		then case stemchange of
			EtoIE -> buildChanger 'e' "ie"
			OtoUE -> buildChanger 'o' "ue"
			UtoUE -> buildChanger 'u' "ue"
			EtoI -> buildChanger 'e' "i"
		else return
	Preterite -> if ending == "ir" && elem p [El, Ellos]
		then case stemchange of
			EtoIE -> buildChanger 'e' "i"
			OtoUE -> buildChanger 'o' "u"
			UtoUE -> buildChanger 'u' "u"
			EtoI -> buildChanger 'e' "i"
		else return
	Imperfect -> return

-- given multiple choices, final vowel is changed
applyStemChange :: Verb -> Tense -> Person -> Verb
applyStemChange v t p = (reverse $ take vowelNegIndex rev ++ reverse (actualchange vowel) ++ drop (vowelNegIndex + 1) rev) ++ ending
	where
		stemchange = fromJust $ getStemChange v
		stem = take (length v - 2) v
		ending = drop (length v - 2) v
		actualchange = stemChangeAsApplied t p ending stemchange
		rev = reverse stem
		vowel = vowelChanged stemchange
		vowelNegIndex = fromJust $ elemIndex vowel rev

-- Preserving sounds when vowels change hardness

hardVowels = "aouáó"
softVowels = "eiéí"
vowels = hardVowels ++ softVowels

hardToSoft :: Char -> String
hardToSoft 'c' = "qu"
hardToSoft 'g' = "gu"
hardToSoft 'z' = "c"
hardToSoft x = return x

softToHard :: Char -> Char -> String
softToHard prev 'c' = if elem prev vowels
	then "zc"
	else "z"
softToHard _ 'g' = "j"
softToHard _ x = return x

lastLetterRule :: Verb -> String -> String -> Verb
lastLetterRule orig stem ending = init stem ++ replacement ++ ending where
	origVowel = orig !! (length orig - 2)
	newVowel = head ending
	replacement = if elem origVowel hardVowels && elem newVowel softVowels
		then hardToSoft $ last stem
		else if elem origVowel softVowels && elem newVowel hardVowels
			then softToHard (last $ init stem) $ last stem
			else return $ last stem

-- Preterite semiregulars

isPreteriteSemiregular :: Verb -> Bool
isPreteriteSemiregular = isJust . getPreteriteSemiregularForm

preteriteSemiregularStem :: Verb -> String
preteriteSemiregularStem = fromJust . getPreteriteSemiregularForm

preteriteSemiregularEnding :: Person -> String
preteriteSemiregularEnding = unpack ["e", "iste", "o", "imos", undefined, "ieron"]
