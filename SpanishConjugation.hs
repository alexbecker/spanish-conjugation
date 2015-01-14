module Main where

import Data.Tuple
import Data.Maybe
import Data.List
import Control.Monad
import System.IO
import SpacedRepetition

type Verb = String

data Tense = Present
		   | PresentProgressive
		   | Preterite
		   | Imperfect
		   | Subjunctive
		   | Future
		   | Conditional
		   | Command
	deriving (Read, Show, Eq)

data Person = Yo
			| Tu
			| El
			| Nosotros
			| Vosotros
			| Ellos
	deriving (Read, Show, Eq)

data StemChange = EtoIE
				| OtoUE
				| UtoUE

data SpanishCard = SpanishCard Int Verb Tense
	deriving (Show, Read, Eq)

-- Extractors for SpanishCard (other than getId)

getVerb :: SpanishCard -> Verb
getVerb (SpanishCard _ v _) = v

getTense :: SpanishCard -> Tense
getTense (SpanishCard _ _ t) = t

-- Ascii -> Spanish alphabet

parseAccent :: String -> Char
parseAccent s = case s of
	"a'" -> 'á'
	"e'" -> 'é'
	"i'" -> 'í'
	"o'" -> 'ó'
	"n~" -> 'ñ'

readVerb :: String -> Verb
readVerb s = if isJust ind
	then (readVerb $ take (fromJust ind - 1) s) ++ parseAccent (take 2 $ drop (fromJust ind - 1) s) : (readVerb $ drop (fromJust ind + 1) s)
	else s
	where
		ind = elemIndex '\'' s

-- Interaction functions

testSinglePerson :: SpanishCard -> Person -> IO Bool
testSinglePerson (SpanishCard _ v t) p = do
	putStr $ show p ++ ": "
	hFlush stdout
	attempt <- getLine
	let correct = conjugate v t p
	if readVerb attempt == correct
		then return True
		else
			putStrLn correct >> return False

-- Ignore Vosotros for now
allPersons = [Yo, Tu, El, Nosotros, Ellos]

instance Card SpanishCard where
	getId (SpanishCard i _ _) = i
	getAction card = do
		putStrLn $ "Conjugate '" ++ getVerb card ++ "' in " ++ show (getTense card)
		attempts <- sequence $ map (testSinglePerson card) allPersons
		if and attempts
			then return True
			else return False

---- Conjugation handling functions ----

-- Stem change handling

vowelChanged :: StemChange -> Char
vowelChanged s = case s of
	EtoIE -> 'e'
	OtoUE -> 'o'
	UtoUE -> 'u'

-- Helper function
buildChanger :: Char -> String -> Char -> String
buildChanger a b c = if a == c
	then b
	else [c]

-- Gives the actual transformation Char -> String for the specific tense, person and ending
-- Ending argument is ending of verb
stemChangeAsApplied :: Tense -> Person -> String -> StemChange -> Char -> String
stemChangeAsApplied t p ending stemchange = case t of
	Present -> if p /= Nosotros && p /= Vosotros
		then case stemchange of
			EtoIE -> buildChanger 'e' "ie"
			OtoUE -> buildChanger 'o' "ue"
			UtoUE -> buildChanger 'u' "ue"
		else return
	Preterite -> if ending == "ir" && elem p [Yo, El, Ellos]
		then case stemchange of
			EtoIE -> buildChanger 'e' "i"
			OtoUE -> buildChanger 'o' "u"
			UtoUE -> buildChanger 'u' "u"
		else return
	Imperfect -> return

isStemChange :: Verb -> Bool
isStemChange = isJust . getStemChange

-- given multiple choices, final vowel is changed
applyStemChange :: Verb -> Tense -> Person -> String
applyStemChange v t p = (reverse $ take vowelNegIndex rev ++ reverse (actualchange vowel) ++ drop (vowelNegIndex + 1) rev) ++ ending
	where
		stemchange = fromJust $ getStemChange v
		stem = take (length v - 2) v
		ending = drop (length v - 2) v
		actualchange = stemChangeAsApplied t p ending stemchange
		rev = reverse stem
		vowel = vowelChanged stemchange
		vowelNegIndex = fromJust $ elemIndex vowel rev

-- Regular verb handling

regularConjugate :: Verb -> Tense -> Person -> String
regularConjugate v t p = stem ++ case t of 
	Present -> case ending of
		"ar" -> case p of
			Yo -> "o"
			Tu -> "as"
			El -> "a"
			Nosotros -> "amos"
			Vosotros -> "áis"
			Ellos -> "an"
		"er" -> case p of
			Yo -> "o"
			Tu -> "es"
			El -> "e"
			Nosotros -> "emos"
			Vosotros -> "éis"
			Ellos -> "en"
		"ir" -> case p of
			Yo -> "o"
			Tu -> "es"
			El -> "e"
			Nosotros -> "imos"
			Vosotros -> "ís"
			Ellos -> "en"
	Preterite -> if ending == "ar"
		then case p of
			Yo -> "é"
			Tu -> "aste"
			El -> "ó"
			Nosotros -> "amos"
			Vosotros -> "asteis"
			Ellos -> "aron"
		else case p of
			Yo -> "í"
			Tu -> "iste"
			El -> "ió"
			Nosotros -> "imos"
			Vosotros -> "isteis"
			Ellos -> "ieron"
	Imperfect -> if ending == "ar"
		then case p of
			Yo -> "aba"
			Tu -> "abas"
			El -> "aba"
			Nosotros -> "ábamos"
			Vosotros -> "abais"
			Ellos -> "aban"
		else case p of
			Yo -> "ía"
			Tu -> "ías"
			El -> "ía"
			Nosotros -> "íamos"
			Vosotros -> "íais"
			Ellos -> "ían"
	where
		stem = take (length v - 2) v
		ending = drop (length v - 2) v

---- Final conjugation ----

conjugate :: Verb -> Tense -> Person -> String
conjugate v t p = if irregular v t
	then irregularConjugate v t p
	else if isStemChange v
		then regularConjugate (applyStemChange v t p) t p
		else regularConjugate v t p

main :: IO ()
main = do
	putStrLn "Enter savefile, or blank to skip: "
	filename <- getLine
	gs <- if filename /= ""
		then do
			fileContents <- readFile filename
			return $ read fileContents
		else do
			putStrLn "Enter probabilities, seperated by spaces: "
			probStr <- getLine
			let probs = map read $ words probStr
			putStrLn "Enter verbs, seperated by spaces: "
			verbStr <- getLine
			let verbs = words $ readVerb verbStr
			putStrLn "Enter tense: "
			tenseStr <- getLine
			let tense = read tenseStr
			let cards = [SpanishCard i v tense | (i, v) <- zip [1..length verbs] verbs]
			buildGameState cards probs
	gs' <- playGamePrompt gs
	putStrLn "Enter savefile, or blank to skip: "
	saveFile <- getLine
	if saveFile /= ""
		then writeFile saveFile $ show gs'
		else return ()

---- Conjugation data ----

getStemChange :: Verb -> Maybe StemChange
getStemChange "acertar" = Just EtoIE
getStemChange "divertirse" = Just EtoIE
getStemChange "pensar" = Just EtoIE
getStemChange "atender" = Just EtoIE
getStemChange "empezar" = Just EtoIE
getStemChange "perder" = Just EtoIE
getStemChange "atravesar" = Just EtoIE
getStemChange "encender" = Just EtoIE
getStemChange "preferir" = Just EtoIE
getStemChange "calentar" = Just EtoIE
getStemChange "encerrar" = Just EtoIE
getStemChange _ = Nothing

irregular :: Verb -> Tense -> Bool
irregular "ser" _ = True
irregular "ir" _ = True
irregular "ver" _ = True
irregular _ Imperfect = False
irregular _ _ = False

irregularConjugate :: Verb -> Tense -> Person -> String
irregularConjugate "ser" t p = case t of
	Present -> case p of
		Yo -> "soy"
		Tu -> "eres"
		El -> "es"
		Nosotros -> "somos"
		Vosotros -> "sois"
		Ellos -> "son"
	Preterite -> case p of
		Yo -> "fui"
		Tu -> "fuiste"
		El -> "fue"
		Nosotros -> "fuimos"
		Vosotros -> "fuisteis"
		Ellos -> "fueron"
	Imperfect -> case p of
		Yo -> "era"
		Tu -> "eras"
		El -> "era"
		Nosotros -> "éramos"
		Vosotros -> "erais"
		Ellos -> "eran"
irregularConjugate "ir" t p = case t of
	Present -> case p of
		Yo -> "voy"
		Tu -> "vas"
		El -> "va"
		Nosotros -> "vamos"
		Vosotros -> undefined
		Ellos -> "van"
	Preterite -> irregularConjugate "ser" Preterite p
	Imperfect -> case p of
		Yo -> "iba"
		Tu -> "ibas"
		El -> "iba"
		Nosotros -> "ibamos"
		Vosotros -> undefined
		Ellos -> "iban"

