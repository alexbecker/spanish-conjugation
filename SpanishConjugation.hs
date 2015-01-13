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
	deriving (Read, Show)

data Person = Yo
			| Tu
			| El
			| Nosotros
			| Vosotros
			| Ellos
	deriving (Read, Show)

type StemChange = Char -> Maybe String

allTenses = [Present, PresentProgressive, Preterite, Imperfect]

allPersons = [Yo, Tu, El, Nosotros, Vosotros, Ellos]

irregular :: Verb -> Tense -> Bool
irregular "ser" _ = True
irregular "ir" _ = True
irregular "ver" _ = True
irregular _ Imperfect = False
irregular _ _ = False

-- All possible stem changes
e_ie 'e' = Just "ie"
e_ie _ = Nothing

e_i 'e' = Just "i"
e_i _ = Nothing

i_ie 'i' = Just "ie"
i_ie _ = Nothing

o_ue 'o' = Just "ue"
o_ue _ = Nothing

o_hue 'o' = Just "hue"
o_hue _ = Nothing

u_ue 'u' = Just "ue"
u_ue _ = Nothing

getStemChange :: Verb -> Maybe StemChange
getStemChange "acertar" = Just e_ie
getStemChange "divertirse" = Just e_ie
getStemChange "pensar" = Just e_ie
getStemChange "atender" = Just e_ie
getStemChange "empezar" = Just e_ie
getStemChange "perder" = Just e_ie
getStemChange "atravesar" = Just e_ie
getStemChange "encender" = Just e_ie
getStemChange "preferir" = Just e_ie
getStemChange "calentar" = Just e_ie
getStemChange "encerrar" = Just e_ie
getStemChange _ = Nothing

isStemChange :: Verb -> Bool
isStemChange = isJust . getStemChange

showStemChange :: StemChange -> String
showStemChange = concat . catMaybes . (flip map "eiou")

applyStemChange :: Verb -> String
applyStemChange v = (reverse $ take vowelNegIndex rev ++ reverse (fromJust $ s vowel) ++ drop (vowelNegIndex + 1) rev) ++ ending
	where
		s = fromJust $ getStemChange v
		stem = take (length v - 2) v
		ending = drop (length v - 2) v
		rev = reverse stem
		vowel = fromJust $ find (\x -> isJust (s x)) "eiou"
		vowelNegIndex = fromJust $ elemIndex vowel rev

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

conjugate :: Verb -> Tense -> Person -> String
conjugate v t = if isStemChange v
				then regularConjugate (applyStemChange v) t
				else if irregular v t
					then irregularConjugate v t
					else regularConjugate v t

test :: Verb -> Tense -> Person -> IO Bool
test v t p = do
	putStr $ show p ++ ": "
	hFlush stdout
	attempt <- getLine
	let correct = conjugate v t p
	if attempt == correct
		then return True
		else
			putStrLn correct >> return False

testAllPersons :: Verb -> Tense -> IO Bool
testAllPersons v t = do
	putStrLn $ "Conjugate '" ++ v ++ "' in " ++ show t
	attempts <- sequence $ map (test v t) allPersons
	if and attempts
		then return True
		else return False

main :: IO ()
main = do
	putStrLn "Enter probabilities, seperated by spaces: "
	probStr <- getLine
	let probs = map read $ words probStr
	putStrLn "Enter verbs, seperated by spaces: "
	verbStr <- getLine
	let verbs = words verbStr
	putStrLn "Enter tense: "
	tenseStr <- getLine
	let tense = read tenseStr
	let cards = zip [1..length verbs] $ map (flip testAllPersons tense) verbs
	gs <- buildGameState cards probs
	playGame gs
