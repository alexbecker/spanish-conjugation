module Main where

import Data.Tuple
import Data.Maybe
import Data.List
import Control.Monad
import System.IO
import SpacedRepetition
import SpanishParse

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
				| EtoI

data VerbCard = VerbCard Int Verb Tense
	deriving (Show, Read, Eq)

---- Extractors for VerbCard (other than getId) ----

getVerb :: VerbCard -> Verb
getVerb (VerbCard _ v _) = v

getTense :: VerbCard -> Tense
getTense (VerbCard _ _ t) = t

---- Interaction functions ----

testSinglePerson :: VerbCard -> Person -> IO Bool
testSinglePerson (VerbCard _ v t) p = do
	putStr $ show p ++ ": "
	hFlush stdout
	attempt <- getLine
	let correct = conjugate v t p
	if parseSpanish attempt == correct
		then return True
		else
			putStrLn correct >> return False

-- Ignore Vosotros for now
allPersons = [Yo, Tu, El, Nosotros, Ellos]

instance Card VerbCard where
	getId (VerbCard i _ _) = i
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
	EtoI -> 'e'

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

-- Conjugation data unpacking

-- Conjugation data is stored as lists by person, so unpack to functions
unpack :: [String] -> Person -> String
unpack cs p = cs !! case p of
	Yo -> 0
	Tu -> 1
	El -> 2
	Nosotros -> 3
	Vosotros -> 4
	Ellos -> 5

-- Regular verb handling

regularConjugate :: Verb -> Tense -> Person -> String
regularConjugate v t = (stem ++) . case t of 
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
		stem = take (length v - 2) v
		ending = drop (length v - 2) v

---- Final conjugation ----

conjugate :: Verb -> Tense -> Person -> String
conjugate v t p = parseSpanish $ if irregular v t
	then irregularConjugate v t p
	else if isStemChange v
		then regularConjugate (applyStemChange v t p) t p
		else regularConjugate v t p

---- Main ----

main :: IO ()
main = do
	maybegs <- maybeLoadState
	gs <- if isJust maybegs
		then return $ fromJust maybegs
		else do
			probs <- promptProbabilities
			putStrLn "Enter verbs, seperated by spaces: "
			verbStr <- getLine
			let verbs = words $ parseSpanish verbStr
			putStrLn "Enter tense: "
			tenseStr <- getLine
			let tense = read tenseStr
			return $ buildGameState [VerbCard i v tense | (i, v) <- zip [1..length verbs] verbs] probs
	playGamePrompt gs

---- Conjugation data ----

getStemChange :: Verb -> Maybe StemChange
-- EtoIE
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
getStemChange "querer" = Just EtoIE
-- OtoUE
getStemChange "dormir" = Just OtoUE
-- UtoUe
-- EtoI
getStemChange "pedir" = Just EtoI
getStemChange _ = Nothing

irregular :: Verb -> Tense -> Bool
irregular "ser" _ = True
irregular "ir" _ = True
irregular "ver" _ = True
irregular _ Imperfect = False
irregular "estar" _ = True
irregular "decir" _ = True
irregular "hacer" _ = True
irregular "poner" _ = True
irregular "saber" _ = True
irregular "tener" _ = True
irregular "poder" Preterite = True
irregular "querer" Preterite = True
irregular "empezar" Preterite = True
irregular _ _ = False

-- Template:
-- irregularConjugate "" t = case t of
-- _ -> unpack ["", "", "", "", "", ""]
irregularConjugate :: Verb -> Tense -> Person -> String
irregularConjugate "ser" t = case t of
	Present -> unpack ["soy", "eres", "es", "somos", "sois", "son"]
	Preterite -> unpack ["fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron"]
	Imperfect -> unpack ["era", "eras", "era", "éramos", "erais", "eran"]
irregularConjugate "ir" t = case t of
	Present -> unpack ["voy", "vas", "va", "vamos", undefined, "van"]
	Preterite -> irregularConjugate "ser" Preterite
	Imperfect -> unpack ["iba", "ibas", "iba", "ibamos", undefined, "iban"]
irregularConjugate "ver" t = case t of
	Present -> unpack ["veo", "ves", "ve", "vemos", undefined, "ven"]
	Preterite -> unpack ["vi", "viste", "vio", "vimos", undefined, "vieron"]
	Imperfect -> regularConjugate "veer" Imperfect
irregularConjugate "estar" t = case t of
	Present -> unpack ["estoy", "esta's", "esta'", "estamos", undefined, "esta'n"]
	Preterite -> unpack ["estuve", "estuviste", "estuvo", "estuvimos", undefined, "estuvieron"]
irregularConjugate "decir" t = case t of
	Present -> unpack ["digo", "dices", "dice", "decimos", undefined, "dicen"]
	Preterite -> unpack ["dije", "dijiste", "dijo", "dijimos", undefined, "dijieron"]
irregularConjugate "hacer" t = case t of
	Present -> unpack ["hago", "haces", "hace", "hacemos", undefined, "hacen"]
	Preterite -> unpack ["hice", "hiciste", "hizo", "hicimos", undefined, "hicieron"]
irregularConjugate "poner" t = case t of
	Present -> unpack ["pongo", "pones", "pone", "ponemos", undefined, "ponen"]
	Preterite -> unpack ["puse", "pusiste", "puso", "pusimos", undefined, "pusieron"]
irregularConjugate "saber" t = case t of
	Present -> unpack ["se'", "sabes", "sabe", "sabemos", undefined, "saben"]
	Preterite -> unpack ["supe", "supiste", "supo", "supimos", undefined, "supieron"]
irregularConjugate "tener" t = case t of
	Present -> unpack ["tengo", "tienes", "tiene", "tenemos", undefined, "tienen"]
	Preterite -> unpack ["tuve", "tuviste", "tuvo", "tuvimos", undefined, "tuvieron"]
irregularConjugate "poder" t = case t of
	Preterite -> unpack ["pude", "pudiste", "pudo", "pudimos", undefined, "pudieron"]
irregularConjugate "querer" t = case t of
	Preterite -> unpack ["quise", "quisiste", "quiso", "quisimos", undefined, "quisieron"]
irregularConjugate "empezar" t = case t of
	Preterite -> unpack ["empece'", "empezaste", "empezo'", "empezamos", undefined, "empezaron"]
