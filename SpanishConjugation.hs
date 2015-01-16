module Main where

import Data.Tuple
import Data.Maybe
import Data.List
import Control.Monad
import SpacedRepetition
import SpanishParse
import ImmediateIO

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

---- Interaction functions ----

testSinglePerson :: VerbCard -> Person -> IO Bool
testSinglePerson (VerbCard _ v t) p = do
	putStrImmediate $ show p ++ ": "
	attempt <- getLine
	let correct = conjugate v t p
	if parseSpanish attempt == correct
		then return True
		else putStrLn correct >> return False

instance Card VerbCard where
	getId (VerbCard i _ _) = i
	getAction (VerbCard i v t) = do
		putStrLn $ "Conjugate '" ++ v ++ "' in " ++ show t
		attempts <- sequence $ map (testSinglePerson (VerbCard i v t)) knownPersons
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

isStemChange :: Verb -> Bool
isStemChange = isJust . getStemChange

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

-- Final conjugation

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

---- Main ----

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

---- Conjugation data ----

knownPersons = [Yo, Tu, El, Nosotros, Ellos]
knownVerbs = sort ["ser", "estar", "tener", "hacer", "poder", "decir", "ir", "dar", "saber", "querer", "llegar", "pasar", "deber", "poner", "parecer", "quedar", "creer", "hablar", "llevar", "dejar", "seguir", "encontrar", "llamar", "venir", "pensar", "salir", "volver", "tomar", "conocer", "vivir", "sentir", "tratar", "mirar", "contar", "empezar", "esperar", "buscar", "existir", "entrar", "trabajar", "escribir", "perder", "producir", "ocurrir", "entender", "pedir", "recibir", "recordar", "terminar", "permitir", "aperecer", "conseguir", "comenzar", "servir", "sacar", "necesitar", "mantener", "resultar", "leer", "caer", "cambiar", "presentar", "crear", "abrir", "considerar", "oír", "acabar", "convertir", "ganar", "formar", "traer", "partir", "morir", "aceptar", "realizar", "suponer", "comprender", "lograr", "explicar"]

-- (UNIFORM) Regular verb handling

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

-- (UNIFORM) -go verbs

isGoVerb :: Verb -> Bool
isGoVerb v = v !! (length v - 3) == 'n' && v !! (length v - 2) /= 'a'

goVerbStem :: Verb -> Verb
goVerbStem = (++ "g") . regularStem

-- (UNIFORM) Preserving sounds when vowels change hardness

hardVowels = "aouáó"
softVowels = "eiéí"
vowels = hardVowels ++ softVowels

hardToSoft :: Char -> String
hardToSoft 'c' = "qu"
hardToSoft 'g' = "gu"
hardToSoft 'z' = "c"
hardToSoft x = return x

-- Args: previous char, char
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

-- (UNIFORM) Stem change handling

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

-- (INDIVIDUAL) Preterite common patterns

getPreteriteSemiregularForm :: Verb -> Maybe Verb
getPreteriteSemiregularForm "estar" = Just "estuv"
getPreteriteSemiregularForm "decir" = Just "dij"
getPreteriteSemiregularForm "hacer" = Just "hic"
getPreteriteSemiregularForm "poner" = Just "pus"
getPreteriteSemiregularForm "suponer" = Just "supus"
getPreteriteSemiregularForm "saber" = Just "sup"
getPreteriteSemiregularForm "tener" = Just "tuv"
getPreteriteSemiregularForm "venir" = Just "vin"
getPreteriteSemiregularForm "poder" = Just "pud"
getPreteriteSemiregularForm "querer" = Just "quis"
getPreteriteSemiregularForm "producir" = Just "produj"
getPreteriteSemiregularForm "mantener" = Just "mantuv"
getPreteriteSemiregularForm "traer" = Just "traj"
getPreteriteSemiregularForm _ = Nothing

isPreteriteSemiregular :: Verb -> Bool
isPreteriteSemiregular = isJust . getPreteriteSemiregularForm

preteriteSemiregularStem :: Verb -> String
preteriteSemiregularStem = fromJust . getPreteriteSemiregularForm

preteriteSemiregularEnding :: Person -> String
preteriteSemiregularEnding = unpack ["e", "iste", "o", "imos", undefined, "ieron"]

-- (INDIVIDUAL) Stem change handling

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
getStemChange "sentir" = Just EtoIE
getStemChange "tener" = Just EtoIE
getStemChange "venir" = Just EtoIE
getStemChange "mantener" = Just EtoIE
getStemChange "entender" = Just EtoIE
getStemChange "comenzar" = Just EtoIE
getStemChange "convertir" = Just EtoIE
-- OtoUE
getStemChange "dormir" = Just OtoUE
getStemChange "doler" = Just OtoUE
getStemChange "poder" = Just OtoUE
getStemChange "encontrar" = Just OtoUE
getStemChange "volver" = Just OtoUE
getStemChange "contar" = Just OtoUE
getStemChange "recordar" = Just OtoUE
getStemChange "morir" = Just OtoUE
-- UtoUE
-- EtoI
getStemChange "pedir" = Just EtoI
getStemChange "seguir" = Just EtoI
getStemChange "conseguir" = Just EtoI
getStemChange "servir" = Just EtoI
-- Not a stem changer
getStemChange _ = Nothing

-- Irregular verb handling

irregular :: Verb -> Tense -> Bool
irregular "ser" _ = True
irregular "ir" _ = True
irregular "ver" _ = True
irregular _ Imperfect = False
irregular "dar" _ = True
irregular "hacer" _ = True
irregular "caer" _ = True
irregular "oír" _ = True
irregular "estar" Present = True
irregular "decir" Present = True
irregular "saber" Present = True
irregular "salir" Present = True
irregular "seguir" Present = True
irregular "conseguir" Present = True
irregular "traer" Present = True
irregular "creer" Preterite = True
irregular "leer" Preterite = True
irregular _ _ = False

irregularConjugate :: Verb -> Tense -> Person -> String
-- all tenses
irregularConjugate "ser" t = case t of
	Present -> unpack ["soy", "eres", "es", "somos", "sois", "son"]
	Preterite -> unpack ["fui", "fuiste", "fue", "fuimos", "fuisteis", "fueron"]
	Imperfect -> unpack ["era", "eras", "era", "éramos", "erais", "eran"]
irregularConjugate "ir" t = case t of
	Present -> unpack ["voy", "vas", "va", "vamos", undefined, "van"]
	Preterite -> irregularConjugate "ser" Preterite
	Imperfect -> unpack ["iba", "ibas", "iba", "i'bamos", undefined, "iban"]
irregularConjugate "ver" t = case t of
	Present -> unpack ["veo", "ves", "ve", "vemos", undefined, "ven"]
	Preterite -> unpack ["vi", "viste", "vio", "vimos", undefined, "vieron"]
	Imperfect -> ("ve" ++) . regularEnding "ver" Imperfect
-- Present & Preterite
irregularConjugate "dar" t = case t of
	Present -> unpack ["doy", "das", "da", "damos", undefined, "dan"]
	Preterite -> unpack ["di", "diste", "dio", "dimos", undefined, "dieron"]
irregularConjugate "hacer" t = case t of
	Present -> unpack ["hago", "haces", "hace", "hacemos", undefined, "hacen"]
	Preterite -> unpack ["hice", "hiciste", "hizo", "hicimos", undefined, "hicieron"]
irregularConjugate "caer" t = case t of
	Present -> unpack ["caigo", "caes", "cae", "caemos", undefined, "caen"]
	Preterite -> unpack ["cai'", "cai'ste", "oyo'", "oi'mos", undefined, "cayeron"]
irregularConjugate "oír" t = case t of
	Present -> unpack ["oigo", "oyes", "oye", "oi'mos", undefined, "oyen"]
	Preterite -> unpack ["oi'", "oi'ste", "cayo'", "cai'mos", undefined, "oyeron"]
-- Present
irregularConjugate "estar" t = case t of
	Present -> unpack ["estoy", "esta's", "esta'", "estamos", undefined, "esta'n"]
irregularConjugate "salir" t = case t of
	Present -> unpack ["salgo", "sales", "sale", "salimos", undefined, "salen"]
irregularConjugate "decir" t = case t of
	Present -> unpack ["digo", "dices", "dice", "decimos", undefined, "dicen"]
irregularConjugate "saber" t = case t of
	Present -> unpack ["se'", "sabes", "sabe", "sabemos", undefined, "saben"]
irregularConjugate "seguir" t = case t of
	Present -> unpack ["sigo", "sigues", "sigue", "seguimos", undefined, "siguen"]
irregularConjugate "conseguir" t = ("con" ++) . irregularConjugate "seguir" t
irregularConjugate "traer" t = case t of
	Present -> unpack ["traigo", "traes", "trae", "traemos", undefined, "traen"]
-- Preterite
irregularConjugate "creer" t = case t of
	Preterite -> unpack ["crei'", "crei'ste", "creyo'", "crei'mos", undefined, "creyeron"]
irregularConjugate "leer" t = case t of
	Preterite -> unpack ["lei'", "lei'ste", "leyo'", "lei'mos", undefined, "leyeron"]
