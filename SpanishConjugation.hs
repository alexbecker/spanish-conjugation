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

-- Final conjugation

conjugate :: Verb -> Tense -> Person -> String
conjugate v t p = parseSpanish $ if irregular v t
	then irregularConjugate v t p
	else if isStemChange v
		then lastLetterRule (applyStemChange v t p) t p
		else lastLetterRule v t p

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
knownVerbs = sort ["ser", "estar", "tener", "hacer", "poder", "decir", "ir", "dar", "saber", "querer", "llegar", "pasar", "deber", "poner", "parecer", "quedar", "creer", "hablar", "llevar", "dejar", "seguir", "encontrar", "llamar", "venir", "pensar", "salir", "volver", "tomar", "conocer", "vivir", "sentir", "tratar", "mirar", "contar", "empezar", "esperar", "buscar", "existir", "entrar", "trabajar", "escribir", "perder", "producir", "ocurrir", "entender", "pedir", "recibir", "recordar", "terminar", "permitir", "aperecer", "conseguir", "comenzar", "servir", "sacar", "necesitar", "resultar"]

-- (UNIFORM) Regular verb handling

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

-- (UNIFORM) Last letter rules

-- assumes any stem changes have been applied
lastLetterRule :: Verb -> Tense -> Person -> Verb
lastLetterRule v t p = if isJust replacement
	then take (len - 3) conj ++ fromJust replacement ++ drop (len - 2) conj
	else conj
	where
		len = length v
		conj = regularConjugate v t p
		preceding = conj !! (len - 4)
		letter = conj !! (len - 3)
		following = conj !! (len - 2)
		replacement = case letter of
			'z' -> if elem following "eé"
				then Just "c"
				else Nothing
			'g' -> if elem following "eé"
				then Just "gu"
				else Nothing
			'c' -> if elem preceding "aeiouáéíó" && elem following "aoáó"
				then Just "zc"
				else if preceding == 's' && elem following "eé"
					then Just "qu"
					else Nothing
			'n' -> if elem preceding "aeiouáéíó" && elem following "aoáó"
				then Just "ng"
				else Nothing
			'l' -> if elem preceding "aeiouáéíó" && elem following "oó"
				then Just "lg"
				else Nothing
			_ -> Nothing

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
getStemChange "entender" = Just EtoIE
getStemChange "comenzar" = Just EtoIE
-- OtoUE
getStemChange "dormir" = Just OtoUE
getStemChange "doler" = Just OtoUE
getStemChange "encontrar" = Just OtoUE
getStemChange "volver" = Just OtoUE
getStemChange "contar" = Just OtoUE
getStemChange "recordar" = Just OtoUE
-- UtoUe
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
irregular "estar" _ = True
irregular "decir" _ = True
irregular "hacer" _ = True
irregular "poner" _ = True
irregular "saber" _ = True
irregular "dar" _ = True
irregular "venir" _ = True
irregular "tener" _ = True
irregular "seguir" Present = True
irregular "conseguir" Present = True
irregular "poder" Preterite = True
irregular "querer" Preterite = True
irregular "creer" Preterite = True
irregular "producir" Preterite = True
irregular _ _ = False

irregularConjugate :: Verb -> Tense -> Person -> String
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
irregularConjugate "venir" t = case t of
	Present -> unpack ["vengo", "vienes", "viene", "venemos", undefined, "venien"]
	Preterite -> unpack ["vine'", "viniste", "vino", "vinimos", undefined, "vinieron"]
irregularConjugate "dar" t = case t of
	Present -> unpack ["doy", "das", "da", "damos", undefined, "dan"]
	Preterite -> unpack ["di", "diste", "dio", "dimos", undefined, "dieron"]
irregularConjugate "seguir" t = case t of
	Present -> unpack ["sigo", "sigues", "sigue", "siguemos", undefined, "siguen"]
irregularConjugate "conseguir" t = case t of
	Present -> unpack ["consigo", "consigues", "consigue", "consiguemos", undefined, "consiguen"]
irregularConjugate "poder" t = case t of
	Preterite -> unpack ["pude", "pudiste", "pudo", "pudimos", undefined, "pudieron"]
irregularConjugate "querer" t = case t of
	Preterite -> unpack ["quise", "quisiste", "quiso", "quisimos", undefined, "quisieron"]
irregularConjugate "creer" t = case t of
	Preterite -> unpack ["crei'", "crei'ste", "creyo'", "crei'mos", undefined, "creyeron"]
irregularConjugate "producir" t = case t of
	Preterite -> unpack ["produje", "produjiste", "produjo", "produjimos", undefined, "produjieron"]
