module SpanishConjugationData where

import Data.Maybe
import Data.List
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

knownPersons = [Yo, Tu, El, Nosotros, Ellos]
knownVerbs = sort ["ser", "estar", "tener", "hacer", "poder", "decir", "ir", "dar", "saber", "querer", "llegar", "pasar", "deber", "poner", "parecer", "quedar", "creer", "hablar", "llevar", "dejar", "seguir", "encontrar", "llamar", "venir", "pensar", "salir", "volver", "tomar", "conocer", "vivir", "sentir", "tratar", "mirar", "contar", "empezar", "esperar", "buscar", "existir", "entrar", "trabajar", "escribir", "perder", "producir", "ocurrir", "entender", "pedir", "recibir", "recordar", "terminar", "permitir", "aperecer", "conseguir", "comenzar", "servir", "sacar", "necesitar", "mantener", "resultar", "leer", "caer", "cambiar", "presentar", "crear", "abrir", "considerar", "oír", "acabar", "convertir", "ganar", "formar", "traer", "partir", "morir", "aceptar", "realizar", "suponer", "comprender", "lograr", "explicar"]

-- Conjugation data is stored as lists by person, so unpack to functions
unpack :: [String] -> Person -> String
unpack cs p = cs !! case p of
	Yo -> 0
	Tu -> 1
	El -> 2
	Nosotros -> 3
	Vosotros -> 4
	Ellos -> 5

-- Stem changers

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

-- Preterite semiregulars

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

-- Irregular verbs

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
	Imperfect -> unpack ["vei'a", "vei'as", "vei'a", "vei'amos", undefined, "vei'an"]
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
