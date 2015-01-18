module SpanishConjugationTypes where

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
