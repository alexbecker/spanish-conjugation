-- Translate ASCII -> Spanish alphabet
module SpanishParse where

import Data.Maybe
import Data.List

parseAccent :: String -> Char
parseAccent s = case s of
	"a'" -> 'á'
	"e'" -> 'é'
	"i'" -> 'í'
	"o'" -> 'ó'
	"n~" -> 'ñ'

parseSpanish :: String -> String
parseSpanish s = if isJust ind
	then (parseSpanish $ take (fromJust ind - 1) s) ++ parseAccent (take 2 $ drop (fromJust ind - 1) s) : (parseSpanish $ drop (fromJust ind + 1) s)
	else s
	where
		ind = findIndex (flip elem ['\'', '~']) s
