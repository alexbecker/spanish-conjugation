module ImmediateIO where

import System.IO

putStrImmediate :: String -> IO ()
putStrImmediate s = putStr s >> hFlush stdout

getCharImmediate :: IO Char
getCharImmediate = do
	buffering <- hGetBuffering stdin
	hSetBuffering stdin NoBuffering
	c <- getChar
	hSetBuffering stdin buffering
	putStrLn ""
	return c
