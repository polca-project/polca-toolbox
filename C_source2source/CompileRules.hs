module CompileRules where 

import Rul2Has (stml2Has)

import System.Environment( getArgs )


main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "A rules source file is needed."
		_ ->
			case (take 2 (reverse (args!!0))) of 
				('c':('.':_)) ->
					stml2Has (reverse (drop 2 (reverse (args!!0))))
				_ ->
					stml2Has (args!!0)	