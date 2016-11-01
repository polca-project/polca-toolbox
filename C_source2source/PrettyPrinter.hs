module PrettyPrinter where 

import Main as M (pretty_program)

import System.Environment( getArgs )

main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "A source file is needed."
		_ ->
			case (take 2 (reverse (args!!0))) of 
				('c':('.':_)) ->
					M.pretty_program (reverse (drop 2 (reverse (args!!0))))
				_ ->
					M.pretty_program (args!!0)	