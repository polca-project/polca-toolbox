module Expander where 

import Main as M (expandAnns)

import System.Environment( getArgs )

main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "A source file is needed."
		_ ->
			case (take 2 (reverse (args!!0))) of 
				('c':('.':_)) ->
					M.expandAnns (reverse (drop 2 (reverse (args!!0))))
				_ ->
					M.expandAnns (args!!0)	
			 