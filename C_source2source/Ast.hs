module Ast where 

import Main as M (ast2)

import System.Environment( getArgs )

main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "A source file is needed."
		_ ->
			case (take 2 (reverse (args!!0))) of 
				('c':('.':_)) ->
					M.ast2 (reverse (drop 2 (reverse (args!!0))))
				_ ->
					M.ast2 (args!!0)	
			 