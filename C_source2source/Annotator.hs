-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module Annotator where 

import Main as M (justAnnotate)

import System.Environment( getArgs )

detFileName args = 
	case (Prelude.take 2 (Prelude.reverse (args!!0))) of 
		('c':('.':_)) ->
			(Prelude.reverse (Prelude.drop 2 (Prelude.reverse (args!!0)))) 
		_ ->
			(args!!0)

errorMsg = 
	putStrLn "Usage:\n\tpolca_annotate filename\nThe source file is needed."

main = do
	args <- getArgs
	case length args of 
		0 -> 
			errorMsg
		1 ->
			justAnnotate (detFileName args)  
			 