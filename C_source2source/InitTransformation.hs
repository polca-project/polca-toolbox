-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module InitTransformation where 

import Main as M (init_trans)

import System.Environment( getArgs )

appFun filename def = 
	case (take 2 (reverse filename)) of 
		('c':('.':_)) ->
			M.init_trans (reverse (drop 2 (reverse filename))) def
		_ ->
			M.init_trans filename def	 

main = do
	args <- getArgs
	case args of 
		[] -> 
			putStrLn "Usage:\n\tpolca_apply filename [polca_block].\nThe source file is mandatory.\nThe name of a defined block is optional."
		[filename] ->
			appFun filename Nothing
		(filename:(def:_)) ->
			appFun filename (Just def)	
			 