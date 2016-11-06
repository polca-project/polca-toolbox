-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module TestRules where 

import Main as M (trans_report, trans_report_auto, trans_report_explore)

import System.Environment( getArgs )

main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "A source file is needed."
		_ ->
			do 
				let file_name = 
					case (take 2 (reverse (args!!0))) of 
						('c':('.':_)) ->
							(reverse (drop 2 (reverse (args!!0))))
						_ ->
							(args!!0)	
				--M.trans_report file_name "ran" 20
				M.trans_report_auto file_name "ran"
				--M.trans_report_explore file_name 
				
			 