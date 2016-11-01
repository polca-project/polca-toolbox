module AppTransformation where 

import Main as M (appTrans)

import System.Environment( getArgs )

showError = 
	putStrLn "Usage:\n\tpolca_apply filename transformation_step (all | next_rule) [polca_block].\nThe source file, the selected transformation step and the next rule to be applied (all if unknown) are needed.\nThe name of a defined block is optional."

appFun filename step rule def = 
	case (take 2 (reverse filename)) of 
		('c':('.':_)) ->
			M.appTrans (reverse (drop 2 (reverse filename))) ((read step)::Int) rule def
		_ ->
			M.appTrans filename ((read step)::Int) rule def	

main = do
	args <- getArgs
	case args of 
		[] -> 
			showError
		[_] -> 
			showError
		(filename:(step:(rule:(def:_)))) ->
			appFun filename step rule (Just def)
		(filename:(step:(rule:_))) ->
			appFun filename step rule Nothing
			 