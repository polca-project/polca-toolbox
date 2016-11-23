-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module IsRollable where 

import RulesLib
import PragmaPolcaLib

import Language.C

import System.Environment( getArgs )

main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "Two expressions and a directory to store temp files are needed."
		1 -> 
			putStrLn "Another expression and a directory to store temp files is needed."
		2 -> 
			putStrLn "A directory to store temp files is needed."
		_ ->
			-- TODO: Needs previuos parsing
			do 
				let dir = (args!!2)
				a0 <- parseExp (args!!0) dir
				a1 <- parseExp (args!!1) dir
				case (a0, a1) of 
					(Just a0_, Just a1_) ->
						case isRollable  a0_ a1_ of 
							(True,_) ->
								putStrLn "1"
							(False,_) ->
								putStrLn "0"
					_ ->
						putStrLn "-1"
			 

parseExp e dir = 
	do
		let tempFile = dir ++ "temp_file_read.c"
		writeFile tempFile ("main(){" ++ e ++ ";}")
		ast <- parseMyFile tempFile
		case (fmap (\nI -> Ann nI nodePropertiesDefault) ast) of
			(CTranslUnit 
				[(CFDefExt (CFunDef _ _ _ 
					(CCompound _ 
						[(CBlockStmt (CExpr (Just pe) _))] _) _))] _) ->
				return (Just pe)
			_ ->
				do 
					putStrLn $ "Error while parsing the expression.\n" ++ e ++ " is not a valid C expression."
					return Nothing