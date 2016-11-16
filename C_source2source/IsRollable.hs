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
			putStrLn "Two expressions are needed."
		1 -> 
			putStrLn "Another expression is needed."
		_ ->
			-- TODO: Needs previuos parsing
			do 
				a0 <- parseExp (args!!0)
				a1 <- parseExp (args!!1)
				case (a0, a1) of 
					(Just a0_, Just a1_) ->
						case isRollable  a0_ a1_ of 
							(True,_) ->
								putStrLn "1"
							(False,_) ->
								putStrLn "0"
					_ ->
						putStrLn "-1"
			 

parseExp e = 
	do
		writeFile "temp_file_read.c" ("main(){" ++ e ++ ";}")
		ast <- parseMyFile "temp_file_read.c"
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