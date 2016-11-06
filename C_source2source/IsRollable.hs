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
				case isRollable  a0 a1	of 
					(True,_) ->
						putStrLn "1"
					(False,_) ->
						putStrLn "0"
			 

parseExp e = 
	do
		writeFile "temp_file_read.c" ("main(){" ++ e ++ ";}")
		ast <- parseMyFile "temp_file_read.c"
		let (CTranslUnit 
				[(CFDefExt (CFunDef _ _ _ 
					(CCompound _ 
						[(CBlockStmt (CExpr (Just pe) _))] _) _))] _) = 
			fmap (\nI -> Ann nI nodePropertiesDefault) ast
		return pe