-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship


{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}

module Main where

import RulesLib
import Rules
import PragmaPolcaLib
import Translator

import Language.C
import Language.C.Analysis

import Data.Generics
--import Data.Generics.Serialization.SExp
--import Data.Generics.Serialization.Streams
import Data.List
import Data.List.Split
-- import Data.Graph.AStar
import Data.Set as Set (fromList, toList, empty, union) 
import Data.Maybe
import Data.Char
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Binary as DBin
import Data.Aeson as JSON
import Data.Text as DT (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL


import Debug.Trace

import Control.Exception as CE
import Control.Monad
import Control.Monad.Trans.State
import Control.Lens ((^.))
--import Control.Lens

--import GHC.Vacuum.GraphViz 
--import GHC.Vacuum
import System.Process
import System.FilePath.Posix
import System.Exit
import System.Directory
import System.IO 
import System.IO.Unsafe
import System.Random 

import qualified Text.Groom as Gr
import Text.Read as TR



---------------------------------------------------------
-- Main functions
---------------------------------------------------------

main :: IO ()
main = return ()


transWithExt command filename polca_block = 
	do 
		state <- getTransInt filename "int" 0 (-1) 0 polca_block command
		to_platform filename state

trans_to_platform name = 
	trans_to_platformInt name (-1) 0 ""

trans_to_platformInt name seq_id print_id polca_block = 
	do 
		steps <- readSteps name
		let mode = 
			case steps of 
				[] -> 
					"int"
				_ -> 
					"aut"
		state <- getTransInt name mode 0 seq_id print_id polca_block ""
		to_platform name state

to_platform name state = 
	do 
		let finish_fun = 
			\() ->
				do 		
					askStoreSteps name state
					putStrLn ("Transformation process finished.\n")
		let platforms = 
			[
			"opencl",
			"mpi",
			"omp",
			"maxj",
			"plain c (with all anotations)",
			"plain c (without STML annotations)",
			"plain c (without any annotation)",
			"go back to transformation",
			"none"
			]
		let (question, answers,dict) = prepareQuestionAnswerRule platforms  1
		answer0 <- ask ("To what platform do you want to generate current code:\n" ++ question) answers
		case (head [plat | (answer_,plat) <- dict, answer_ == answer0]) of 
			"none" ->
				do 
					putStrLn ("No output generated.\n")
					finish_fun()
			"plain c (with all anotations)" -> 
				do 
					writeTransFile name state
					finish_fun()
			"plain c (without STML annotations)" ->
				do 
					writeTransFileOnlyPolca name state
					finish_fun()
			"plain c (without any annotation)" ->
				do 
					writeTransFileWithoutAnns name state
					finish_fun()
			"go back to transformation" ->
				do 
					nstate <- applyruleInt state  name Nothing False
					to_platform name nstate
			plat -> 
				do 
				-- TODO: Uncomment line below to enable translation
				--trans_platform_internal name plat (includes state) (fmap (\(Ann nI _) -> nI) (rebuildAst state)) (pragmas state) 
				trans_platform_internal name plat (includes state) (rebuildAst state)
				--writeTransFile name state
				finish_fun()

askStoreSteps name state = 
	do 
		answer0 <- 
			ask "Do you want to save the followed steps in a file"
				["y", "n"]
		case answer0 of 
			"n" ->
				putStrLn ("No steps file generated.\n")
			"y" ->
				writeFile (name ++ ".stp") (intercalate "\n" $ map show $ reverse (acc_steps state))


trans_rand_def iter =
	trans "test" "ran" iter

trans_rand file iter =
	trans file "ran" iter

trans_inter_def =
	trans "test" "int" 0

trans_inter file =
	trans file "int" 0

trans_auto file =
	trans file "aut" 0

ast name = 
	do
		ast <- parseMyFile (name ++ ".c")
		--putStrLn $ show $ annotation $ ast
		-- The fmap remove (all but Ident) nodeInfo to make a clearer AST output
		writeFile (name ++ ".ast") (Gr.groom (fmap (\_ -> ()) ast))

ast2 name = 
	do
		--ast <- parseMyFile (name ++ ".c")
		(ast, linkedPolcaAnn,includes) <- readFileInfo True name True
		--putStrLn $ show $ linkedPolcaAnn
		-- The fmap remove (all but Ident) nodeInfo to make a clearer AST output
		let annAST = fmap (\nI -> Ann nI nodePropertiesDefault) ast
		let changedAnnAST = changeAnnAST linkedPolcaAnn annAST
		let idAST = addPositionInfo changedAnnAST

		--writeFile (name ++ ".ast") (Gr.groom (fmap (\_ -> ()) ast))
		writeFile (name ++ ".ast") (Gr.groom idAST)

expandAnns name = 
	do
		(ast, linkedPolcaAnn,includes) <- readFileInfo True name True
		let lastNode = getLastNode ast
		let annAST = fmap (\nI -> Ann nI nodePropertiesDefault) ast
		let changedAnnAST = changeAnnAST linkedPolcaAnn annAST
		let state =
			TransState 
			{
				free_node_id = lastNode + 1, 
				--pragmas = linkedPolcaAnn,
				-- TODO: remove asts from the process since ast is now in the state
				--current_ast = ast1,
				freeVar = 0,
				includes = includes,
				fun_defs = applyRulesGeneral searchFunDefsGeneral changedAnnAST,
				no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral changedAnnAST,
				last_changed = "",
				previous_changes = ([],[]),
				applied_rules = [],
				applicable_rules = Set.empty,
				trans_with_anns = False,
				ast_to_transform = Nothing,
				print_id = 0,
				seq_id = -1,
				acc_steps = [],
				oracle = ""
			}
		--writeFile (filename ++ "_transformed.c") (prettyMyASTIncludes ast)
		writeFileWithPragmas (name ++ ".ann.c") state
		putStrLn ("Annotated code stored in " ++ name ++ ".ann.c")

pretty_program name = 
	do
		(ast, linkedPolcaAnn,includes) <- readFileInfo False name False
		let lastNode = getLastNode ast
		let annAST = fmap (\nI -> Ann nI nodePropertiesDefault) ast
		-- let changedAnnAST = changeAnnAST linkedPolcaAnn annAST
		let state =
			TransState 
			{
				free_node_id = lastNode + 1, 
				freeVar = 0,
				includes = includes,
				fun_defs = applyRulesGeneral searchFunDefsGeneral annAST,
				no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral annAST,
				last_changed = "",
				previous_changes = ([],[]),
				applied_rules = [],
				applicable_rules = Set.empty,
				trans_with_anns = False,
				ast_to_transform = Nothing,
				print_id = 0,
				seq_id = -1,
				acc_steps = [],
				oracle = ""
			}
		-- writeFileWithPragmas (name ++ ".ann.c") state
		putStrLn (printWithoutPragmas state)

only_translate name = 
	do
		(ast, linkedPolcaAnn,includes) <- readFileInfo True name True
		let lastNode = getLastNode ast
		let annAST = fmap (\nI -> Ann nI nodePropertiesDefault) ast
		let changedAnnAST = changeAnnAST linkedPolcaAnn annAST
		let state =
			TransState 
			{
				free_node_id = lastNode + 1, 
				--pragmas = linkedPolcaAnn,
				-- TODO: remove asts from the process since ast is now in the state
				--current_ast = ast1,
				freeVar = 0,
				includes = includes,
				fun_defs = applyRulesGeneral searchFunDefsGeneral changedAnnAST,
				no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral changedAnnAST,
				last_changed = "",
				previous_changes = ([],[]),
				applied_rules = [],
				applicable_rules = Set.empty,
				trans_with_anns = False,
				ast_to_transform = Nothing,
				print_id = 0,
				seq_id = -1,
				acc_steps = [],
				oracle = ""
			}
		--writeFile (filename ++ "_transformed.c") (prettyMyASTIncludes ast)
		to_platform name state
		

trans name mode iter = 
	do
		state <- getTrans name mode iter 
		writeTransFile name state


initial_steps_test :: String -> IO ()
initial_steps_test name =
	do 
		currentDirectory <- getCurrentDirectory
		let directoryFile = outputDirectory name
		setCurrentDirectory directoryFile
		let compileCommand = "make > compile0_result.txt 2> compile0_errors.txt"
		compileResult <- system compileCommand
		case compileResult of 
			ExitSuccess ->
				--do 
				--	contents <- readFile "compile0_result.txt"
				--	putStrLnCond verbose ("\n" ++ compileCommand ++ "\n" ++ contents ++ "\n\nRun make\n")
				putStrLn ("Start run make.\n")
			_ ->
				do 
					contents <- readFile "compile0_errors.txt"
					fail (contents ++ "\n\n\nError while executing make")
		setCurrentDirectory currentDirectory 
		putStrLn ("End run make.\n")


trans_report name mode iter = 
	do
		initial_steps_test name
		state <- getTrans name mode 1
		putStrLn ("Iter: " ++ (show iter))
		check_continue name state iter 

trans_report_auto name mode =
	do
		initial_steps_test name
		state <- getTrans name mode 1
		check_continue_auto name state 10 

trans_report_explore name =
	do
		initial_steps_test name
		state0 <- initialStepsTrans True name True
		trans_report_aux_explore name state0 [] state0 []
		--(state, stack, stateRep, selectableRules) <- applyOneRuleExploring state0 [] state0 []
		--check_continue_explore name state stack stateRep selectableRules
 
trans_report_aux name state 0 = 
	do 
		putStrLn "Tested rules are OK"
		report_rules_not_applied state
trans_report_aux name state0 iter = 
	do
		state <- applyNrule state0 1
		putStrLn ("Iter: " ++ (show iter))
		check_continue name state iter 

trans_report_aux_auto name state 0 = 
	do 
		putStrLn "Tested rules are OK"
		report_rules_not_applied state
trans_report_aux_auto name state0 iter = 
	do
		(state, niter) <- 
			--trace 
			--	(show iter) 
				applyOneRuleLimited iter 10 state0
		check_continue_auto name state niter 

trans_report_aux_explore _ _ _ stateRep ["none"] = 
	do 
		putStrLn "Tested rules are OK"
		report_rules_not_applied stateRep
trans_report_aux_explore name state0 stack0 stateRep0 selectableRules0 = 
	do
		(state, stack, stateRep, selectableRules) <- applyOneRuleExploring state0 stack0 stateRep0 selectableRules0
		check_continue_explore name state stack stateRep selectableRules
		--trace  
		--	("\n********\nCurrentStack: " ++ (show [sr | (_,sr) <-  stack])
		--	++ "\n********\nApplied rules: " ++ (show $ reverse $ applied_rules state))
			--check_continue_explore name state stack stateRep selectableRules


check_continue name state iter =
	do 
		-- Change last parameter to define verbosity
		result <- check_result name state False
		case result of 
			True -> 
				trans_report_aux name state (iter - 1)
			False ->
				do 
					putStrLn $ "ERROR: There was an error after applying last rule: " ++ (head $ applied_rules state)
					report_rules_not_applied state

check_continue_auto name state iter =
	do 
		-- Change last parameter to define verbosity
		result <- check_result name state False
		case result of 
			True -> 
				trans_report_aux_auto name state iter
			False ->
				do 
					putStrLn $ "ERROR: There was an error after applying last rule: " ++ (head $ applied_rules state)
					report_rules_not_applied state

check_continue_explore name state stack stateRep selectableRules =
	do 
		-- Change last parameter to define verbosity
		result <- check_result name state False
		case result of 
			True -> 
				trans_report_aux_explore name state stack stateRep selectableRules
			False ->
				do 
					putStrLn $ "ERROR: There was an error after applying last rule: " ++ (head $ applied_rules state)
					report_rules_not_applied stateRep



report_rules_not_applied state = 
	do 
		putStrLn ("\n\n\tRules applied: " ++ (show (applied_rules state) ))
		putStrLn ("\n\n\tRules not applied: " ++ (show (nameRules \\ (applied_rules state) )))
		putStrLn ("\n\tRules that could not be applied because their patterns or conditions: " ++ (show (nameRules \\ (Set.toList $ applicable_rules state) )))
		putStrLn ("\n\tRules that could be applied, but has not be chosen: " ++ (show ((Set.toList $ applicable_rules state) \\ (applied_rules state) )))
		putStrLn ("\n")


check_result name state verbose =
	do 
		writeTransFileReport verbose name state
		let directoryFile = outputDirectory name
		setCurrentDirectory directoryFile
		--let compileComand = "gcc -std=gnu99 -w -c " ++ name ++ "_transformed.c -o " ++ name ++ ".o; gcc -std=gnu99 -w IO.c " ++ name ++ ".o -o " ++ name ++ ".x > compile_result.txt 2> compile_errors.txt"
		let compileComand2 = "make trans > compile_result.txt 2> compile_errors.txt"
		let runCommand = name ++ "_transformed.x > run_result.txt 2> run_errors.txt"
		let checkCommand = "./check_output.x > check_result.txt 2> check_errors.txt"
		compileResult <- system compileComand2
		case compileResult of 
			ExitSuccess ->
				do 
					contents <- readFile "compile_result.txt"
					putStrLnCond verbose ("\n" ++ compileComand2 ++ "\n" ++ contents ++ "\n\nCompiled file " ++ name ++ "_transformed.c\n")
			_ ->
				do 
					contents <- readFile "compile_errors.txt"
					fail (contents ++ "\n\n\nError while compiling " ++ name ++ ".c")
		runResult <- system runCommand
		case runResult of 
			ExitSuccess ->
				do
					contents <- readFile "run_result.txt"
					putStrLnCond verbose ("\n" ++ runCommand ++ "\n" ++ contents ++ "\n\nRun executable from " ++ name ++ "_transformed.c\n")
			_ ->
				do 
					contents1 <- readFile "run_result.txt"
					contents2 <- readFile "run_errors.txt"
					fail (contents1 ++ "\n" ++ contents2 ++ "\n\n\nError while runing executable from " ++ name ++ ".c")
		checkResult <- system checkCommand
		case checkResult of 
			ExitSuccess ->
				do 
					contents <- readFile "check_result.txt"
					putStrLnCond verbose ("\n" ++ checkCommand ++ "\n" ++ contents ++ "\n\nChecked result of executable from " ++ name ++ "_transformed.c\n")
			_ ->
				do 
					contents1 <- readFile "check_result.txt"
					contents2 <- readFile "check_errors.txt"
					fail (contents1 ++ "\n" ++ contents2 ++ "\n\n\nError while runing checker.")
		contents <- readFile "check_result.txt"
		let lastLine = last (lines contents)
		let returned = 
			case lastLine of 
				"0" ->
					True
				_ ->
					False
		let cleanerCommand = 
			"rm -f " ++ name ++ "_transformed.c;"
			++ "rm -f " ++ name ++ "_transformed.x;"
			++ "rm -f " ++ name ++ "_transformed.o;"
			++ "rm -f *_result.txt;"
			++ "rm -f *_errors.txt"
		_ <- system cleanerCommand
		return returned


writeTransFileReport True name state =
	writeTransFile name state
writeTransFileReport False name state =
	writeFileWithPragmas (name ++ "_transformed.c") state

writeTransFile name state =
	writeTransFileInt name state "" "_transformed.c"

writeTransFileInt name state header suffix =
	do 
		writeFileWithPragmasInt (name ++ suffix) state header
		putStrLn ("Transformed code stored in " ++ name ++ suffix)

writeTransFileOnlyPolca name state = 
	writeTransFileOnlyPolcaInt name state "" "_transformed_wo_stml_anns.c"

writeTransFileOnlyPolcaInt name state header suffix =
	do 
		writeFileOnlyPolcaPragmasInt (name ++ suffix) state header
		putStrLn ("Transformed code stored in " ++ name ++ suffix)

writeTransFileWithoutAnns name state =
	writeTransFileWithoutAnnsInt name state "" "_transformed_wo_anns.c"

writeTransFileWithoutAnnsInt name state header suffix =
	do 
		writeFileWithoutPragmasInt (name ++ suffix) state header
		putStrLn ("Transformed code stored in " ++ name ++ suffix)

outputDirectory file = 
	case isAbsolute file of 
		True ->
			dropFileName file
		False -> 
			"../" ++ (dropFileName file)

data ChangeCode = 
	ChangeCode
	{ 
		idChange :: Int,
		ruleName :: String,
		line :: Int,
		oldCode  :: String,
		newCode :: String,
		-- newCodeBlock :: String,
		-- newCodeFun :: String,
		newCodeAll :: String
		-- termPosition :: String
	} deriving Show

data CodeAndChanges = 
	CodeAndChanges
	{ 
		code :: String,
		-- codeBlock :: String,
		-- codeFun :: String,
		changes :: [ChangeCode]
	} deriving Show

instance FromJSON ChangeCode where
    parseJSON (Object v) = ChangeCode <$>
    					   v .: DT.pack "idChange" <*>
                           v .: DT.pack "ruleName" <*>
                           v .: DT.pack "line" <*>
                           v .: DT.pack "oldCode" <*>
                           v .: DT.pack "newCode" <*>
                           -- v .: DT.pack "newCodeBlock" <*>
                           -- v .: DT.pack "newCodeFun"
                           v .: DT.pack "newCodeAll"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON CodeAndChanges where
    parseJSON (Object v) = CodeAndChanges <$>
    					   v .: DT.pack "code" <*>
    					   -- v .: DT.pack "codeBlock" <*>
    					   -- v .: DT.pack "codeFun" <*>
                           v .: DT.pack "changes"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance ToJSON ChangeCode where
    toJSON (ChangeCode idChange ruleName line oldCode newCode newCodeAll) = 
    	object [
    			DT.pack "idChange" .= idChange, 
    			DT.pack "ruleName" .= ruleName, 
    			DT.pack "line" .= line,
    			DT.pack "oldCode" .= oldCode, 
    			DT.pack "newCode" .= newCode,
    			DT.pack "newCodeAll" .= newCodeAll
    			-- DT.pack "newCodeBlock" .= newCodeBlock,
    			-- DT.pack "newCodeFun" .= newCodeFun]
    			]

instance ToJSON CodeAndChanges where
    toJSON (CodeAndChanges code changes) = 
    	object [
    			DT.pack "code" .= code, 
    			-- DT.pack "codeBlock" .= codeBlock, 
    			-- DT.pack "codeFun" .= codeFun, 
    			DT.pack "changes" .= changes
    			]

featuresExtract filename rules block = 
	do 
		iniState <- initialStepsTrans False filename True
		let astsDef = 
			case block of 
				(Just def) ->
					applyRulesGeneral (search_def def) (rebuildAst iniState)
				Nothing ->
					[]
		let funs = 
			[f | (rule1, f) <- dictRules, elem rule1 rules]
		case astsDef of 
			[] ->
				-- putStrLn $ buildJSON  iniState (getApplicableChangesSpecRules iniState rules)
				putStrLn $ buildJSON  iniState (getApplicableChanges funs iniState{ast_to_transform = Nothing})
			(astDef:_) ->
				-- putStrLn $ buildJSON  iniState (getApplicableChangesSpecRulesGivenAst iniState rules astDef)
				let 
					nstate = 
						iniState{ast_to_transform = Just (fromMaybe "" block, astDef, searchASTFun astDef (fun_defs iniState))}
				in 
					putStrLn $ buildJSON  nstate (getApplicableChanges funs nstate)
		-- putStrLn $ filename ++ "\n" ++ (show rules) ++ "\n" ++ (fromMaybe "ALL" block)

init_trans name defM =
	do
		iniState <- initialStepsTrans False name True
		let (nameDef, astsDef) = 
			case defM of 
				(Just def) ->
					(def, applyRulesGeneral (search_def def) (rebuildAst iniState))
				Nothing ->
					([], [])
		case astsDef of 
			[] ->
				init_trans_int name iniState (getApplicableChanges [] iniState)
			(astDef:_) ->
				let 
					nIniState = 
						iniState{ast_to_transform = Just (nameDef, astDef, searchASTFun astDef (fun_defs iniState))}
				in 
					init_trans_int name nIniState (getApplicableChanges [] iniState) --(getApplicableChangesForGivenAst iniState astDef)
		

search_def :: String -> CStatAnn -> [CStatAnn]
search_def name stmt = 
	case [def|def@("def":name1:_) <- (extractPolcaPragmas (getAnnotation stmt)), name1 == name] of
		[] ->
			[]
		_ ->
			[stmt]

searchTermPosition :: NodeAnn -> NodeAnn -> [String]
searchTermPosition (Ann nIS _) (Ann nIF nP) = 
	case (geq nIS nIF) of 
		True -> 
			[nP^.term_position]
		False ->
			[]


init_trans_int name iniState changes = 
	do
		DBin.encodeFile (name ++ ".ser") iniState
		putStrLn (buildJSON iniState changes) 

appTrans name selected rule defM = 
	do
		modState <- getModifiedState name selected
		let astsDef = 
			case defM of 
				(Just def) ->
					applyRulesGeneral (search_def def) (rebuildAst modState)
				Nothing ->
					[]
		case astsDef of 
			[] ->
				app_trans_int name modState (getApplicableChangesSpecRules modState [rule])
			(astDef:_) ->
				app_trans_int name modState (getApplicableChangesSpecRulesGivenAst modState [rule] astDef)

app_trans_int name modState changes = 
	putStrLn (buildJSON modState changes) 

printMyASTBlock astToPrint state = 
	case (ast_to_transform state) of 
		Nothing ->
			prettyMyASTAnn astToPrint
		(Just (polca_block, _ , _)) ->
			
				case (applyRulesGeneral (search_def polca_block) astToPrint) of 
					[] ->
						prettyMyASTAnn astToPrint
					(astBlock:_) ->
						prettyMyASTAnn astBlock

buildJSON state0 (listChangesStmts,listChangesExprs) = 
	do
		let state = updateASTToTransform state0
		-- let state = addPositionInfoState state1
		-- let newBlockPrinterS = 
		-- 	\(fun,old, new) ->
		-- 		case (ast_to_transform state) of 
		-- 			Nothing -> 
		-- 				("", "")
		-- 			(Just  (polca_block, _, _)) ->
		-- 				let 
		-- 					nstate = state{fun_defs = (changeASTFun (fun,old, new) state)}
		-- 				in 
		-- 					case (applyRulesGeneral (search_def polca_block) (rebuildAst nstate)) of 
		-- 						[] ->
		-- 							("", "")
		-- 						(newBlock:_) ->
		-- 							(
		-- 								printWithPragmasStmt prettyPragmasPolca (searchASTFun newBlock (fun_defs nstate)),
		-- 								(printMyASTBlock newBlock nstate)
		-- 							)
		-- let newBlockPrinterE = 
		-- 	\(fun,old, new) ->
		-- 		case (ast_to_transform state) of 
		-- 			Nothing -> 
		-- 				("", "")
		-- 			(Just  (polca_block, _, _)) ->
		-- 				let 
		-- 					nstate = state{fun_defs = (changeASTFun (fun,old, new) state)}
		-- 				in 
		-- 					case (applyRulesGeneral (search_def polca_block) (rebuildAst nstate)) of 
		-- 						[] ->
		-- 							("", "")
		-- 						(newBlock:_) ->
		-- 							(
		-- 								printWithPragmasStmt prettyPragmasPolca (searchASTFun newBlock (fun_defs nstate)),
		-- 								(printMyASTBlock newBlock nstate)
		-- 							)
		let newBlockPrinterS = 
			\(fun,old, new) ->
				printWithPragmasInt prettyPragmasPolca state{fun_defs = (changeASTFun (fun,old, new) state)}
		let newBlockPrinterE = 
			\(fun,old, new) ->
				printWithPragmasInt prettyPragmasPolca state{fun_defs = (changeASTFun (fun,old, new) state)}
		let cS = 
			[(rule_name, searchMinMaxLine old, (printMyASTBlock old state), (printMyASTBlock new state), newBlockPrinterS (fun,old, new)) 
			 | (fun, ((rule_name,old,new), _, [])) <- listChangesStmts]
		let cE = 
			[(rule_name, searchMinMaxLine old, (printMyASTBlock old state), (printMyASTBlock new state), newBlockPrinterE (fun,old, new)) 
			 | (fun, ((rule_name,old,new), _, [])) <- listChangesExprs]
		let allChanges = 
			cE ++ cS 
		let idedChanges = 
			zip [0..((length allChanges) - 1)] allChanges
		let strProg = 
			-- printWithPragmas state
			printWithPragmasInt prettyPragmasPolca state
		let (strBlock, strFun) = 
			case (ast_to_transform state) of 
				Nothing ->
					("", "")
				(Just (_,astBlock,_)) ->
					-- "{\n" ++ (printChangeWithoutAnns astBlock) ++ "}" 
					(prettyMyASTAnn astBlock,
				     printWithPragmasStmt prettyPragmasPolca (searchASTFun astBlock (fun_defs state)))
		let linesInclude = 
			-- length $ includes state
			0
		let jsonContent = 
			CodeAndChanges 
				{
				 code = 
				 	strProg,
				 -- codeBlock = 
				 -- 	strBlock,
				 -- codeFun = 
				 -- 	strFun,
				 changes = 
					[ChangeCode {
						idChange = id, 
						ruleName = r, 
						line = l + linesInclude, 
						oldCode = o, 
						newCode = n, 
						newCodeAll = nall
						-- newCodeBlock = nb, 
						-- newCodeFun = nf
					} 
					 | (id, (r, l, o, n, nall)) <- idedChanges]}
		BSL.unpack (JSON.encode jsonContent)

getTermPositionE :: CExprAnn -> TransState -> String
getTermPositionE term state = 
	-- case (applyRulesGeneral (searchTermPosition (getAnnotation term)) (rebuildAst state)) of
	-- 	[] ->
	-- 		""
	-- 	(someRes:_) ->
	-- 		someRes
	""

getTermPositionS :: CStatAnn -> TransState -> String
getTermPositionS term state = 
	case (applyRulesGeneral (searchTermPosition (annotation term)) (rebuildAst state)) of
		[] ->
			""
		(someRes:_) ->
			someRes


getModifiedState name selected = 
	do
		iniState <- DBin.decodeFile (name ++ ".ser") 
		let changes@(listChangesStmts0,listChangesExprs0) = 
			case (ast_to_transform iniState) of 
				Nothing ->
					getApplicableChangesWOPrevious iniState
				Just (_,astDef, _) ->
					getApplicableChangesForGivenAst iniState astDef
		let listChangesExprsToStmts = 
			[(True, (fun, ((rule,(CExpr (Just old) undefNodeAnn),(CExpr (Just new) undefNodeAnn)),nstate, []))) 
			 | (fun, ((rule,old,new),nstate, [])) <- listChangesExprs0]
		let listChangesStmtsToStmts = 
			[(False, item) 
			 | item@(_,((rule,_,_),_,[])) <- listChangesStmts0]
		let allChanges = 
			listChangesStmtsToStmts ++ listChangesExprsToStmts
		let (isExpr, (fun, ((_,oldS,newS),nstate,_))) = 
				allChanges!!selected
		let nfun_defs = 
			case isExpr of 
				False -> 
					changeASTFun (fun,oldS,newS) iniState
				True -> 
					changeASTFun (fun,removeStmtForExpr oldS,removeStmtForExpr newS) iniState
		let modState = 
	 		nstate
	 		{
	 			previous_changes = ([], []),
	 			trans_with_anns = (trans_with_anns iniState),
	 			fun_defs = nfun_defs,
	 			last_changed = fun
	 		}
	 	DBin.encodeFile (name ++ ".ser") modState
	 	-- writeTransFileWithoutAnns (name ++ ".temp1") iniState
	 	-- writeTransFileWithoutAnns (name ++ ".temp2") modState
	 	return modState

getApplicableChangesSpecRules :: TransState -> [String]
		->	([(String,
              ((String, CStatAnn, CStatAnn), TransState, [(String, CStatAnn)]))],
             [(String,
              ((String, CExprAnn, CExprAnn), TransState, [(String, CStatAnn)]))]) 
getApplicableChangesSpecRules state rules = 
	case [f | (rule1,f) <- dictRules, elem rule1 rules] of 
		[] ->
			getApplicableChangesWOPrevious state
		funs ->
			let 
				rulesForStmts = [fun | (Right fun) <- funs]
				rulesForExprs = [fun | (Left fun) <- funs]
			in
			(filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesWOPrevious rulesForStmts state),
			 filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesWOPrevious rulesForExprs state))
			-- case funs of 
			-- 	[Left fun] ->
			-- 		([], filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesWOPrevious [fun] state))
			-- 	[Right fun] ->
			-- 		(filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesWOPrevious [fun] state), [])

getApplicableChangesSpecRulesGivenAst state rules ast = 
	case [f | (rule1, f) <- dictRules, elem rule1 rules] of 
		[] ->
			getApplicableChangesForGivenAst state ast
		funs ->
			let 
				rulesForExprs = [fun | (Left fun) <- funs]
				rulesForStmts = [fun | (Right fun) <- funs]
			in
			(filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesForGivenAst rulesForStmts ast state),
			 filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesForGivenAst rulesForExprs ast state))
			-- case funs of 
			-- 	[Left fun] ->
			-- 		([], filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesForGivenAst [fun] ast state))
			-- 	[Right fun] ->
			-- 		(filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesForGivenAst [fun] ast state), [])

getApplicableChangesForGivenAst state ast = 
	let -- extractRules :: (Typeable a, Typeable b) => [a] -> [b]

		rulesForStmts = extractRulesForGivenAst stmtRules ast state
		rulesForExprs = extractRulesForGivenAst exprRules ast state
	in 
		(filter (\(_,((_,o,n),_,_)) -> not (geq o n)) rulesForStmts, 
		 filter (\(_,((_,o,n),_,_)) -> not (geq o n)) rulesForExprs)

extractRulesForGivenAst :: (Data t) => [TransState -> t -> [((String, t, t), TransState,[(String, CStatAnn)])]] -> CStatAnn -> TransState -> [(String, ((String, t, t), TransState,[(String, CStatAnn)]))]
extractRulesForGivenAst listRules astApplicable state = 
	(nubBy geq 
		(concat 
			[
				concat
				[
					[(fun_name, change) 
					| change <- (applyRulesGeneral (rule state) astApplicable)] 
				| (fun_name, _, ast) <- fun_defs state, containsAst ast astApplicable]  
			| rule <- listRules]))

containsAst astAll astSearched = 
	case (applyRulesGeneral (astEqualTo astSearched) astAll) of 
		[] ->
			False
		_ ->
			True

astEqualTo astSearched ast = 
	case (geq ast astSearched) of 
		True ->
			[ast]
		False ->
			[]

initialStepsTrans verbose name runCetus = 
	initialStepsTransInt verbose name (-1) 0 runCetus
 
initialStepsTransInt verbose name seqId printId runCetus = 
	do 
		(ast00, linkedPolcaAnn, includes0) <- readFileInfo verbose name True
		-- case (geq annAST00 ast000) of 
		-- 	True ->
		-- 		putStrLn "Nothin' changed."
		-- 	False ->
		-- 		do 
		-- 			writeFile "a.aaa" (Gr.groom linkedPolcaAnn)
		-- 			-- writeFile "a0.ast" (Gr.groom (fmap (\(Ann nI nP) -> nP) annAST00))
		-- 			-- writeFile "a1.ast" (Gr.groom (fmap (\(Ann nI nP) -> nP) ast000))
		-- 			writeFile "a0.ast" (Gr.groom ast00)
		-- 			writeFile "a1.ast" (Gr.groom annAST00)
		-- 			writeFile "a2.ast" (Gr.groom ast000)
		-- 		-- putStrLn $ "Somethin' changed." ++ (ppDiff (getGroupedDiff (lines $ Gr.groom annAST00) (lines $ Gr.groom ast000)))
		(ast1, lastNode) <-
			case runCetus of 
				False ->
					do 
						let annAST00 = fmap (\nI -> Ann nI nodePropertiesDefault) ast00
						let ast000 = changeAnnAST linkedPolcaAnn annAST00
						return (ast000, getLastNode ast00)
				True ->
					do
						-- TODO: Delete .cetus.c previous file OR delete it after use
						let normAST = normalize ast00
						let annAST00 = fmap (\nI -> Ann nI nodePropertiesDefault) normAST
						let ast000 = changeAnnAST linkedPolcaAnn annAST00
						putStrLnCond verbose ("Annotating the code using Cetus...")
						let initialState00  = 
							TransState 
							{
								free_node_id = 0, 
								freeVar = 0,
								includes = [],
								fun_defs = applyRulesGeneral searchFunDefsGeneral ast000,
								no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral ast000,
								last_changed = "",
								previous_changes = ([],[]),
								applied_rules = [],
								applicable_rules = Set.empty,
								trans_with_anns = False,
								ast_to_transform = Nothing,
								print_id = printId,
								seq_id = seqId,
								acc_steps = [],
								oracle = ""
							} 
						writeFile (name ++ ".cetus") (printWithPragmasWithoutStdLib initialState00)
						--writeFile (name ++ ".cetus") ((unlines includes0) ++ "\n\n" ++ (prettyMyAST ast00) ++ "\n")
						let directoryToCetus = outputDirectory name
						let cetusCommand = "cetus/cetus_stml.sh " ++ (directoryToCetus ++ (takeFileName name)) ++ ".cetus " ++ directoryToCetus ++ " > cetus/output.txt 2> cetus/output.txt"
						--putStrLnCond verbose cetusCommand
						--ExitSuccess <- system "cd cetus"
						--ExitSuccess <- system cetusCommand
						--let annotatedFile = name ++ ""
						let annotatedFile = name ++ ".cetus.cetus"
						execResult <- system cetusCommand
						case execResult of 
							ExitSuccess ->
								putStrLnCond verbose ("STML Annotated code stored in " ++ annotatedFile ++ ".c")
							_ ->
								do 
									contents <- readFile "cetus/output.txt"
									--return (error contents)
									fail (contents ++ "\n\n\nError while annotating using Cetus.")
						(ast0, linkedPolcaAnn,_) <- readFileInfo verbose annotatedFile True
						let lastNode1 = getLastNode ast0

						let annAST = fmap (\nI -> Ann nI nodePropertiesDefault) ast0
						let ast2 = changeAnnAST linkedPolcaAnn annAST
						writeFile (name ++ ".ast") (Gr.groom ast2)
						--writeFile (name ++ ".ast") (show ast2)
						return (ast2, lastNode1)
		-- let ast1 = ast2
		----print lastNode 
		let initialState0  = 
			TransState 
			{
				free_node_id = lastNode + 1, 
				--pragmas = linkedPolcaAnn,
				-- TODO: remove asts from the process since ast is now in the state
				--current_ast = ast1,
				freeVar = 0,
				includes = includes0,
				fun_defs = applyRulesGeneral searchFunDefsGeneral ast1,
				no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral ast1,
				last_changed = "",
				previous_changes = ([],[]),
				applied_rules = [],
				applicable_rules = Set.empty,
				trans_with_anns = False,
				ast_to_transform = Nothing,
				print_id = printId,
				seq_id = seqId,
				acc_steps = [],
				oracle = ""
			} 
		--DBin.encodeFile "temp" initialState0
		--initialState <- DBin.decodeFile "temp" 
		--putStrLn (show initialState)
		return initialState0

getTrans name mode iter =
	getTransInt name mode iter (-1) 0 "" ""

getTransInt name mode iter seq_id print_id polca_block command = 
	do
		initialState0 <- initialStepsTransInt True name seq_id print_id True
		let initialState1 = 
			case polca_block of 
				"" ->
					initialState0
				_ ->
					do 
						let astDef = 
							case (applyRulesGeneral (search_def polca_block) (rebuildAst initialState0)) of 
								[] ->
									Nothing
								(first:_) ->
									(Just (polca_block, first, searchASTFun first (fun_defs initialState0)))
						initialState0{ast_to_transform = astDef}
		let initialState = 
			initialState1{oracle = command}
		nstate <- 
			--case (trace (show (geq ast1 ast0)) mode) of 
			--case (trace (show ast1) mode) of 
			case mode of 
				"ran" ->
					applyNrule initialState iter 
					--applyNrule 0 iter ast1
				"int" ->
					applyruleInt initialState name Nothing True
				"aut" ->
					do 
						steps <- readSteps name
						--putStrLn (show steps)
						applyruleInt initialState  name (Just steps) True
		--let ast3 = rebuild_ast nstate
		return nstate

searchASTFun ast funDefs =
	let 
		body = 
			-- [fun_body | (_, _, fun_body) <- funDefs, containsAst ast fun_body]  
			[fun_body | (_, _, fun_body) <- funDefs, containsAst fun_body ast]  
	in 
		-- head  $ trace (show $ length body) body
		head body

updateASTToTransform state = 
	let nast_to_transform = 
		case (ast_to_transform state) of 
			Nothing ->
				Nothing 
			(Just (polca_block, _, _)) ->
				case (applyRulesGeneral (search_def polca_block) (rebuildAst state)) of 
					[] ->
						Nothing
					(first:_) ->
						let 
							funAST = searchASTFun first (fun_defs state)
						in 
							(Just (polca_block, first, funAST))
	in 
		state{ast_to_transform = nast_to_transform}

--getApplicableChanges :: TransState -> ([(String, ((String,CStatAnn,CStatAnn), TransState))], [(String, ((String,CExprAnn,CExprAnn), TransState))])
getApplicableChanges :: [Either 
							(TransState -> CExprAnn -> [((String, CExprAnn, CExprAnn), TransState,[(String, CStatAnn)])])
							(TransState -> CStatAnn -> [((String, CStatAnn, CStatAnn), TransState,[(String, CStatAnn)])]) ] 
						-> TransState 
						-> ([(String,
	                          ((String, CStatAnn, CStatAnn), TransState, [(String, CStatAnn)]))],
	                        [(String,
	                          ((String, CExprAnn, CExprAnn), TransState, [(String, CStatAnn)]))])
getApplicableChanges funs state =
	case funs of 
		-- [Left fun] ->
		-- 	trace "gac1" ([], filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesWOPrevious [fun] state))
		-- [Right fun] ->
		-- 	trace "gac2" (filter (\(_,((_,o,n),_,_)) -> not (geq o n)) (extractRulesWOPrevious [fun] state), [])
		_ ->
			let 
				nstate = updateASTToTransform state
				choosenRulesForStmts = 
					case funs of 
						[] ->
							stmtRules
						_ ->
							[fun | (Right fun) <- funs]
				choosenRulesForExprs = 
					case funs of 
						[] ->
							exprRules
						_ ->
							[fun | (Left fun) <- funs]
				rulesForStmts = extractRules choosenRulesForStmts nstate
				--rulesForStmts = []
				rulesForExprs = extractRules choosenRulesForExprs nstate
				(prevRulesForStmts0, prevRulesForExprs0) = 
					-- trace ((show (length rulesForStmts)) ++ " " ++ (show (length rulesForExprs))) 
					previous_changes nstate
				--(prevRulesForStmts0, prevRulesForExprs0) = ([],[])
				prevRulesForStmts = 
					[item | item@(fun_name, _) <- prevRulesForStmts0, fun_name /= (last_changed nstate)]
				prevRulesForExprs = 
					[item | item@(fun_name, _) <- prevRulesForExprs0, fun_name /= (last_changed nstate)]
				filterFuns = 
					case (ast_to_transform nstate) of 
						Nothing ->
							((\(_,((_,o,n),_,_)) -> not (geq o n)),
							 (\(_,((_,o,n),_,_)) -> not (geq o n)))
						(Just (polca_block, oldBlock, _)) ->
							((\(_,((_,o,n),_,_)) -> 
								let 
									newAST = changeAST (o, n) (rebuildAst state)
								in 
									case (applyRulesGeneral (search_def polca_block) newAST) of 
										[] ->
											False
										(newBlock:_) ->
											not (geq oldBlock newBlock)
							 ),
							 (\(_,((_,o,n),_,_)) -> 
								let 
									newAST = changeAST (o, n) (rebuildAst state)
								in 
									case (applyRulesGeneral (search_def polca_block) newAST) of 
										[] ->
											False
										(newBlock:_) ->
											not (geq oldBlock newBlock)
							 ))
				changes = 
					(filter (fst filterFuns) (rulesForStmts ++ prevRulesForStmts),
					 filter (snd filterFuns) (rulesForExprs ++ prevRulesForExprs))
					--((rulesForStmts ++ prevRulesForStmts),
					-- (rulesForExprs ++ prevRulesForExprs))
			in 
				changes

extractRules :: (Data t, Typeable b) => [TransState -> b -> [t]] -> TransState -> [(String, t)]
extractRules listRules state = 
	let 
		astToTransform = ast_to_transform state
		filterAst _ Nothing = 
			True 
		filterAst ast (Just (_, _, astDef)) = 
			containsAst ast astDef
		chooseAST ast Nothing = 
			ast 
		chooseAST ast (Just (_, _, astDef)) = 
			astDef
	in 
		(nubBy geq 
			(concat 
				[
					concat
					[
						[(fun_name, change) 
						| change <- (applyRulesGeneral (rule state) (chooseAST ast astToTransform))] 
					| (fun_name, _, ast) <- fun_defs state,
					  filterAst ast astToTransform,
					  case (last_changed state) of
					  	"" -> 
					  		True
					  	prev_fun
					  		| prev_fun == fun_name -> 
					  			True 
					  		| otherwise -> 
					  			False]  
				| rule <- listRules]))

getApplicableChangesWOPrevious state = 
	let 
		nstate = updateASTToTransform state
		rulesForStmts = extractRulesWOPrevious stmtRules nstate
		rulesForExprs = extractRulesWOPrevious exprRules nstate
		filterFuns = 
			case (ast_to_transform nstate) of 
				Nothing ->
					((\(_,((_,o,n),_,_)) -> not (geq o n)),
					 (\(_,((_,o,n),_,_)) -> not (geq o n)))
				(Just (polca_block, oldBlock, _)) ->
					((\(_,((_,o,n),_,_)) -> 
						let 
							newAST = changeAST (o, n) (rebuildAst state)
						in 
							case (applyRulesGeneral (search_def polca_block) newAST) of 
								[] ->
									False
								(newBlock:_) ->
									not (geq oldBlock newBlock)
					 ),
					 (\(_,((_,o,n),_,_)) -> 
						let 
							newAST = changeAST (o, n) (rebuildAst state)
						in 
							case (applyRulesGeneral (search_def polca_block) newAST) of 
								[] ->
									False
								(newBlock:_) ->
									not (geq oldBlock newBlock)
					 ))

	in 
		(filter (fst filterFuns) rulesForStmts, 
		 filter (snd filterFuns) rulesForExprs)

extractRulesWOPrevious :: (Data t) => [TransState -> t -> [((String, t, t), TransState,[(String, CStatAnn)])]] -> TransState -> [(String, ((String, t, t), TransState,[(String, CStatAnn)]))]
extractRulesWOPrevious listRules state = 
	let 
		astToTransform = 
			ast_to_transform state
		filterAst _ Nothing = 
			True 
		filterAst ast (Just (_, _, astDef)) = 
			containsAst ast astDef
		chooseAST ast Nothing = 
			ast 
		chooseAST ast (Just (_, _, astDef)) = 
			astDef
	in 
		(nubBy geq 
			(concat 
				[
					concat
					[
						[(fun_name, change) 
						| change <- (applyRulesGeneral (rule state) (chooseAST ast astToTransform) )] 
					| (fun_name, _, ast) <- fun_defs state, filterAst ast astToTransform]  
				| rule <- listRules]))


applyNrule :: TransState -> Int -> IO TransState
applyNrule state n = 
	do 
		nstate <- (applyOneRule state)
		result <- 
			case (n - 1) of 
				0 -> return nstate
				_ -> 
					if geq nstate state 
					then return nstate
					else applyNrule nstate (n - 1)
		return result


applyOneRuleCommon state applicable_rule_names listChangesStmts listChangesExprs = 
	do
		let totalChanges = 
			(length listChangesStmts) + (length listChangesExprs)
		selected <- randomRIO (0, totalChanges - 1)
		let (rule1, old1, new1, nstate1) = 
			if selected >= (length listChangesStmts) && totalChanges > 0
			then 
				let 
					(fun, ((rule, old, new), nstate0,_)) = 
						listChangesExprs!!(selected - (length listChangesStmts))
					nfun_defs = changeASTFun (fun, old, new) state
				in 
					(rule, 
					 prettyMyASTAnn old, 
					 prettyMyASTAnn new, 
					 nstate0
					 {
					 	fun_defs = nfun_defs,
					 	last_changed = fun,
					 	--previous_changes = changes,
					 	applied_rules = nub (rule: (applied_rules nstate0)),
					 	applicable_rules = Set.union (applicable_rules nstate0) (Set.fromList applicable_rule_names)
					 })
			else ("","","",state)
		let (rule2, old2, new2, nstate2) = 
			if selected < (length listChangesStmts) && totalChanges > 0
			then 
				let 
					(fun, ((rule, old, new), nstate0,_)) = 
						listChangesStmts!!selected
					(nold, nnew) = 
					 	case rule of 
							--"inlining" -> 
							--	head(concat [ 
							--		[ (oldChange, newChange)
							--		|  ((ruleChange, oldChange, newChange), _) <- (applyRulesGeneral (ruleStmt state) old), ruleChange == "inlining"] 
							--	| ruleStmt <- stmtRules])
							_ ->
								(old, new)
					nfun_defs = changeASTFun (fun, nold, nnew) state
				in (rule,
					prettyMyASTAnn nold, 
					prettyMyASTAnn nnew, 
					nstate1
					{
						fun_defs = nfun_defs,
						last_changed = fun,
						--previous_changes = changes,
						applied_rules = nub (rule:(applied_rules nstate1) ),
						applicable_rules = Set.union (applicable_rules nstate0) (Set.fromList applicable_rule_names)
					})
			else ("","","",nstate1)
		case (rule1 ++ rule2) of 
			"" -> 
				putStrLn "No more rules can be applied"
			_ -> 
				putStrLn ("Applied rule " ++ rule1 ++ rule2 ++ "\n"
					++ old1 ++ old2 ++ "\n->\n" ++ new1 ++ new2)
		return (nstate2, rule1 ++ rule2)

--applyOneRule :: TransState -> CTranslUnit -> CTranslUnit
applyOneRule state = 
	do 
		let changes@(listChangesStmts,listChangesExprs) = getApplicableChanges [] state
		let applicable_rule_names = 
			[rule | (_, ((rule, _, _), _,_)) <- listChangesStmts] 
			++ [rule | (_, ((rule, _, _), _,_)) <- listChangesExprs]
		--let listChanges2 = applyRulesGeneral applyRule ast
		--putStrLn (show listChanges)
		(nstate,_) <- applyOneRuleCommon state applicable_rule_names listChangesStmts listChangesExprs
		return nstate


--applyOneRule :: TransState -> CTranslUnit -> CTranslUnit
applyOneRuleLimited current_limit original_limit state = 
	do 
		let changes@(listChangesStmts0,listChangesExprs0) =
			getApplicableChanges [] state
		let applicable_rule_names = 
				[rule | (_, ((rule, _, _), _,_)) <- listChangesStmts0] 
			++ 	[rule | (_, ((rule, _, _), _,_)) <- listChangesExprs0]
		let not_applied_yet =
			--trace 
			--	(show $ sort (nub (applied_rules state))) 
				(nub applicable_rule_names) \\ (nub (applied_rules state))
		let (listChangesStmts, listChangesExprs, nn) = 
			case 
				--trace 
					--(show $ sort not_applied_yet) 
					not_applied_yet 
			of 
				[] ->
					(listChangesStmts0,listChangesExprs0, current_limit - 1)
				_ -> 
					(
						[change | change@(_, ((rule, _, _), _,_)) <- listChangesStmts0, elem rule not_applied_yet],
						[change | change@(_, ((rule, _, _), _,_)) <- listChangesExprs0, elem rule not_applied_yet],
						original_limit
					)
		(nstate,_) <- applyOneRuleCommon state applicable_rule_names listChangesStmts listChangesExprs
		return (nstate, nn)

applyOneRuleExploring :: TransState -> [(TransState, [String])] -> TransState -> [String] -> IO (TransState, [(TransState, [String])], TransState, [String])
applyOneRuleExploring state stack stateRep shouldBeApplied0 = 
	do 
		let changes@(listChangesStmts0,listChangesExprs0) =
			getApplicableChanges [] state
		let applicable_rule_names = 
			nub $
				[rule | (_, ((rule, _, _), _,_)) <- listChangesStmts0] 
			++ 	[rule | (_, ((rule, _, _), _,_)) <- listChangesExprs0]	
		let shouldBeApplied = 
			case 
				--trace 
				--	("\n********\nApplicable :\n" ++ (show applicable_rule_names)) 
					shouldBeApplied0 
			of 
				(_:_) ->
					--trace 
					--	("\n********\nTaken directly:\n" ++ (show shouldBeApplied0)) 
						shouldBeApplied0
				[] ->
					--trace 
					--	("\n********\nTaken from difference:\n" ++ (show (applicable_rule_names \\ (nub (applied_rules state))))) 
						applicable_rule_names \\ (nub (applied_rules state))
		let listChangesStmts = 
			[change | change@(_, ((rule, _, _), _,_)) <- listChangesStmts0, elem rule shouldBeApplied]
		let listChangesExprs = 
			[change | change@(_, ((rule, _, _), _,_)) <- listChangesExprs0, elem rule shouldBeApplied]
		(nstate0,rule) <- applyOneRuleCommon state applicable_rule_names listChangesStmts listChangesExprs
		let  (nstate, finalStack, nStateRep, nSelectableRules) = 
			case 
				--trace 
				--	("\n********\nSelected rule: " ++ rule) 
					shouldBeApplied 
			of 
				[] ->
					case stack of 
						((nstate0, nselectableRules0):finalStack0) ->
							--trace 
							--	("\n********\nRemoved from:\n" ++ (show [sr | (_,sr) <-  stack])) 
								(nstate0, finalStack0, stateRep, nselectableRules0)	
						[] ->
							--trace 
							--	("\n********\nEmpty stack") 
								(state, stack, stateRep, ["none"])
				_ -> 
					do 
						let nStateRep0 = 
							stateRep
							{
								applied_rules = 
									nub $ 		(applied_rules stateRep) ++ (applied_rules nstate0),
								applicable_rules = 
									Set.union 	(applicable_rules stateRep) (applicable_rules nstate0)
							}
						let finalStack0 = 
							case shouldBeApplied\\[rule] of 
								sth@(_:_) ->
									--trace 
									--	("\n********\nAdded to stack:\n" ++ (show sth)) 
										(state, sth):stack
								[] -> 
									--trace 
									--	("\n********\nStack unchanged.") 
										stack
						(nstate0, finalStack0, nStateRep0, [])	
		return (nstate, finalStack, nStateRep, nSelectableRules)

selectRuleToApply state = 
	do 
		let answersDict = zip ([1..((length dictRules))] ++ [0]) ([(n, [lr]) | (n, lr) <- dictRules] ++ [("all", [])])
		let question = intercalate "\n" [((show iden) ++ ".- " ++ n) | (iden, (n, _)) <- answersDict] 
		answerRuleStr <- ask ("What is the rule that should be applied next\n" ++ question)
			[(show iden) | (iden, _) <- answersDict]
		let answerRule  = read answerRuleStr::Int
		let ((selectedFun, nameRule):_) =  [(f, nameRule1) | (iden, (nameRule1,f)) <- answersDict, iden == answerRule]
		-- let selectedFun = []
		case getApplicableChanges selectedFun state of 
			([], []) ->
				do 
					putStrLn nameRule
					putStrLn "Sorry, the selected rule is not applicable."
					putStrLn "Press ENTER to continue..."
					_ <- getLine
					putStrLn ""
					selectRuleToApply state
			result ->
				return (result, True)

askNextRule state =
	do 
		answer0 <- 
			ask "Do you know what rule should be applied next"
				["y", "n"]
		case answer0 of 
			"n" ->
				return $ (getApplicableChanges [] state, False)
				-- return $ trace "executat" (getApplicableChanges [] (trace "entra" state), False)
				-- return (([], []), False)
			"y" ->
				selectRuleToApply state

applyruleInt :: TransState -> String -> Maybe [(String, Int)] -> Bool -> IO TransState
applyruleInt state filename steps recalculate = 
	do 
			-- case (ast_to_transform iniState) of 
			-- 	Nothing ->
			-- 		getApplicableChangesWOPrevious iniState
			-- 	Just astDef ->
			-- 		getApplicableChangesForGivenAst iniState astDef
		-- putStrLn "arriba1"
		(changes@(listChangesStmts,listChangesExprs), preselected) <-
			case (recalculate, steps, oracle state) of 
				(True, Nothing, "") -> 
					askNextRule state
				(True, Nothing, _) -> 
					return $ (getApplicableChanges [] state, False)
				(True, Just [], _) -> 
					return $ (getApplicableChanges [] state, False)
					-- return (([], []), False)
				(True, Just ((ruleStep, _):_), _) -> 
					return $ (getApplicableChanges [f | (rule1, f) <- dictRules, rule1 == ruleStep] state, False)
				(False, _, "") -> 
					return $ (previous_changes state, False)
		writeFileFromOption 
			"plain c (without STML annotations)" 
			filename state (\() -> return ())
		-- JSON printing
		let jsonChanges = 
			-- trace (show $ (length listChangesStmts) + (length listChangesExprs)) 
			buildJSON state changes	
		-- putStrLn "arriba2"	
		writeFile (filename ++ (buildSuffix state ".json")) jsonChanges

		let rules = nub $
				[rule | (_,((rule,_,_),_,_)) <- listChangesStmts] 
			++ 	[rule | (_,((rule,_,_),_,_)) <- listChangesExprs]
		case rules of 
			[] -> 
				do 
					putStrLn "No more rules can be applied"
					return state
			_ -> 
				case (oracle state) of 
					"" ->
						applyruleIntAux state{previous_changes = changes} preselected rules changes filename steps
					_ ->
						do 
							selected <- applyRuleWithOracle state jsonChanges
							case selected of 
								(-1) ->
									do 
										putStrLn "Oracle chose to finalize the transformation."
										return state
								(-2) ->
									do 
										putStrLn "The transformation has been stopped due to an error."
										return state
								_ ->
									do 
										let listChangesExprsToStmts = 
											[(fun, ((rule,(CExpr (Just old) undefNodeAnn),(CExpr (Just new) undefNodeAnn)),nstate, unknownProps)) 
											 | (fun, ((rule,old,new),nstate, unknownProps)) <- listChangesExprs]
										let (_, ((rule,_,_),_,_))  = (listChangesExprsToStmts ++ listChangesStmts)!!selected
										applyruleIntAux state{previous_changes = changes} preselected rules changes filename (Just [(rule, selected)])


applyRuleWithOracle state jsonChanges = 
	do 
		let cmd = 
			(oracle state) ++ " \"" ++ (intoString jsonChanges) ++ "\" > oracle_choice.txt"
		-- putStrLn cmd
		checkResult <- system cmd
		case checkResult of 
			ExitSuccess ->
				do 
					contents <- readFile "oracle_choice.txt"
					let mBAnswer = 
						(TR.readMaybe contents)::(Maybe Int)
					case mBAnswer of 
						Nothing -> 
							do 
								putStrLn "Error while using the external oracle."
								return (-1)
						(Just v) ->
							return v
			_ ->
				do 
					putStrLn "Error while using the external oracle."
					return (-2)


intoString ('\\':'\"':rest)= 
	('\\':'\\':'\\':'\"':(intoString rest))
intoString ('\"':rest) = 
	('\\':'\"':(intoString rest))
intoString (other:rest) = 
	(other:(intoString rest))
intoString [] = 
	[]

applyruleIntAux state preselected rules changes filename steps =
	do 
		(answer,dict) <-
			case steps of 
				Nothing -> 
					do 
						let (question, answers, dict0) = prepareQuestionAnswerRule rules 1
						case (length answers) of
							1 ->
								return ((head answers), dict0)
							_ ->
								do 
									answer0 <- 
										ask ("What rule do you want to apply\n" ++ question) 
											(answers ++ ["h","r","s","c","ec","dc","p","f"])
									return (answer0, dict0)
				(Just []) ->
					return ("f", [])
				(Just _) ->
					return ("other_answer", [("","")])
		case answer of 
			"h" ->
			    do 
			    	showHelpGeneral
			    	applyruleInt state filename steps False
			"f" ->
				return state
			"r" ->
				do 
					let optionsSteps = ["1","5","10","25","50","75","100","150","200"]
					let (questionSteps, answersSteps,dictSteps) = prepareQuestionAnswerRule optionsSteps 1
					answerSteps <- ask ("How many steps do you want to perform randomly\n" ++ questionSteps) answersSteps
					nstate <- applyNrule state (head [ (read steps::Int) | (answer_,steps) <- dictSteps, answer_ == answerSteps])
					(applyruleInt nstate filename steps True)
			"p" ->
				do 
					--writeFile (filename ++ "_transformed.c") (prettyMyASTIncludes ast)
					-- putStrLn (show (print_id state))
					askWriteOptionsAndReturn filename state (\() -> (applyruleInt state filename steps False))
					-- writeFileWithPragmas (filename ++ "_transformed.c") state
					-- putStrLn ("Transformed code stored in " ++ filename ++ "_transformed.c")
					-- (applyruleInt state filename steps False)
			"s" ->
				do 
					--putStrLn (prettyMyASTIncludes ast)
					putStrLn  (printWithPragmas state)
					(applyruleInt state filename steps False)
			"c" ->
				do 
					--putStrLn (prettyMyASTIncludes ast)
					putStrLn  (printWithoutPragmas state)
					(applyruleInt state filename steps False)
			"ec" ->
				do 
					--putStrLn (prettyMyASTIncludes ast)
					putStrLn  "From now on transformation steps are shown without annotations"
					(applyruleInt state{trans_with_anns = False} filename steps False)
			"dc" ->
				do 
					--putStrLn (prettyMyASTIncludes ast)
					putStrLn  "From now on transformation steps are shown with annotations"
					(applyruleInt state{trans_with_anns = True} filename steps False)
			_ ->
				applyruleIntAux2 state preselected changes (head [rule | (answer_,rule) <- dict, answer_ == answer]) filename steps

applyruleIntAux2 state0 preselected changes@(listChangesStmts,listChangesExprs) selectedRule filename steps =
	do 
		let listChangesExprsToStmts = 
			[(True, (fun, ((rule,(CExpr (Just old) undefNodeAnn),(CExpr (Just new) undefNodeAnn)),nstate, unknownProps))) 
			 | (fun, ((rule,old,new),nstate, unknownProps)) <- listChangesExprs]
		let listChangesStmtsToStmts = 
			[(False, item) 
			 | item@(_,((rule,_,_),_,_)) <- listChangesStmts]
		let wholeList = 
			listChangesExprsToStmts ++ listChangesStmtsToStmts
		case steps of 
			Nothing ->
				do 
					let selectedRules = 
						selectRules wholeList selectedRule 0 0
					(continue, hasChanged, state1) <- 
						applyChangesStmt 
							selectedRules  
							state0
							Nothing
							preselected
					let state2 = 
						state1
						--{
						--	previous_changes = changes
						--}
					if continue
					then applyruleInt state2 filename steps hasChanged
					else return state2
			(Just (item@(_,indice):nsteps)) ->
				do 
					let (expr,(fun, ((rule,old,new),nstate, _)))  = wholeList!!indice
					let (nold, nnew) = 
						case rule of 
							--"inlining" -> 
							--	head(concat [ 
							--		[ (oldChange, newChange)
							--		|  ((ruleChange, oldChange, newChange), _) <- (applyRulesGeneral (ruleStmt state0) old), ruleChange == "inlining"] 
							--	| ruleStmt <- stmtRules])
							_ ->
								(old, new)
					let (oldStr, newStr) = 
						case (trans_with_anns state0) of 
							True ->
								(printChange nold state0, printChange nnew nstate)
							False ->
								(printChangeWithoutAnns nold, printChangeWithoutAnns nnew)
					putStrLn "\n*********************************************\n"
					putStrLn ("Applied rule " ++ rule ++ " to this piece of code:\n\n" 
						++ oldStr ++ "\n\nresulting in this new code:\n\n" 
						++ newStr ++ "\n")	
					let nfun_defs = 
						case expr of 
							False -> 
								changeASTFun (fun,nold,nnew) state0
							True -> 
								changeASTFun (fun, removeStmtForExpr old,removeStmtForExpr new) state0
					applyruleInt 
						(nstate
						{
							--current_ast = nast, 
							--fun_defs = nfun_defs,
							--last_changed = fun,
							--previous_changes = changes
							trans_with_anns = (trans_with_anns state0),
				 			fun_defs = nfun_defs,
				 			last_changed = fun,
				 			print_id = (print_id nstate) + 1,
				 			applied_rules = (rule:(applied_rules nstate)),
				 			acc_steps = (item:(acc_steps nstate))
						}) 
						filename (Just nsteps) True

selectRules (item@(_, (_,((rule,_,_),_,_))):tail_) selectedRule current currentRule
	| rule == selectedRule = 
		((item,current, currentRule):(selectRules tail_ selectedRule (current + 1) (currentRule + 1) ))
	| otherwise = 
		selectRules tail_ selectedRule (current + 1) currentRule 
selectRules [] _ _ _ = 
	[]


askWriteOptionsAndReturn filename state finalCall = 
	do 
		let op1 = 
			"plain c (with all anotations)"
		let op2 = 
			"plain c (without STML annotations)"
		let op3 = 
			"plain c (without any annotation)"
		let options = [ op1, op2, op3]
		let (question, answers, dict) = prepareQuestionAnswerRule options 1
		answer <- 
			ask ("Select an output format\n" ++ question) answers
		writeFileFromOption 
			(head [option | (answer_,option) <- dict, answer_ == answer]) 
			filename state finalCall

writeFileFromOption option filename state finalCall = 
	do
		let header0 = 
			case (applied_rules state) of 
				[] ->
					""
				(last_applied:_) ->
					("// RULE_APPLD: " ++ last_applied ++ "\n") 
		let header = 
			header0 ++ "// FUNC_ANALYZ: main BLOCK_ABS\n"	
		-- let suffix = 
		-- 	buildSuffix state ".c"
		case option of
			"plain c (with all anotations)" ->
				writeTransFileInt filename state header (buildSuffix state "_all_anns.c")
			"plain c (without STML annotations)" ->
				writeTransFileOnlyPolcaInt filename state header (buildSuffix state ".c")
			"plain c (without any annotation)" ->
				writeTransFileWithoutAnnsInt filename state header (buildSuffix state "_no_anns.c")
		finalCall ()

buildSuffix state ext = 
	let suffix_seq = 
			case (seq_id state) of 
				-1 ->
					""
				sid ->
					('_':(show3 sid))
	in
		suffix_seq ++ ('_':(show3 (print_id state))) ++ ext

show3 n = 
	let 
		strn = show n 
	in
		case length strn of 
			3 ->
				strn
			2 -> 
				('0':strn)
			1 -> 
				('0':('0':strn))
			0 -> 
				"000"

prepareQuestionAnswerRule [] _ = 
	("",[],[])
prepareQuestionAnswerRule (rule:rules) n =
		((show n) ++".- " ++ rule ++ "\n" ++ question,(show n):answers,((show n),rule):dict)
	where
		(question,answers,dict) = prepareQuestionAnswerRule rules (n+1)



applyChangesStmt [] state0 _ preselected = 
	return (True, preselected, state0)
applyChangesStmt whole_list@(((isExpr, (fun, ((rule,old,new),nstate,unknownProps))), index, indexRule):tail_)  state0 answer0 preselected =
	do
		let (nold, nnew) = 
			case rule of 
				--"inlining" -> 
				--	head(concat [ 
				--		[ (oldChange, newChange)
				--		|  ((ruleChange, oldChange, newChange), _) <- (applyRulesGeneral (ruleStmt state0) old), ruleChange == "inlining"] 
				--	| ruleStmt <- stmtRules])
				_ ->
					(old, new)
		let (oldStr, newStr) = 
			case (trans_with_anns state0) of 
				True ->
					(printChange nold state0, printChange nnew nstate)
				False ->
					(printChangeWithoutAnns nold, printChangeWithoutAnns nnew)
		--let oldStr = printChange nold state0
		--let newStr = printChange nnew nstate
		let options = 
			["y","n","a","c","d","v","s","h","f"]
		let pendingInfo = 
			case unknownProps of 
				[] ->
					""
				_ ->
					foldl 
						(\acc (up,uv) -> acc ++ "\nProperty " ++ up ++ " for:\n" ++ (prettyMyASTAnn uv))
						"\nPending properties:"
						unknownProps
		--let questionWithAnns = 
		--	"Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
		--		++ oldStr ++ "\n\nresulting in this new code:\n\n" 
		--		++ newStr ++ "\n" ++ pendingInfo
		--let questionWithoutAnns = 
		--	"Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
		--		++ (printChangeWithoutAnns nold) ++ "\n\nresulting in this new code:\n\n" 
		--		++ (printChangeWithoutAnns nnew) ++ "\n"
		let question = 
			"Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
				++ oldStr ++ "\n\nresulting in this new code:\n\n" 
				++ newStr ++ "\n" ++ pendingInfo
		answer <- 
			case answer0 of 
				Nothing ->
					ask question options
				(Just answer0_) ->
					answer0_
		case answer of 
			"h" -> 
				do
					showHelpApplyChange
					applyChangesStmt whole_list state0 Nothing preselected
			"y" -> 
				do
					let nfun_defs = 
						case isExpr of 
							False -> 
								changeASTFun (fun,nold,nnew) state0
							True -> 
								changeASTFun (fun,removeStmtForExpr old,removeStmtForExpr new) state0
					-- TODO: Could be used to perform undo operation
					-- putStrLn (show index)
					--putStrLn (show (previous_changes state0))
					-- let new_ast_to_transform = 
					-- 	case (ast_to_transform nstate) of 
					-- 		Nothing ->
					-- 			Nothing 
					-- 		(Just (polca_block,_)) ->
					-- 			case (applyRulesGeneral (search_def polca_block) (rebuildAst nstate)) of 
					-- 				[] ->
					-- 					Nothing
					-- 				(first:_) ->
					-- 					(Just (polca_block, first))
				 	return (True, True, 
				 		nstate
				 		{
				 			--current_ast = nast,
				 			trans_with_anns = (trans_with_anns state0),
				 			applied_rules = (rule:(applied_rules nstate)),
				 			fun_defs = nfun_defs,
				 			last_changed = fun,
				 			print_id = (print_id nstate) + 1,
				 			acc_steps = ((rule, indexRule):(acc_steps nstate))
				 			-- ast_to_transform = 
				 			-- 	-- trace (show new_ast_to_transform) 
				 			-- 	new_ast_to_transform
				 		})
			"n" ->
				applyChangesStmt tail_  state0 Nothing preselected
			"c" ->
				do 
					let questionWithoutAnns = 
						"Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
							++ (printChangeWithoutAnns nold) ++ "\n\nresulting in this new code:\n\n" 
							++ (printChangeWithoutAnns nnew) ++ "\n" ++ pendingInfo
					let nanswer = ask questionWithoutAnns options
					applyChangesStmt whole_list state0 (Just nanswer) preselected
			"a" ->
				do 
					let questionWithAnns = 
						"Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
							++ (printChange nold state0) ++ "\n\nresulting in this new code:\n\n" 
							++ (printChange nnew nstate) ++ "\n" ++ pendingInfo
					let nanswer = ask questionWithAnns options
					applyChangesStmt whole_list state0 (Just nanswer) preselected
			"d" ->
				do 
					let linesOldStr = lines oldStr
					let linesNewStr = lines newStr
					let diffResult = getGroupedDiff linesOldStr linesNewStr
					let diffOutput = ppDiff diffResult
					let nanswer =
						ask ("Do you want to apply rule " ++ rule ++ " resulting in the following changes:\n\n" 
						++ diffOutput ++ "\n") ["y","n","c","d","v", "s","h","f"]
					applyChangesStmt whole_list  state0 (Just nanswer) preselected
			"v" -> 
				do 
					writeFile "meld_old" oldStr
					writeFile "meld_new" newStr
					spawnCommand "meld meld_old meld_new"
					applyChangesStmt whole_list state0 Nothing preselected
			"s" ->
				return (True, preselected, state0)
			"f" -> 
				return (False, False, state0)

removeStmtForExpr (CExpr (Just e) undefNode) = 
	e


printChange (CCompound _ block _) state = 
	concat (map (++ "\n") (map printWithPragmasBlockExt block))
printChange (CExpr (Just e) undefNode) _ = 
	prettyMyASTAnn e

printChangeWithoutAnns (CCompound _ block _) = 
	concat (map (++"\n") (map (\item -> prettyMyASTAnn item) block))
printChangeWithoutAnns (CExpr (Just e) undefNode) = 
	prettyMyASTAnn e

 
showHelpGeneral =
 	do 
		let helpStr = 
			[
			 "-----------------------------------------------------------------",
			 "Polca S2S Tool help.",
			 "-----------------------------------------------------------------",
			 "Select one numerical option to apply the corresponding rule.",
			 "The rest of available options are:",
			 "h -> This help",
			 "r -> Perform a number of random transformation steps",
			 "p -> Print current code in a file",
			 "s -> Show current code",
			 "c -> Show current code without annotations",
			 "ec -> Show transformation steps without annotations",
			 "dc -> Show transformation steps with annotations (default option)",
			 "f -> Finalise transformation session",
			 "-----------------------------------------------------------------"
			]
		putStrLn (unlines helpStr)
		putStrLn "Press ENTER to continue..."
		_ <- getLine
		putStrLn ""

showHelpApplyChange =
	do 
		let helpStr = 
			[
			 "---------------------------------------------------------------",
			 "Polca S2S Tool help.",
			 "---------------------------------------------------------------",
			 "Available options are:",
			 "y -> Apply the transformation step",
			 "n -> Omit the transformation step",
			 "a -> Show with annotations",
			 "c -> Show without annotations",
			 --"yc -> Apply the transformation step and continue applying rule",
			 --"a  -> Apply all the transformation steps for this rule",
			 "d -> Show only the differences",
			 "v -> Open Meld to see the differences",
			 "s -> Stop rule application",
			 "h -> This help",
			 "f -> Finalise transformation session",
			 "---------------------------------------------------------------"
			]
		putStrLn (unlines helpStr)
		putStrLn "Press ENTER to continue..."
		_ <- getLine
		putStrLn ""

ask :: String -> [String] -> IO String 
ask question answers = 
	do
		hSetBuffering stdout NoBuffering
		putStrLn question 
		putStr ("? [" ++ (init (concat ([ q ++ "/" | q <- answers ]))) ++ "]: ")
		answer <- getLine
		if elem answer answers
		then return answer
		else ask question answers

readSteps name = 
	do
		content <- CE.try (readFile (name ++ ".stp")):: IO (Either SomeException [Char])
		case content of 
			Left ex -> 
				return [] 
			Right val -> 
				do 
					return [read stepStr::(String, Int) | stepStr <- (lines val)]

elemIndexChanges p1 [] n
	= -1
elemIndexChanges p1 (p2:tail_) n 
	| geq p1 p2 = n
	| otherwise =  elemIndexChanges p1 tail_ (n + 1)


---------------------------------------------------------
-- SEARCH
---------------------------------------------------------

--searchPy name =
--	do 
--		(ast1, linkedPolcaAnn, includes) <- readFileInfo name
--		let lastNode = getLastNode ast1
--		let initialState  = 
--			TransState 
--			{
--				free_node_id = lastNode + 1, 
--				pragmas = linkedPolcaAnn,
--				freeVar = 0,
--				includes = includes,
--				fun_defs = applyRulesGeneral searchFunDefsGeneral ast1,
--				no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral ast1,
--				last_changed = "",
--				previous_changes = ([],[])
--			}
--		nstate <-
--			getFinalCode searchStep is_goal [initialState]
--		writeTransFile name nstate

--getFinalCode:: (TransState -> [TransState]) -> (TransState -> IO Bool) -> [TransState] -> IO TransState
--getFinalCode trans_fun goal_fun states = 
--	do 
--		let n_states = concat [trans_fun state | state <- states] 
--		--let final_info = [ (state, goal_fun state ) | state <- n_states]
--		final_info <- get_final_info_states goal_fun n_states
--		case [state | (state, True) <- final_info] of
--			[] -> 
--				getFinalCode trans_fun goal_fun n_states
--			(state:_) -> 
--				return state 

--get_final_info_states::(TransState -> IO Bool) -> [TransState] -> IO [(TransState, Bool)]
--get_final_info_states goal_fun (state:states) = 
--	do 
--		info_state <- get_final_info goal_fun state 
--		info_states <- get_final_info_states goal_fun states
--		return (info_state:info_states)
--get_final_info_states _ [] = 
--	return []

--get_final_info goal_fun state = 
--	do 
--		is_goal <- goal_fun state
--		return (state,is_goal) 


--is_goal::TransState -> IO Bool
--is_goal state = 
--	do 
--		let fileContent = printWithPragmas state
--		--classification <- readProcess "./fake_class"  [fileContent] ""
--		result@(_,classification,_) <- readProcessWithExitCode "./fake_class"  [fileContent] ""
--		putStrLn ("result: " ++ (show result) ) 
--		return (classification == "99")

----searchBest name = 
----	do 
----		(ast1, linkedPolcaAnn, includes) <- readFileInfo name
----		let lastNode = getLastNode ast1
----		----print lastNode 
----		let initialState  = 
----			TransState 
----			{
----				free_node_id = lastNode + 1, 
----				pragmas = linkedPolcaAnn,
----				-- TODO: remove asts from the process since ast is now in the state
----				--current_ast = ast1,
----				freeVar = 0,
----				includes = includes,
----				fun_defs = applyRulesGeneral searchFunDefsGeneral ast1,
----				no_fun_defs = applyRulesGeneral searchNoFunDefsGeneral ast1,
----				last_changed = "",
----				previous_changes = ([],[])
----			}
----		let nstate = 
----			aStar searchStep searchDistance searchHeuristicDistance searchGoal initialState
----		writeTransFile name (last (fromJust nstate))

----searchDistance state1 state2 = 
----	--(freeVar state2) - (freeVar state1)
----	1

----searchHeuristicDistance _ = 
----	1

----searchGoal state =
----	(trace ("Checks if it is goal: " ++ (show (freeVar state))) (freeVar state))  > 0

--searchStep::TransState -> [TransState]
--searchStep state = 
--	let 
--		(listChangesStmts,listChangesExprs) = getApplicableChanges state
--		listChangesExprsToStmts = 
--			[(True, (fun, ((rule,(CExpr (Just old) undefNode),(CExpr (Just new) undefNode)),nstate))) 
--			 | (fun, ((rule,old,new),nstate)) <- listChangesExprs]
--		listChangesStmtsToStmts = 
--			[(False, item) 
--			 | item@(_,((rule,_,_),_)) <- listChangesStmts]
--		changes = listChangesStmtsToStmts ++ listChangesExprsToStmts
--		statesFromState = map (applyChange state) changes
--	in 
--		--fromList statesFromState
--		statesFromState

--applyChange state0 (isExpr, (fun, ((rule,old,new),nstate))) = 
--	let 
--		(nold, nnew) = 
--			case rule of 
--				"inlining" -> 
--					head(concat [ 
--						[ (oldChange, newChange)
--						|  ((ruleChange, oldChange, newChange), _) <- (applyRulesGeneral (ruleStmt state0) old), ruleChange == "inlining"] 
--					| ruleStmt <- stmtRules])
--				_ ->
--					(old, new)
--		nfun_defs = 
--			case isExpr of 
--				False -> 
--					changeASTFun (fun,nold,nnew) state0
--				True -> 
--					changeASTFun (fun,removeStmtForExpr old,removeStmtForExpr new) state0
--	in 
-- 		nstate
-- 		{
-- 			fun_defs = nfun_defs,
-- 			last_changed = fun
-- 		}





--start_population :: (RandomGen g) => g -> [[Double]]
--start_population gen =
--  [take 50 $ randomRs (0.1, 2) gen | x <- [0..]]
 
--getRandomValue :: Double
--getRandomValue = do
--  head $ head $ take 1 $ start_population (unsafePerformIO newStdGen)

--getRandomValue =
--	unsafePerformIO (getStdRandom (randomR (0.1, 2)) :: IO Double )
--	unsafePerformIO (head (randomRs (0.1, 2) newStdGen))
--	unsafePerformIO (randomRIO (0.1, 2) :: IO Double)

---------------------------------------------------------
-- TODOS
---------------------------------------------------------

--DONE
-- Allow to change from interactive to random mode for a number of steps (or until no more clauses can be applied)
-- 02/06/2015
-- Rules for expressions can be expressed as statements in the list of changes and undo before apply changes.
--		This would remove the need to treat them separatedly.
-- 03/06/2015
-- Recalculate applicable changes for only the body (or bodies using option all or yes&continue) of the function that has changed in last step. (applicableChangesFun) 
-- 		This obligates to carry the function where it has been applied, and previous applicable changes
--		and mix º new changes and previous chenges according to this function.
-- pragma kernel -> pragma adapt

--TODO

-- 02/06/2015
-- Apply the uniformation of statetements and expression to random transformation code 

-- 26/05/2015
-- Undo operation should be a stack with [(ast, changes)]. In this way no recalculation is needed when going back.
-- 		An alternative approach for undo would be to store only the initial ast, and the list of changes [(old,new)], 
-- 		and apply these changes each iteration. In this way undo operation is just to remove the head of the changes stack.
--		This stack can contain other informartion neede to improve aplicable rule calcule.

-- 30/10/2015
-- When a rule application is discarded (say no to its applications), the next time is tried to be applied it is duplicated, tripiclated, ans so on...
-- Annotate AST 
-- Improve STML syntax in order to avoid repeat the type of each meta-variable
-- Gruop/Label rules so user could choose easyly which rules should the system use
-- Prepare transformation system to work in phases. This phases could be fix point or till fullfill some condition
-- Read rules from more than one file
-- Decide waht are the applicable rules according to a decision tree build using node properties


---------------------------------------------------------
-- DOCUMENTATION
---------------------------------------------------------

-- See other uses of Language.C
--http://www.tiresiaspress.us/haskell/language-objc/examples/ComputeSize.hs
--http://zwizwa.be/darcs/meta/clua/
--http://stackoverflow.com/questions/27214605/count-c-instructions
--http://www.cse.unsw.edu.au/~keller/Papers/acc-cuda.pdf
--http://hsbene.blogspot.com.es/2008/07/languagec-analysing-definitions.html
--https://github.com/haskell/c2hs
--http://spin.atomicobject.com/2013/03/05/why-haskell/
--https://github.com/vesalvojdani/haskell-fm/blob/master/ParseC.hs
-- ANALYSYS
--http://hsbene.blogspot.jp/search/label/c%20haskell%20analysis%20Language.C
--http://nizumical.tumblr.com/post/29899418378/%E5%A4%8F%E3%81%AEhaskell%E5%90%88%E5%AE%BF2%E6%97%A5%E7%9B%AE
--http://www.codegithub.com/watch?v=0zSSzqxWjPQX
-- ANALYSYS (Very detailed)
--http://zwizwa.be/-/meta


---------------------------------------------------------
-- OLD CODE
---------------------------------------------------------

--data PolcaAnns = PolcaAnns  [(Name,[String])] deriving (Show, Read)
--type CExprStmt = ExprStmt NodeInfo
--data ExprStmt a = 
--	CStatement a | 
--	CExpression a

--(globals,warnings) <- (runTrav_ >>> checkResult "[analysis]") $ analyseAST ast0
--mapM (hPutStrLn stderr . show) warnings

--checkResult label = either (error . (label++) . show) return

--generateSizeTests :: FilePath -> GlobalDecls -> CTranslUnit
--generateSizeTests c_file globals =
--       translUnit $
--          map defineComp referenced_comps ++
--          [ genSizeTest (Map.elems comps_of_interest) ]
--     where
--	     comps = Map.mapMaybe fromComp (gTags globals)
--	     comps_of_interest  = compsOfInterest c_file comps
--	     referenced_comps   = computeRefClosure comps comps_of_interest
--	     fromComp (CompTag struct_union) = Just struct_union
--	     fromComp (EnumTag _) = Nothing
--	     translUnit = flip CTranslUnit intn


--intn = mkUndefNodeInfo

--separateNodesLib filename ast = 
--	let
--		(CTranslUnit decl nI) = ast
--		(extDelcs,intDecl) = separateDeclaration filename decl
--	in
--		((CTranslUnit intDecl nI),extDelcs)

--separateDeclaration filename ((decl@(CDeclExt (CDecl _ _ nI))):t) = 
--	separateDeclarationGen filename decl nI t
--separateDeclaration filename ((decl@(CFDefExt (CFunDef _ _ _ _ nI))):t) = 
--	separateDeclarationGen filename decl nI t
----separateDeclaration filename (other:t) = 
----	let 
----		(extDelcs,intDecl) = (separateDeclaration filename t)
----	in 
----		trace (show other) (extDelcs,other:intDecl)
--separateDeclaration _ [] = 
--	([],[])
 
--separateDeclarationGen filename decl nI t = 
--	let 
--		(extDelcs,intDecl) = (separateDeclaration filename t)
--	in 
--		case fileOfNode nI of 
--			Nothing -> 
--				(decl:extDelcs,intDecl)
--			Just filenameNI -> 
--				if filenameNI == filename 
--				then
--					(extDelcs,decl:intDecl)
--				else 
--					(decl:extDelcs,intDecl)

--addExternalDefs ast2_ externalDefs = 
--	let 
--		(CTranslUnit decl nI) = ast2_
--	in 
--		(CTranslUnit (externalDefs++decl) nI) 


		--answer <-
		--	ask ("Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
		--		++ oldStr ++ "\n\nresulting in this new code:\n\n" 
		--		++ newStr ++ "\n") ["y","n","f"]
		--case answer of 
		--	"y" -> 
		--		do
		--			let nast = (changeAST (old,new) ast)
		--			-- TODO: Could be used to perform undo operation
		--			--putStrLn (show (expr, (elemIndexChanges item list 0)) )
		--		 	return (True,nast , nstate{current_ast = nast})
		--	"n" ->
		--		--let (_,nast) = (applyChanges tail_ ast)
		--		--in (True, nast)
		--		--(False, ast)
		--		applyChanges tail_ ast expr list state0
		--	"f" -> 
		--		return (False, ast, state0)

--applyChanges [] ast _ _ state0 = 
--	return (True, ast, state0)
--applyChangesGeneral (oldStr, newStr) (item@((rule,old,new),nstate):tail_) ast  list state0 =
--	do
--		--let (oldStr, newStr) = 
--		--	if(expr)
--		--	then ((prettyMyAST old), (prettyMyAST new))
--		--	else 
--		--		(
--		--			printWithPragmasBlock (pragmas state0) (CBlockStmt old),
--		--			printWithPragmasBlock (pragmas nstate) (CBlockStmt new)
--		--		)
--		answer <-
--			ask ("Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
--				++ oldStr ++ "\n\nresulting in this new code:\n\n" 
--				++ newStr ++ "\n") ["y","n","f"]
--		case answer of 
--			"y" -> 
--				do
--					let nast = (changeAST (old,new) ast)
--					-- TODO: Could be used to perform undo operation
--					--putStrLn (show (expr, (elemIndexChanges item list 0)) )
--				 	return (True,nast , nstate{current_ast = nast})
--			"n" ->
--				--let (_,nast) = (applyChanges tail_ ast)
--				--in (True, nast)
--				--(False, ast)
--				applyChanges tail_ ast  list state0
--			"f" -> 
--				return (False, ast, state0)
				
		--return (processAnswer answer tail_)
	--where 
	--	processAnswer answer tail_ = 
	--		case answer of 
	--			"y" -> 
	--				(True, (changeAST (old,new) ast))
	--			"n" ->
	--				let (_,nast) = (applyChanges tail_ ast)
	--				in (True, nast)
	--			"f" -> 
	--				(False, ast)

--applyChangesExpr [] ast _ state0 = 
--	return (True, ast, state0)
--applyChangesExpr (item@((rule,old,new),nstate):tail_) ast  list state0 =
--	do
--		let (oldStr, newStr) = ((prettyMyAST old), (prettyMyAST new))
--		answer <-
--			ask ("Do you want to apply rule " ++ rule ++ " to this piece of code:\n\n" 
--				++ oldStr ++ "\n\nresulting in this new code:\n\n" 
--				++ newStr ++ "\n") ["y","n","s","h","f"]
--		case answer of 
--			"h" -> 
--				do
--					showHelpApplyChange
--					applyChangesExpr (item:tail_) ast list state0
--			"y" -> 
--				do
--					let nast = (changeAST (old,new) ast)
--					-- TODO: Could be used to perform undo operation
--					putStrLn (show (True, (elemIndexChanges item list 0)) )
--				 	return (True, nast, 
--				 		nstate
--				 		{
--				 			current_ast = nast, 
--				 			fun_defs = applyRulesGeneral searchFunDefsGeneral nast
--				 		})
--			--"yc" -> 
--			--	do
--			--		let nast = (changeAST (old,new) ast)
--			--		-- TODO: Needs to fuse nstate and state0 (otherwise accumulated info in state0 would be lost)
--			--	 	applyChangesExpr tail_ nast list 
--			--	 		nstate
--			--	 		{
--			--	 			current_ast = nast,
--			--	 			fun_defs = applyRulesGeneral searchFunDefsGeneral nast
--			--	 		}
--			"n" ->
--				--let (_,nast) = (applyChanges tail_ ast)
--				--in (True, nast)
--				--(False, ast)
--				applyChangesExpr tail_ ast list state0
--			"s" ->
--				return (True, ast, state0)
--			"f" -> 
--				return (False, ast, state0)


--applyGivenTransStep (ast,state0) item@((_,old,new),nstate) = 
--	let 
--		nast = (changeAST (old,new) ast)
--	-- TODO: Needs to fuse nstate and state0 (otherwise accumulated info in state0 would be lost)
--	in (nast, 
--		state0
--		{
--			current_ast = nast, 
--			fun_defs = applyRulesGeneral searchFunDefsGeneral nast
--		})

			--"yc" -> 
			--	do
			--		let nast = (changeAST (old,new) ast)
			--		-- TODO: Needs to fuse nstate and state0 (otherwise accumulated info in state0 would be lost)
			--	 	applyChangesStmt tail_ nast list 
			--	 		nstate
			--	 		{
			--	 			current_ast = nast,
			--	 			fun_defs = applyRulesGeneral searchFunDefsGeneral nast
			--	 		}
			--"a" -> 
			--	do
			--		let allRulesExprs = 
			--			[item | item@((ruleL,_,_),_) <- listChangesExprs, ruleL == rule]
			--		let (nast, nstate1) =
			--			foldl 

		--let ast3 = applyRulesExpr ast2
		--printMyAST ast2
		--let ast2 = addExternalDefs ast2_ externalDefs
		--writeFile (name ++ "_transformed.c") (prettyMyASTIncludes ast2)
		--writeFileWithPragmas (name ++ "_transformed.c") includes linkedPragmas ast
		--putStrLn ("Transformed code stored in " ++ name ++ "_transformed.c")
		--linesFile <- readLinesFile (name ++ "_transformed.c")
		--let newLinkedPolcaAnn = getPolcaAnn 1
		----print newLinkedPolcaAnn
		--let codeAnn = getAllNodes newLinkedPolcaAnn ast2
		----print (head linesFile)
		----print codeAnn
		--let newFileContent = writePragmas linesFile (reverse codeAnn) []
		----print newFileContent
		----putStrLn (unlines newFileContent)
		--writeFile (name ++ "_transformed.c") (unlines newFileContent)
		----print (getPolcaAnn 2)
		--writeFile (name ++ ".ast2") (Gr.groom ast2)
		----putStrLn "*************************************"
		----putStrLn "Transformed code second iteration:"
		----ast4 <- return (applyRules ast2)
		----printMyAST ast2

		--let result = geq (CVar (Ident "res" 1897203 undefNode) undefNode)  (CVar (Ident "res" 1897202 undefNode) undefNode)
		--print (empty Bool)
		--let filename = name ++ ".c"
		--polcaAnn' <- parsePolcaAnn filename
		--ast0 <- parseMyFile filename
		--let (errors,polcaAnn) = (errorsNclean polcaAnn') 
		----let polcaAnn = 
		----	case polcaAnnCleaned of 
		----		[] -> []
		----		(x:_) -> x
		----print [i | i@(n,_) <- polcaAnn, n /= -1]
		----let linkedPolcaAnn = linkPolcaAnn polcaAnn ast
		--let linkedPolcaAnn = linkPolcaAnn ast0 polcaAnn
		----print linkedPolcaAnn
		----putStrLn "Parsed results"
		--if errors /= [] 
		--then error (unlines errors)
		--else putStrLn ("Pragmas polca successfully read from " ++ filename)
		--putStrLn ("AST successfully read from " ++ filename ++ " and stored in " ++ name ++ ".ast")
		----putStrLn "AST:"
		----putStrLn $ Gr.groom ast 
		--writeFile (name ++ ".ast") (Gr.groom ast0)
		----putStrLn ("Pragmas polca successfully read from " ++ (show (pretty globals)))
		----putStrLn "*************************************"
		----putStrLn "Original code:"
		----printMyAST ast
		----putStrLn "*************************************"
		----putStrLn "Transformed code:"
		----writeFile "state" ("0\n"++ (show (incName lastNode)) ++  "\n" ++ (show linkedPolcaAnn) ++ "\n")
		----ast2 <- applyOneRule ast
		--let (ast1,externalDefs) = separateNodesLib filename ast0


					--(boolS,nastS, state1) <- applyChangesStmt selectedRulesStmts ast  listChangesStmts state0
					--(boolE,nast, state2) <- applyChangesExpr selectedRulesExprs nastS  listChangesExprs state1
					--if boolS && boolE 
					--then applyruleInt state2 nast filename steps
					--else return (nast,state2)
				--do
				--	let ((rule,old,new),_)  =
						--if expr
						--then 

								--else 
						--	do 
						--		let ((rule,old,new),nstate)  = listChangesStmts!!item
						--		putStrLn "\n*********************************************\n"
						--		putStrLn ("Applied rule " ++ rule ++ " to this piece of code:\n\n" 
						--			++ (prettyMyAST old) ++ "\n\nresulting in this new code:\n\n" 
						--			++ (prettyMyAST new) ++ "\n")
						--		let nast = (changeAST (old,new) ast)	
						--		applyruleInt (
						--			nstate
						--			{
						--				current_ast = nast, 
						--				fun_defs = applyRulesGeneral searchFunDefsGeneral nast
						--			}) 
						--			nast filename (Just nsteps)
					--putStrLn "Applied rule " ++ rule ++ " to this piece of code:\n\n" 
					--	++ (prettyMyAST old) ++ "\n\nresulting in this new code:\n\n" 
					--	++ (prettyMyAST new) ++ "\n"	
					--applyruleInt state (changeAST (old,new) ast) filename (Just nsteps)
					--applyruleInt state ast filename (Just nsteps)

--main =
--	trans_rand_def 10

--trans_to_opencl name = 
--	do 
--		(ast, linkedPolcaAnn, includes) <- getTrans name "int" 0
--		trans_platform_internal name "opencl" includes ast linkedPolcaAnn 



--sem_info name = 
--	do
--		ast <- parseMyFile name
--  		--(globals,warnings) <- (checkResult "[Analysis]" . runTrav_) (analyseAST ast)
--  		(globals,warnings) <- (checkResult "" . runTrav_)  (analyseAST ast)
--  		let Right (result,_) = runTrav_  (analyseAST ast)
--  		putStrLn (Gr.groom result)
--  		--print . pretty $ globals
--		--putStrLn (Gr.groom globals)

--checkResult :: (Show a) => String -> (Either a b) -> IO b
--checkResult label = either (error . (label++) . show) return

--analysis program = do 
--  ast <- parseMyFile program
--  (globals,warnings) <- (checkResult "[Analysis]" . runTrav_) (analyseAST ast)
--  print . pretty $ globals

--checkResult :: (Show a) => String -> (Either a b) -> IO b
--checkResult label = either (error . (label++) . show) return

--prova::  [Char] -> IO [Char]
--prova name = 
--	do 
--		(ast1, linkedPolcaAnn,includes) <- readFileInfo name
--		let res = fmap f ast1
--		trace (show res) return "hola"
--	where 
--		f nI = trace ("entra" ++ (show nI) ++ "\n") nI
--		--f other = other



--view name = 
--	do
--		ast <- parseMyFile (name ++ ".c")
--		--vacuumToPng name [1..10]
--		--vacuumToSvg name [1..10]
--		vacuumToPng name ast
--		--vacuumToSvg name ast
--		--return (graphToDotParams vacuumParams (nameGraph (vacuum ast)))

--		--vacuum (parseMyFile (name ++ ".c"))
--		--let 
--		--	ast = parseMyFile (name ++ ".c")
--		--in
--		--	nameGraph (vacuum ast)

