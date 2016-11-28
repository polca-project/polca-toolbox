-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module Rul2Has where

import RulesLib
import PragmaPolcaLib

import Language.C
import Language.C.System.GCC   -- preprocessor used
import Language.C.Data.Ident
import Language.C.Data.Name

import Data.List
import Data.Generics
import Data.Char
import Data.Text as Txt (replace, pack, unpack)

import System.IO 

import Debug.Trace

import qualified Text.Groom as Gr

-- Record to store info about variables
data DictVarEntry = 
	DictVarEntry 
	{
		nameSTML :: String,
		varType :: String,
		namesHas :: [String]
    }
    deriving Show

-- Record to store info about operators
data DictOpsEntry = 
	DictOpsEntry 
	{
		nameOp :: String,
		op :: CBinaryOp
    }
    deriving Show

-- Record to store info about internal variables (for blocks)
data DictInternalEntry = 
	DictInternalEntry 
	{
		node :: NodeAnn,
		name :: String,
		asts :: [CBlockItemAnn]
    }
    deriving Show

-- Record to store info about name of functions corresponding to blocks
data DictFunInternalEntry = 
	DictFunInternalEntry 
	{
		nodeFun :: NodeAnn,
		nameFun :: String
    }
    deriving Show

-- State with all the info needed to print the Haskell functions
data PrintState = 
	PrintState 
	{
		currentFree :: Int,
		dictVars :: [DictVarEntry],
		dictInternals :: [DictInternalEntry],
		dictFunInternals :: [DictFunInternalEntry],
		dictOperator :: [DictOpsEntry],
		pragmaNode :: [(Name, String)],
		condForNode :: [String],
		funsChangingState :: [(String, String)],
		isDecl :: Bool
    }
    deriving Show

-- Record that store all the info in a STML rule
data STMLRule = 
	STMLRule 
	{
		ruleName :: String,
		pattern :: [CBlockItemAnn],
		generate :: [CBlockItemAnn],
		condition :: [[CBlockItemAnn]],
		postcondition :: [[CBlockItemAnn]]
    }
    deriving Show


---------------------------------------------------------
-- Main functions
---------------------------------------------------------

main =
	stml2Has "rules"

-- Main function that translates a file with STML rules into a Haskell file
stml2Has name =  
	do
		--let filename = name ++ ".c"
		----handle <- openFile  filename ReadMode
		--polcaAnn' <- parsePolcaAnn filename
		--ast <- parseMyFile filename
		--let (errors,polcaAnn) = (errorsNclean polcaAnn') 
		--let linkedPolcaAnn = linkPolcaAnn ast polcaAnn
		(ast0, linkedPolcaAnn,includes) <- readFileInfo True name True

		let annAST = fmap (\nI -> Ann nI nodePropertiesDefault) ast0
		let ast = changeAnnAST linkedPolcaAnn annAST

		-- trace (show linkedPolcaAnn) 
		writeFile (name ++ ".ast") (Gr.groom ast)
		--print linkedPolcaAnn
		let ast_rules = applyRulesGeneral extract_ast_rules ast
		let pcg_rules = extractInfoSTML ast_rules
		let (rules,rulStmt,rulExp,rulNames, rulDict) = createRules pcg_rules 0 linkedPolcaAnn
		putStrLn ("Rules read:\n**************\n" ++ (unlines rulNames) ++ "**************\n")
		let rulStmtStr = printList id ", " rulStmt
		let rulExpStr = printList id ", " rulExp
		let rulNamesStr = printList id "\", \"" rulNames
		let rulDictStr = intercalate ", " (map (\(rStml, rHas) -> "(\"" ++ rStml ++ "\", " ++ rHas ++ ")") rulDict)
		let listRules = 
				"nameRulesAll = [\"" ++ rulNamesStr ++ "\"]\n"
			++ 	"dictRulesAll = [" ++ rulDictStr ++ "]\n"
			++ 	"nameRules = [r | r <- nameRulesAll, not $ isPrefix \"feat_\" r]\n"
			++ 	"dictRules = [item | item@(r, _) <- dictRulesAll, not $ isPrefix \"feat_\" r]\n"
			++ 	"stmtRules :: [TransState -> CStatAnn -> [((String, CStatAnn, CStatAnn), TransState,[(String, CStatAnn)])]]\n" 
			-- ++ "stmtRules = [" ++ rulStmtStr ++ "]\n"
			++ 	"stmtRules = [r | (_, Right r) <- dictRules]\n"
			++ 	"exprRules :: [TransState -> CExprAnn -> [((String, CExprAnn, CExprAnn), TransState,[(String, CStatAnn)])]]\n"
			-- ++ "exprRules = [" ++ rulExpStr ++ "]\n" 
			++ 	"exprRules = [r | (_, Left r) <- dictRules]\n" 
		let pragmasRules = 
			"pragmasRules = " ++ (show linkedPolcaAnn) ++ "\n"
		let modHeader = 
			"{-# OPTIONS_GHC -w #-}\n\n"
			++ "module Rules where \n\n"
			++ "import RulesLib\n\n"
			++ "import Language.C\nimport Language.C.Data.Ident\n\n"
			++ "import Data.Generics\n\n"
		let haskellFileName = 
			--[(toUpper (head name))] ++ (drop 1 name) ++ ".hs"
			"Rules.hs"
		writeFile haskellFileName (modHeader ++ listRules ++ pragmasRules ++ rules)
		putStrLn (haskellFileName ++ " created.")

---------------------------------------------------------
-- Rule creation
---------------------------------------------------------

-- Function to build all the rules given a list containing data for each rule
createRules [] _ _ =
	("",[],[],[], [])
createRules ((rule@STMLRule{ruleName = ruleName,pattern = patterns0, generate = generates0}):tail_) free_var0 polcaAnn =
		(ruleStr ++ nrules, rulesStmt, rulesExpr, (ruleName:rulesNames), ((ruleName, ruleNameHsEither):dictRules))
	where
		(nrules, nrulesStmt,nrulesExpr,rulesNames, dictRules) = 
			createRules tail_ free_var2 polcaAnn
		(typePat,patterns1,generates1,free_var1) = 
			case  (length patterns0) of 
				1 -> 
					case (head patterns0) of
						(CBlockStmt (CExpr _ _)) ->
							("expr",patterns0,generates0,free_var0)
						-- TODO: It can be a list of statements also
						(CBlockStmt (CLabel (Ident "pat_list" _ _) _ _ _))->
							("expr",patterns0,generates0,free_var0)
						_ -> 
							let 
								intVar fv = 
									buildVarCStmts (buildVarIntName fv) fv
							in 
								("stmts",
									[intVar free_var0] ++ patterns0 
										++ [intVar (free_var0 + 1)],
									[intVar free_var0] ++ generates0 
										++ [intVar (free_var0 + 1)],
									(free_var0 + 2))
				_ -> 

					("stmts",patterns0,generates0,free_var0)
		ruleNameHsIni = "rule_" ++ ruleName 
		ruleNameHs = ruleNameHsIni ++ "_" ++ (show free_var1)
		-- free_var2 = free_var1 + 1
		(rulesStmt, rulesExpr, ruleNameHsEither) = 
			case typePat of 
				"expr" -> 
					(nrulesStmt, ruleNameHs:nrulesExpr, "Left " ++ ruleNameHs)
				_ -> 
					(ruleNameHs:nrulesStmt, nrulesExpr, "Right " ++ ruleNameHs)
		nstmlrule = rule{pattern = patterns1, generate = generates1}
		(ruleStr,free_var2) =
			case typePat of 
				"expr" -> 
					buildRuleExpr nstmlrule ruleNameHsIni free_var1 
				_ ->
					buildRuleStmts nstmlrule ruleNameHsIni free_var1 polcaAnn

---------------------------------------------------------
-- Build rules for statements
---------------------------------------------------------

-- Build rule for a list of statements
buildRuleStmts (rule@STMLRule{ruleName = ruleName, pattern = patterns}) ruleNameHs0 free_var polcaAnn = 
	let 
		config1 = subsequences (applyRulesGeneral searchCommOp patterns)
		config2 = combinationsAllSomeCons (applyRulesGeneral searchAllCons patterns)
		config3 = combinationsAllSomeCons (applyRulesGeneral searchSomeCons patterns)
		config = [c1 ++ c2 ++ c3 | c1<- config1, c2 <- config2, c3 <- config3]
		ruleNameHs = 
			ruleNameHs0 ++ "_" ++ (show free_var)
			--trace ("\nconfig1: " ++ (show config1) ++ "\nconfig2: " ++ (show config2) ++  "\nconfig3: " ++ (show config3) ++  "\nconfig: " ++ (show config)) ruleNameHs0 ++ "_" ++ (show free_var)
		(mainListComp, otherFunction, free_var1) = buildRuleStmtsForConfig rule ruleNameHs0 (free_var + 1) config polcaAnn
	in 
		("\n-- " ++ ruleName ++ "\n" ++ ruleNameHs ++ " state0 old@(CCompound ident bs nodeInfo)=\n"
		++ "\t" ++ mainListComp ++ "[]\n"
		++ ruleNameHs ++ " _ _ = []\n\n"
		++ otherFunction,
		free_var1)

-- Build rule for each possible order of the pattern (according to its commutative operations)
buildRuleStmtsForConfig rule ruleNameHs0 free_var [] _ = 
	("","",free_var)
buildRuleStmtsForConfig (rule@STMLRule{ruleName = ruleName, pattern = patterns0, generate = generates, condition = conditions, postcondition = postconditions}) 
	ruleNameHs0 free_var (config:otherConfigs) polcaAnn = 
	-- main function generation
	("(concat [(" ++ ruleNameMain ++ " item ident nodeInfo old state0) | item@(True,_,_) <- (" 
	 ++ ruleNameSecondary ++ " bs [] " ++ (concat ["True " | _ <- [1..(length onlyPats)]]) ++ ")]) ++ "
	 ++ mainListComp,
	-- main auxiliar function generation
	(buildMainAuxiliaryFunction ruleName ruleNameMain strAllPats strAllInter wholeCondition strGen strWhere [])
	++ strFuns
	++ otherFunction,
	newFreeVar)
	where 
		(mainListComp,otherFunction,newFreeVar) = 
			buildRuleStmtsForConfig rule ruleNameHs0 (currentFree state4) otherConfigs polcaAnn
		patterns1 = changeOrderOp config patterns0
		patterns2 = changeOrderAllCons config patterns1
		patterns = selectOneSomeCons config patterns2
		ruleNameSecondary = ruleNameAux ++ (show (free_var + 1))
		ruleNameMain = ruleNameAux  ++ (show (free_var + 2))
		ruleNameAux =  ruleNameHs0 ++ "_"
		onlyPats = filter (not.isCstmtsVar) patterns
		(allPats,allInter) = buildAllPatsAllInter defaultPrePat patterns True
		state0 = 
			PrintState
			{
				currentFree = free_var + 3,
				dictVars = [],
				dictInternals = [],
				dictFunInternals = [],
				dictOperator = [],
				pragmaNode = polcaAnn,
				condForNode = [],
				funsChangingState = [],
				isDecl = False
			}
		(strAllPats,state1) = printComBlockItemPatList state0 allPats 
		(strAllInter,state2) = printInterList state1 allInter
		dict2 = dictVars state2
		--strGen =  printComBlockItemGenList dict2 generates
		--subs =  applyRulesGeneral (searchSubs dict2) generates 
		genFuns = getAllGenerators state2 generates
		rc_list = 
			case conditions of 
				[] -> []
				c_list -> 
					map (printCond state2) (head c_list)
		(_, rc, unknownConds, dictConds, state3) = buildConditionHaskell rc_list state2
		--(_, rc , unknownConds, dictConds, state3) = buildConditionHaskell rc_list state2
		strWhere0 = buildWhereConds dictConds
		consStr = buildConsDict dict2 "" 
		consNode = buildConsNode state1 
		wholeCondition = (consStr ++ " && " ++ rc ++ " && " ++ consNode)
		--rpc_list = 
		--	case postconditions of 
		--		[] -> []
		--		c_list -> 
		--			map (printCond state2) (head c_list)
		--rpc = buildConditionHaskell rc_list
		(strGen_, strWhere1, state35) =  printAllGenerators ruleName state3 genFuns 0 unknownConds
		strWhere = strWhere0 ++ strWhere1
		strGen =  "concat (" ++ strGen_ ++ "[])\n"
		(strFuns,state4) = buildSecondaryAuxiliaryFunctions ruleNameAux patterns ruleNameSecondary state35


-- Function to print the list of blocks in the rule
printInterList st [] =
	("",st)
printInterList st0 [p] = 
	let 
		(strp,st1) = printInter st0 p
	in 
		(strp ,st1)
printInterList st0 (p:ps) = 
	let 
		(strp,st1) = printInter st0 p
		(strps,st2) = printInterList st1 ps
	in 
		(strp ++ "," ++ strps,st2)

printInter st0 p =
	case p of 
		[] -> 
			("[]",st0)
		[(CBlockStmt (CExpr (Just e) _))] ->
			let 
				(strInter,st1) = 
					(printExprPat st0 e) 
			in 
				(strInter,st1)

printAllGenerators ruleName state0@PrintState{dictVars = dict} ((g,cond):t) current_state unknownConds = 
	let
		subs =  applyRulesGeneral (searchSubs state0) g 
		(strGen, state1) = printComBlockItemGenList state0 g
		(strWhere, last_state) = buildWhereGenerator state1
		defaultBody = 
			"[((\"" ++ ruleName ++ "\", old, (CCompound ident (" 
			-- ++ (buildGenerateStr generateStr dict) ++ ") nodeInfo))]\n"
			++ strGen ++ ") nodeInfo)),state" ++ (show last_state) ++ ", ("
			++ unknownConds ++ " ++ " ++ unknownCondsSpec ++ ") )]"
		-- TODO: Case to not apply rule when substitution does not change has been commented
		-- Note that for expression where if not is treated could lead to several useless applications
		-- body = buildCaseSubs subs defaultBody
		body = defaultBody
		(_, condStr, unknownCondsSpec, dictConds, state11) = buildConditionHaskell cond state1
		----(_, rc , unknownConds, dictConds, state3) = buildConditionHaskell rc_list state2
		strWhereCond = buildWhereConds dictConds
		(strT, strWhereT, state2) = printAllGenerators ruleName state11 t last_state unknownConds
	in 
		("[" ++ body ++ " | " ++ condStr ++ "] ++ " ++ strT, strWhereCond ++ strWhere ++ strWhereT, state2)
printAllGenerators _ state0 [] _ _ = 
	("", "", state0)

-- The main function where the patterns are matched, the conditions are check, and the new list of staemnts is generated
buildMainAuxiliaryFunction ruleName ruleNameAux strAllPats strAllInter consStr strGen strWhere subs =
		ruleNameAux ++ " (True,[" ++  strAllInter 
		++ "], [" ++ strAllPats
		++  "]) ident nodeInfo old state0 | " ++ consStr ++ " =\n\t"
		++ strGen
		++ (case strWhere of 
			 	"" -> ""
			 	_ -> "\twhere" ++ strWhere ++ "\n")
		++ ruleNameAux ++ " _ _ _ _ _ = []\n\n"

-- Build a rule to march all the patterns inside a block 
buildSecondaryAuxiliaryFunctions ruleNameAux [] name state0 = 
	("",state0)
buildSecondaryAuxiliaryFunctions ruleNameAux patterns name state0 = 
	--(auxFun ++ secAuxFun, newPatsFinal, allPatterns, nnfreVar)
	(auxFun ++ secAuxFun,state2)
	where 
		onlyPats = filter (not.isCstmtsVar) patterns
		patsInPats = searchAllPats onlyPats
		(auxFun,state1) = buildSecondaryAuxiliaryFunction name ruleNameAux patsInPats state0
		(secAuxFun,state2) = buildInternalSecondaryAuxiliaryFunctions patsInPats ruleNameAux state1
		----------
		-- auxFun = buildSecondaryAuxiliaryFunction number ruleNameAux patsInPats dict
		--(secAuxFun,newPatsFinal,nnfreVar) = buildInternalSecondaryAuxiliaryFunctions patsInPats ruleNameAux dict nfreeVar

buildInternalSecondaryAuxiliaryFunctions [] _ state0 =
	("",state0) 
buildInternalSecondaryAuxiliaryFunctions ((p,inPats):tail_) ruleNameAux state0 = 
	let 
		--(strFuns, newPats,nfreeVar) = trace ("BSAFL: "++(show lop) ++ " " ++ (show numbers)) buildSecondaryAuxiliaryFunctionsList ruleNameAux lop numbers dict freeVar
		(strFuns1, state1) = buildSecondaryAuxiliaryFunctionsList ruleNameAux inPats state0
		(strFuns2, state2) = buildInternalSecondaryAuxiliaryFunctions tail_ ruleNameAux state1
	in (strFuns1 ++ strFuns2,state2)


buildSecondaryAuxiliaryFunctionsList ruleNameAux [] state0 = 
	("",state0) 
buildSecondaryAuxiliaryFunctionsList ruleNameAux ((nodeInfo,pats):ps) state0 = 
	let
		ruleName = searchRuleName nodeInfo state0
		(strFuns1, state1) = buildSecondaryAuxiliaryFunctions ruleNameAux pats ruleName state0
		(strFuns2, state2) = buildSecondaryAuxiliaryFunctionsList ruleNameAux ps state1
	in 
		(strFuns1 ++ strFuns2, state2)

-- Build the matching function for each pattern into a given block
buildSecondaryAuxiliaryFunction name ruleNameAux [] state0 =
	(name ++" all [] =\n\t[(True,[all],[])]\n",state0)
buildSecondaryAuxiliaryFunction name ruleNameAux onlyPats state0  =
	let 
		(ruleStmtPat,state1) = 
			(buildRuleStmtsPat name onlyPats ruleNameAux (length onlyPats) state0) 
	in
		(name ++" [] acc " ++ (concat ["False " | _ <- [1..(length onlyPats)]]) ++ "=\n\t[(True,[acc],[])]\n"
		++ name ++" [] acc " ++ (concat ["_ " | _ <- [1..(length onlyPats)]]) ++ "=\n\t[(False,[acc],[])]\n"
		++ ruleStmtPat
		++ name ++ " (other:tail_) acc " ++ (concat [(" bool"++ (show i))| i <- [1..(length onlyPats)]]) ++ " =\n\t" 
		++ name ++ " tail_ (acc ++ [other])" ++ (concat [(" bool"++ (show i))| i <- [1..(length onlyPats)]]) ++ "\n\n",
		state1)

-- Build the clauses for each pattern inside a block of statements
buildRuleStmtsPat name [(pat,inPats)] ruleNameAux total state0 = 
	--let (intPatsStr,intInter,intLCP) = trace (show intPats)  buildIntLC intPats
	let 
		(strPat,state1) = 
			printComBlockItemPat state0 pat
		(intPatsStr,intInter,intLCP,callsIntPats,state2) =  
			buildIntLC ruleNameAux state1 inPats
	in
		(name ++ " (stat@" ++ strPat ++ ":tail_) accsl "
		++  (concat ["False " | _ <- [1..(total -1)]]) ++  "True =\n"
		++ "\tlet\n\t\tlistItems = "  ++ name ++ " tail_ [] " 
		++  (concat ["False " | _ <- [1..total]]) 
		++ callsIntPats 
		++ "\n\tin "
		++ "[(True, (accsl:(" ++ intInter ++ "inter)),(stat:(" ++ intPatsStr ++ "pats))) | (True,inter,pats) <- listItems"
		++ intLCP
		++ "] ++ (" ++ name ++ " tail_ (accsl ++ [stat]) " 
		++  (concat ["False " | _ <- [1..(total -1)]]) ++  "True)\n",
		state2)
buildRuleStmtsPat name allpats@((pat,inPats):pats) ruleNameAux total state0 = 
	let 
		(strPat,state1) = 
			printComBlockItemPat state0 pat
		(intPatsStr,intInter,intLCP,callsIntPats,state2) =  
			buildIntLC ruleNameAux state1 inPats
		(strTail,state3) = 
			(buildRuleStmtsPat name pats ruleNameAux total state2) 
	in
		(name ++ " (stat@" ++ strPat ++ ":tail_) accsl "
		++ (concat ["False " | _ <- [1..(total - (length allpats))]]) 
		++ (concat ["True " | _ <- [1..(length allpats)]]) ++ "=\n"
		++ "\tlet\n\t\tlistItems = "  ++ name ++ " tail_ [] " 
		++ (concat ["False " | _ <- [0..(total - (length allpats))]]) 
		++ (concat ["True " | _ <- [2..(length allpats)]]) 
		++ callsIntPats
		++ "\n\tin "
		++ "[(True, (accsl:(" ++ intInter ++ "inter)),(stat:(" ++ intPatsStr ++ "pats))) | (True,inter,pats) <- listItems"
		++ intLCP
		++ "] ++ (" ++ name ++ " tail_ (accsl ++ [stat]) " 
		++ (concat ["False " | _ <- [1..(total - (length allpats))]]) 
		++ (concat ["True " | _ <- [1..(length allpats)]]) ++ ")\n"
		++ strTail,
		state3)
buildRuleStmtsPat _ [] _ _ state0 = 
	("",state0)


-- Build for each block in the pattern names to put inside the list comprehension, the selectors and also the call to the matching function
buildIntLC _ state [] =
	("","","","",state)
buildIntLC ruleNameAux state ((_,[]):ps) =
	let (rp,ri,rl,rc,nstate) = buildIntLC ruleNameAux state ps
	in (rp,
		--("[[]] ++ " ++ ri),
		ri,
		rl,
		rc,
		nstate)
buildIntLC ruleNameAux state@PrintState{currentFree = n} ((nInfo,list):ps) =
	let 
		(rp,ri,rl,rc,nstate) = buildIntLC ruleNameAux (state{currentFree = n + 1}) ps
		funName = ruleNameAux ++ (show n) 
	in (("pats" ++ (show n) ++ " ++ " ++ rp),
		("inter" ++ (show n) ++ " ++ " ++ ri),
		(", (True, inter" ++ (show n) ++ ", pats" ++ (show n) ++ ") <- listItems" ++ (show n) ++ rl),
		"\n\t\tlistItems" ++ (show n) ++ " = " 
		++ funName ++ " " 
		++ (searchCompoundName nInfo state)
		++ " [] " 
		++ (concat ["True "| _ <- [1..(length (filter (not.isCstmtsVar) list))]])
		++ rc,
		nstate{dictFunInternals = (
			(DictFunInternalEntry
			{
				nameFun = funName,
				nodeFun = nInfo
			})
			:(dictFunInternals nstate))})


---------------------------------------------------------
-- Build rules for expressions
---------------------------------------------------------

emptyState = 
	PrintState
	{
		currentFree = 0,
		dictVars = [],
		dictInternals = [],
		dictFunInternals = [],
		dictOperator = [],
		pragmaNode = [],
		condForNode = [],
		funsChangingState = [],
		isDecl = False
	}

-- Build a rule for expressions
buildRuleExpr (rule@STMLRule{ruleName = ruleName, pattern = p, generate = g, condition = c}) ruleNameHs0 fv0 = 
	let 
		ruleNameHs = ruleNameHs0 ++ "_" ++ (show fv0)
		ps = getAllGenerators emptyState p
		(rulesExpr,fv1) = 
			foldl (buildRuleExprList ruleNameHs ruleName g c) ("",(fv0 + 1)) ps
	in 
		("\n-- " ++ ruleName ++ "\n"
		++ rulesExpr
		++ ruleNameHs ++ " _ _ = []\n",
		fv1)

buildRuleExprList ruleNameHs ruleName g c (acc,fv0) (p,_) = 
	let 
		patsBinOp = getAllBinOperators p 
	in 
		foldl (buildRuleExprCaseOperList ruleNameHs ruleName g c) (acc,fv0) patsBinOp

buildRuleExprCaseOperList ruleNameHs ruleName g c (acc,fv0) (p,dictOp) = 
	let  
		patSeq = [(p,(subsequences (applyRulesGeneral searchCommOp p)))]
	in 
		foldl (buildRuleExprCaseList ruleNameHs ruleName g c dictOp) (acc,fv0) patSeq

buildRuleExprCaseList ruleNameHs ruleName g c dictOp (acc,fv0) (p,seqp) = 
	foldl (buildRuleExprCase ruleNameHs ruleName p g c dictOp) (acc,fv0) seqp

-- Build a concrete clause of a rule for expressions
buildRuleExprCase ruleNameHs ruleName p g c dictOp (acc,fv0) ops = 
	let 
		(CBlockStmt (CExpr (Just ep) _)) = changeOrderOp ops (head p)
		(CBlockStmt (CExpr (Just eg) _)) = head g 
		(rp,rg,rc,subs,dictConds,unknownConds, fv1) = 
			get_reps printExprPat printExprGen ep eg c dictOp fv0
		whereConds = buildWhereConds dictConds
		defaultBody = 
			"[((\"" ++ ruleName 
			++ "\",old," ++ rg
			++ "), state, "
			++ unknownConds
			++ ")]"
			++ (case whereConds of 
				"" -> 
					""
				_ -> 
					"\n\twhere" ++ whereConds)
		body = buildCaseSubs subs defaultBody
	in 
		(acc ++ ruleNameHs 
		++ "\n\tstate old@" ++ rp
		++ "\n\t| " ++ rc 
		++ " =\n\t" ++ body 
		++ "\n"
		,fv1)

-- Get the string representation of a rule
get_reps funp fung p g c dictOp fv0 =
	let
		state0 = 
			PrintState
			{
				currentFree = fv0,
				dictVars = [],
				dictInternals = [],
				dictFunInternals = [],
				dictOperator = dictOp,
				pragmaNode = [],
				condForNode = [],
				funsChangingState = [],
				isDecl = False
			}
		(rp,state1) = funp state0 p
		dict = dictVars state1
		(rg, state2) =  fung state1 g
		rc_list = 
			case c of 
				[] -> []
				c_list -> 
					map (printCond state1) (head c_list)
		(_, rc , unknownConds, dictConds, state3) = buildConditionHaskell rc_list state2
		fv1 = currentFree state3
		consStr = buildConsDict dict "" 
		wholeCondition = (consStr ++ " && " ++ rc)
		subs =  applyRulesGeneral (searchSubs state1)  g 
	in (rp,rg,wholeCondition,subs,dictConds, unknownConds, fv1)

buildWhereCond ((cv,cu),c) = 
	"(" ++ cv ++ ", " ++ cu ++ ") = " 
	++ (case c of 
			"True" ->
				"(True, [])"
			_ ->
				c)

buildWhereConds dictConds = 
	foldl 
		(\acc item -> acc ++ "\n\t\t" ++ buildWhereCond item) 
		"" 
		dictConds

---------------------------------------------------------
-- Rule info extraction
---------------------------------------------------------

-- Extract from STML file all the needed information
extractInfoSTML ((name,ast_rule):tail_) = 
	let 
		pgc = applyRulesGeneral extract_ast_pcg ast_rule
		p = head [asts_p | ("pattern",(CCompound _ asts_p _)) <- pgc]
		g = head [asts_g | ("generate",(CCompound _ asts_g _)) <- pgc]
		c = [asts_c | ("condition",(CCompound _ asts_c _)) <- pgc]
		pc = [asts_pc | ("postcondition",(CCompound _ asts_pc _)) <- pgc]
	in (STMLRule
		{
			ruleName = name,
			pattern = p,
			generate = g,
			condition = c,
			postcondition = pc
		}:(extractInfoSTML tail_))
extractInfoSTML [] = 
	[]

---------------------------------------------------------
-- Read from ast
---------------------------------------------------------

-- Extract all the rules of the ast
extract_ast_rules :: CExtDeclAnn -> [(String,CStatAnn)]
extract_ast_rules (CFDefExt (CFunDef _ (CDeclr (Just (Ident name _ _)) _ _ _ _) _ rhs _ )) = 
	[(name,rhs)]
extract_ast_rules _ =
	[] 

-- Extract a part of a rule (i.e. pattern,condition or generate)
extract_ast_pcg :: CStatAnn -> [(String,CStatAnn)]
extract_ast_pcg (CLabel (Ident pcg _ _) ast_pcg _ _) = 
	[(pcg,ast_pcg)]
extract_ast_pcg _ =
	[] 


---------------------------------------------------------
-- Build a list of all patterns and all intermediate spaces
---------------------------------------------------------

--buildAllPatsAllInter::[CBlockItem] -> Bool -> ([CBlockItem],[[CBlockItem]])
buildAllPatsAllInter _ [] _ = 
	([],[])
buildAllPatsAllInter pre [p] first = 
	let 
		(np,inter,interFinal) = buildInitialFinalAllPatsAllInter pre p first
		(pats1,inters1) = buildAllPatsAllInterPat np
	in
		(np ++ pats1,inter ++ inters1 ++ interFinal)
buildAllPatsAllInter pre (p:ps) first = 
	let 
		(np,inter,_) = buildInitialFinalAllPatsAllInter pre p first
		(pats1,inters1) = buildAllPatsAllInterPat np
		(pats2,inters2) = buildAllPatsAllInter p ps False
	in
		(np ++ pats1 ++ pats2,inter ++ inters1 ++ inters2)

defaultPrePat = 
	CBlockStmt (CBreak undefNodeAnn)

buildAllPatsAllInterPat p =
	let
		(_,compInfo) = unzip (searchAllPats p)
		(_,patsInPat) = 
			case compInfo of 
				[] -> 
					([],[])
				_ -> 
					unzip (head compInfo)
	in 
		buildAllPatsAllInter defaultPrePat (concat patsInPat) True

buildInitialFinalAllPatsAllInter pre p first =
	case (isCstmtsVar p) of 
		True ->  
			([],[[p]],[])
		False -> 
			case first of 
				True -> 
					([p],[[]],[[]])
				False ->
					case (isCstmtsVar pre) of 
						True -> 
							([p],[],[[]])
						False -> 
							([p],[[]],[[]])

---------------------------------------------------------
-- Extract patterns from compound expressions
---------------------------------------------------------

-- Search patterns inside blocks of statements
searchAllPats :: [CBlockItemAnn] -> [(CBlockItemAnn,[(NodeAnn,[CBlockItemAnn])])]
searchAllPats [] =
	[]
searchAllPats (pat:pats) =
	let 
		patsInPat = cleanPatsInPat pat (applyRulesGeneral searchCompoundComponents pat)
	in 
		patsInPat:(searchAllPats pats) 
	
searchCompoundComponents (CCompound _ stmts nI) = 
	[(nI,stmts)]
searchCompoundComponents _ = 
	[]

-- To remove the nodes that are descendents of others belonging to 
-- the group. It corrects the result of searchCompoundComponents that
-- returns all the CCompound of the AST, when we only want the CCompound
-- of first level, i.e. without CCompound parents. 
cleanPatsInPat p infoComp = 
	let 
		(compPatsInPat,_) = unzip infoComp
		patsInPatWithDescendents = [(nI,applyRulesGeneral (searchDescendents nI) p) | nI <- compPatsInPat]
		compPatsWithOutDescendents = removeDescendents compPatsInPat patsInPatWithDescendents
		finalCompPatsInPat = [item | item@(nI,_) <- infoComp,(any (geq nI) compPatsWithOutDescendents)]
	in
		(p,finalCompPatsInPat)

searchDescendents nIsearched (CCompound _ stmts nI)
	| (geq nIsearched nI) = 
		let (des,_) = unzip (applyRulesGeneral searchCompoundComponents stmts)
		in des 
	| otherwise = 
		[]
searchDescendents _ _ = 
	[]

removeDescendents allPats ((p,ds):tail_) = 
	removeDescendents (deleteFirstsBy geq allPats ds) tail_
removeDescendents allPats [] = 
	allPats

---------------------------------------------------------
-- Print pattern
---------------------------------------------------------

printStmtPatList state [] = 
	([],state)
printStmtPatList state0 (s:ss) = 
	let 
		(s_s,state1) = printStmtPat state0 (extractStmt s)
		(s_ss,state2) = printStmtPatList state1 ss
	in  ((s_s:s_ss),state2)

printPatList _ state0 [] =
	("", state0)
printPatList fun state0 [e] = 
	fun state0 e
printPatList fun state0 (e:es) = 
	let 
		(se,state1) = fun state0 e
		(ses,state2) = printPatList fun state1 es
	in ((se ++ ", " ++ ses), state2)


printComBlockItemPatList state0 list =
	printPatList printComBlockItemPat state0 list

--CNestedFunDef (CFunctionDef a)
printComBlockItemPat state0 (CBlockStmt (CExpr (Just (CCall (CVar (Ident "cdecl" _ _) _) [ctypefun, e] _)) _)) =
	let 
		(ctype, cstate00) = 
			case ctypefun of 
				(CCall (CVar (Ident "cint" _ _) _) _ _) ->
					("CIntType", state0)
				(CCall (CVar (Ident "cfloat" _ _) _) _ _) ->
					("CFloatType", state0)
				(CCall (CVar (Ident "cstruct" _ _) _) argsCS _) ->
					let 			
						(nargsCS,state01_) = (printExprPat state0 (argsCS!!0))
					in 
						("CSUType (CStruct CStructTag (Just (Ident "
					  	++ nargsCS ++ " _ _)) Nothing [] _)",  state01_)
		(var, derivedDecl, nstate) = 
			case e of 
				(CVar (Ident name _ _) _) ->
					let 
						(se, state1) = (printExprPat cstate00{isDecl = True} e)
					in 
						(se, "[]", state1)
				(CCall (CVar (Ident "cexpr" _ _) _) _ _) ->
					let 
						(se, state1) = (printExprPat cstate00{isDecl = True} e)
					in 
						--(se, "[]", state0{currentFree = (currentFree state1)})
						(se, "[]", state1)
				(CIndex (CIndex vardecl e1 _) e2 _) ->
					let 
						(se, state1) = (printExprPat cstate00{isDecl = True} vardecl)
						(d1, state2) = printArrayDeclPat state1 e1
						(d2, state3) = printArrayDeclPat state2 e2
					in 
						(se, "[" ++ d1 ++ ", " ++ d2 ++ "]", state3)
				(CIndex vardecl e _) ->
					let 
						(se, state1) = (printExprPat cstate00{isDecl = True} vardecl)
						(d1, state2) = printArrayDeclPat state1 e
					in 
						(se, "[" ++ d1 ++ "]", state2)
				other ->
					error ("Declaration no treated: " ++ (prettyMyAST (fmap (\(Ann nI _) -> nI) other)) ++ "\n" ++(show (fmap (\(Ann nI _) -> nI) other))) 
		sdecl =
			"(CDecl [CTypeSpec (" ++ ctype ++ " _)]"
			--  ++ " [(Just (CDeclr (Just (Ident " ++ var ++ " _ _)) " ++ derivedDecl ++ " _ _ _)"
			++ " [(Just (CDeclr (Just " ++ var ++ ") " ++ derivedDecl ++ " _ _ _)"
			++ ", _, _)] _)"
	in ("(CBlockDecl " ++ sdecl ++ ")",nstate)
printComBlockItemPat state0 (CBlockStmt s) =
	let (ss,state1) = (printStmtPat state0 s)
	in ("(CBlockStmt " ++ ss ++ ")",state1)

printArrayDeclPat state0 e = 
	let 
		(se, state1) = (printExprPat state0 e)
	in 
		("(CArrDeclr _ (CArrSize _ " 
		++ se ++") _)",
		state1)


printExprPat (state0@PrintState{currentFree = freeVar0}) (CCall (CVar (Ident name_fun _ _) _) args nodeInfo) = 
	case name_fun `elem` ["cstmts","cstmt","cexpr"] of 
		True -> 
			((buildVarName (extractVarName (args!!0)) freeVar0),
			state0
			 { 
			 	currentFree = freeVar0 + 1,
			 	isDecl = False,
			 	dictVars = 
			 		insertInDictVar (dictVars state0)
				 		DictVarEntry 
				 		{
				 			nameSTML = 
				 				(extractVarName (args!!0)),
				 			varType =
				 				name_fun,
				 			namesHas = 
				 				case isDecl state0 of 
				 					True ->
				 						["(CVar " ++ (buildVarName (extractVarName (args!!0)) freeVar0) ++ " undefNodeAnn)"]
				 					False -> 
				 						[(buildVarName (extractVarName (args!!0)) freeVar0)]
				 		}
			 })
		False -> 
			let 
				(sargs,state1) = printPatList printExprPat state0 args
				-- (newNodeInfo,state2) = (printNodePat state1 nodeInfo)
				(newNodeInfo, state2) = ("_", state1)
				scall = 
					"(CCall (CVar (Ident " ++ name_fun
					++ " _ _) _) [" ++ sargs
					++ "] " ++ newNodeInfo ++ ")"
			in
				(scall,
				state2)
printExprPat state0 (CAssign op lhs rhs _) = 
	let 
		(slhs,state1) = (printExprPat state0 lhs) 
		(srhs,state2) = (printExprPat state1 rhs) 
	in 
		("(CAssign " ++ (show op)
		++ " " ++ slhs 
		++ " " ++ srhs
		++ " _)",
		state2)
printExprPat state0 (CCond con then_ else_ _) = 
	let 
		(scon,state1) = (printExprPat state0 con)
		(sthen_,state2) = (printMaybeState printExprPat state1 then_)
		(selse_,state3) = (printExprPat state2 else_)
	in 
		("(CCond " ++ scon 
		++ " " ++ sthen_
		++ " " ++ selse_
		++ " _)",
		state3)
printExprPat state0 (CBinary bop lop rop _) =
	let 
		(slop,state1) = (printExprPat state0 lop)
		(srop,state2) = (printExprPat state1 rop)
	in 
		("(CBinary " ++ (show bop) 
		++ " " ++ slop
		++ " " ++ srop
		++ " _)",
		state2)
printExprPat state0 (CUnary uop op _) = 
	let
		(sop,state1) = (printExprPat state0 op)
	in
		("(CUnary "++ (show uop) 
		++ " " ++ sop
		++ " _)",
		state1)
printExprPat state0 (CIndex v i _) = 
	let 
		(sv,state1) = (printExprPat state0 v)
		(si,state2) = (printExprPat state1 i)
	in 
		("(CIndex " ++ sv
		++ " " ++ si
		++ " _)",
		state2)
printExprPat state0 (CVar (Ident name_var _ _) _) = 
	("(CVar (Ident " ++ name_var ++ " _ _) _)",state0)
printExprPat (state0@PrintState{currentFree = freeVar0}) (CConst (CIntConst number _)) = 
	("(CConst (CIntConst " ++ (buildVarName (show number) freeVar0) ++ " _))",
	state0
	 { 
	 	currentFree = freeVar0 + 1,
	 	dictVars = 
	 		insertInDictVar (dictVars state0)
		 		DictVarEntry 
		 		{
		 			nameSTML = 
		 				(show number),
		 			varType =
		 				"integer",
		 			namesHas = 
		 				[(buildVarName (show number) freeVar0)]
		 		}
	 })
printExprPat (state0@PrintState{currentFree = freeVar0}) (CConst (CFloatConst number _)) = 
	let 
		hasVarName = (buildVarName [ c | c <- (show (number)), c /= '.' ] freeVar0)
	in 
		("(CConst (CFloatConst " ++ hasVarName ++ " _))",
		state0
		 { 
		 	currentFree = freeVar0 + 1,
		 	dictVars = 
		 		insertInDictVar (dictVars state0)
			 		DictVarEntry 
			 		{
			 			nameSTML = 
			 				(show number),
			 			varType =
			 				"float",
			 			namesHas = 
			 				[hasVarName]
			 		}
		 })

--CIntConst CInteger a	 
--CCharConst CChar a	 
--CFloatConst CFloat a	 
--CStrConst CString a

--printExprPat _ other =
--	trace ("\n\n\n\n" ++ (show other) ++"\n\n\n\n") ""

--CComma [CExpression a] a	 	 
--CCast (CDeclaration a) (CExpression a) a	 
--CSizeofExpr (CExpression a) a	 
--CSizeofType (CDeclaration a) a	 
--CAlignofExpr (CExpression a) a	 
--CAlignofType (CDeclaration a) a	 
--CComplexReal (CExpression a) a	 
--CComplexImag (CExpression a) a	  
--CMember (CExpression a) Ident Bool a
--CCompoundLit (CDeclaration a) (CInitializerList a) a	
--CStatExpr (CStatement a) a	
--CLabAddrExpr Ident a	
--CBuiltinExpr (CBuiltinThing a)


printStmtPat state0 (CExpr e _) =
	let 
		(se_,state1)  =
			case e of 
				Nothing -> 
					("Nothing",state0)
				Just e_ -> 
					printExprPat state0 e_ 
		se = 
			case e of 
				(Just (CCall (CVar (Ident "cstmts" _ _) _) _ _)) ->
					se_
				(Just (CCall (CVar (Ident "cstmt" _ _) _) _ _)) ->
					se_
				_ ->
					let 
						se__ = 
							case e of 
								Just e_ ->
									"(Just " ++ se_ ++ ")"
								Nothing ->
									se_
					in
						printExprAsStmt se__  "_"	

	in 
		(se,state1)
printStmtPat state0 (CCompound _ bi nodeInfo) =
	case bi of 
		[] ->
			("(CCompound _ [] _)",state0)
		_ ->
			let 
				--(sbi,state1) = (printComBlockItemPatList state0 bi)
				name = buildVarIntName (currentFree state0)
				state1 = 
					 state0
					 {
					 	currentFree = (currentFree state0) + 1,
					 	dictInternals = 
					 		insertInDictInternals (dictInternals state0)
					 			DictInternalEntry
					 			{
					 				name = name,
					 				node = nodeInfo,
					 				asts = []
					 			}
					 }
				-- TODO: Not sure if nodeInfo in dictInternals sould be the new one
				-- (newNodeInfo,state2) = (printNodePat state1 nodeInfo)
				(newNodeInfo,state2) = ("_", state1)
			in
				--("(CCompound _ [" ++ sbi ++ "] _)",state1)
				("(CCompound _ "++ name ++ " " ++ newNodeInfo ++ ")",
				 state2)
printStmtPat state0 (CIf cond then_ else_ _) =
	let 
		(scond,state1)  =  printExprPat state0 cond
		(sthen_,state2)  = printStmtPat state1 then_
		(selse_,state3)  = printMaybeState printStmtPat state2 else_
	in
		("(CIf " ++ scond
		++ " " ++ sthen_ 
		++ " " ++ selse_
		++ " _)",
		state3)
printStmtPat state0 (CWhile cond body isDoWhile _) =
	let
		(scond,state1)  =  printExprPat state0 cond
		(sbody,state2)  = printStmtPat state1 body
	in
		("(CWhile  " ++ scond
		++ " " ++ sbody 
		++ " " ++ (show isDoWhile) 
		++ " _)",
		state2)
printStmtPat state0 (CFor assign cond inc body node) =
	let
		(sassign,state1) = 
			case assign of 
				Left assignL ->
					let 
						(sassignL,state1_) = 
							printMaybeState printExprPat state0 assignL
					in 
						("(Left " ++ sassignL ++ ")",state1_)
		(scond,state2) = (printMaybeState printExprPat state1 cond)
		(sinc,state3) = (printMaybeState printExprPat state2 inc)
		(sbody,state4) = (printStmtPat state3 body)
		(nodeInfo,state5) = (printNodePat state4 node)
	in 
		("(CFor " ++ sassign
		++ " " ++ scond
		++ " " ++ sinc 
		++ " " ++ sbody 
		++ " " ++ nodeInfo
		++  ")",
		state5)

--printStmtPat state0 (CLabel _ stmt _ node) =
--	let 
--		(sstmt,state1) = (printStmtPat state0 stmt)
--		(nodeInfo,state2) = (printNodePat state1 (nameOfNode node))
--	in 
--		--("(CLabel _ " ++ sstmt ++ " _ " ++ nodeInfo ++ ")",state2) 
--		("_",state2) 

--CLabel Ident (CStatement a) [CAttribute a] a	
--CSwitch (CExpression a) (CStatement a) a	
--CCase (CExpression a) (CStatement a) a	
--CCases (CExpression a) (CExpression a) (CStatement a) a	
--CDefault (CStatement a) a	
--CGoto Ident a	
--CGotoPtr (CExpression a) a	
--CCont a	
--CBreak a	
--CReturn (Maybe (CExpression a)) a	
--CAsm (CAssemblyStatement a) a	

---------------------------------------------------------
-- Print generate
---------------------------------------------------------

printGenList _ _ state0 [] =
	("", state0)
printGenList fun _ state0 [e] = 
	fun state0 e
printGenList fun sep state0 (e:es) = 
	let 
		(se,state1) = fun state0 e
		(ses,state2) = printGenList fun sep state1 es
	in ((se ++ sep ++ ses), state2)

printExprGenList state0 list =
	--let 
	--	(expGen, state1) = printExprGen state0
	--in 
	--	(printList expGen ", " list, state1
	printGenList printExprGen ", " state0 list

printComBlockItemGenList state0 list =
	printGenList printComBlockItemGen " ++ " state0 list

printComBlockItemGen state0@PrintState{dictVars = dict} (CBlockStmt s) =
	case s of 
		(CExpr (Just (CCall (CVar (Ident "subs" _ _) _) args _)) _) ->
				let 
					(sstm, state1) = printStmtGen state0 s
					strNOcstmts = ("[(CBlockStmt " ++ sstm ++ ")]", state1)
					strcstmts = printStmtGen state0 s
				in 
					case (extractType dict (args!!0)) of 
						(Just "cstmts") -> 
							strcstmts
						(Just "cstmt") -> 
							strNOcstmts
						(Just "cexpr") -> 
							strNOcstmts
						_ -> 
							strcstmts
					
		--(CExpr (Just (CCall (CVar (Ident "inline_stmt" _ _) _) args _)) _) ->
		--	case (extractType dict (args!!0)) of 
		--		(Just "cstmt") -> 
		--			printStmtGen state0 s
		--		_ -> 
		--			let 
		--				(sstm, state1) = printStmtGen state0 s
		--			in 
		--				("[(CBlockStmt " ++ sstm ++ ")]", state1)
		(CExpr (Just (call@(CCall (CVar (Ident "cstmts" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "cdecl" ident _) _) [ctypefun, e] _)) ) _) -> 
			let 
				(ctype, state01) = 
					case ctypefun of 
						(CCall (CVar (Ident "cint" _ _) _) _ _) ->
							("CIntType", state0)
						(CCall (CVar (Ident "cfloat" _ _) _) _ _) ->
							("CFloatType", state0)
				(var, derivedDecl, nstate) = 
					case e of 
						(CVar (Ident name _ _) _) ->
							let 
								(se, state1) = (printExprGen state01 e)
							in 
								(se, "[]", state1)
						(CCall (CVar (Ident "cexpr" _ _) _) _ _) ->
							let 
								(se, state1) = (printExprGen state01 e)
							in 
								("(getIdent " ++ se ++ ")", "[]", state1)
								-- (se, "[]", state1)
						(CIndex (CIndex vardecl e1 _) e2 _) ->
							let 
								(se, state1) = printExprGen state01 vardecl
								(d1, state2) = printArrayDeclGen state1 e1
								(d2, state3) = printArrayDeclGen state2 e2
							in 
								(se, "[" ++ d1 ++ ", " ++ d2 ++ "]", state3)
						(CIndex vardecl e1 _) ->
							let 
								(se, state1) = (printExprGen state0 vardecl)
								(d1, state2) = printArrayDeclGen state1 e1
							in 
								("(getIdent " ++ se ++ ")", "[" ++ d1 ++ "]", state2)
						other -> 
							error ("Declaration no treated: " ++ (prettyMyAST (fmap (\(Ann nI _) -> nI) other)) ++ "\n" ++(show (fmap (\(Ann nI _) -> nI) other))) 
				-- (name_var, state1) = printExprGen state0 e
			in
				-- ("[(CBlockDecl (CDecl [CTypeSpec (" ++ ctype ++ " undefNodeAnn)]"
				-- ++ " [(Just (CDeclr (Just (" ++ name_var ++ "_ident"
				-- ++ ")) [] Nothing [] undefNodeAnn)"
				-- ++ ", Nothing, Nothing)] undefNodeAnn))]",
				-- state1)
			("[(CBlockDecl (CDecl [CTypeSpec (" ++ ctype ++ " undefNodeAnn)]"
				++ " [(Just (CDeclr (Just (" ++ var
				++ ")) " ++ derivedDecl ++ " Nothing [] undefNodeAnn)"
				++ ", Nothing, Nothing)] undefNodeAnn))]",
				nstate)
		(CExpr (Just (call@(CCall (CVar (Ident "fresh" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "change_access" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "count_no_decl" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "filter_no_decl" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "filter_decl" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "change_array" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "decl_from_struct" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "lhs" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		(CExpr (Just (call@(CCall (CVar (Ident "rhs" _ _) _) _ _)) ) _) -> 
			printExprGen state0 call
		_ -> 

			let 
				(sstm, state1) = printStmtGen state0 s
			in 
				("[(CBlockStmt " ++ sstm ++ ")]", state1)
printComBlockItemGen state0 (CBlockDecl (CDecl declSpec die _)) =
	let 
		(sdiegen, state1) = 
			(printGenList printDieGen  ", " state0 die)
	in
		("[(CBlockDecl (CDecl " ++ (printDeclSpec declSpec)
		++ " [" ++ sdiegen ++ "] undefNodeAnn))]",
		state1)

printDieGen state0 (decl, ini, expr) = 
	let 
		(sdecl, state1) = (printMaybeState printCDeclr state0 decl)
		(sini, state2) = (printMaybeState printCInitializer state1 ini)
		(sexpr, state3) = (printMaybeState printExprGen state2 expr)
	in 
		("(" ++ sdecl
		++ ", " ++ sini
		++ ", " ++ sexpr
		++ ")",
		state3)

printDeclSpec [(CTypeSpec (CIntType _))] = 
	--"[CTypeSpec (" ++ (show ctype) ++ " undefNodeAnn)]"
	"[CTypeSpec (CIntType undefNodeAnn)]"

printCDeclr state0 (CDeclr (Just (Ident name_var ident _)) derivedDecl _ _ _) = 
	let 
		(sderivedDecl, state1) = (printGenList printDerivedDecl ", " state0 derivedDecl)
	in
		("(CDeclr (Just (Ident " ++ (show name_var) ++ " " ++ (show ident)
		++ " undefNode)) [" ++ sderivedDecl
		++ "] Nothing [] undefNodeAnn)",
		state1)


printDerivedDecl state0 (CPtrDeclr _ _) = 
	("(CPtrDeclr [] undefNodeAnn)", state0)
printDerivedDecl state0 (CArrDeclr _ arraySize _) = 
	let
		(sarraySize, state1) = (printArraySize state0 arraySize) 
	in 
		("(CArrDeclr [] " ++ sarraySize ++ " undefNodeAnn)",
		state1)

printArraySize state0 (CNoArrSize b) = 
	("(CNoArrSize " ++ (show b) ++ ")", state0)
printArraySize state0 (CArrSize b e) = 
	let 
		(se, state1) = (printExprGen state0 e)
	in 
		("(CArrSize " ++ (show b) 
		++ " " ++ se
		++")",
		state1)

printArrayDeclGen state0 e = 
	let 
		(se, state1) = (printExprGen state0 e)
	in 
		("(CArrDeclr [] (CArrSize False " 
		++ se ++") undefNodeAnn)",
		state1)


printCInitializer state0 (CInitExpr exp _) = 
	let 
		(sexp, state1) = (printExprGen state0 exp) 
	in 
		("(CInitExpr " ++ sexp ++ " undefNodeAnn)",
		state1)

printExprGen :: PrintState -> CExprAnn -> (String, PrintState)
printExprGen state0@PrintState{dictVars = dict, currentFree = freeVar0} (CCall (CVar (Ident name_fun ident _) _) args nodeInfo) = 
	case name_fun `elem` ["cstmts","cstmt","cexpr"] of 
		True -> 
			--findNameHas (extractVarName (args!!0)) dict
			let 
				(sargs0, state1) = printExprGen state0 (args!!0)
				(extracted,state2) = tryExtractNameHas state0 (args!!0)
			in 
				if ((sargs0 == extracted) && (name_fun == "cexpr"))
				then 
					let 
						sargs0 = (buildVarName (extractVarName (args!!0)) freeVar0)
					in 
						(sargs0,
						state2
						{ 
							currentFree = freeVar0 + 1,
							dictVars = 
								insertInDictVar dict
						 		DictVarEntry 
						 		{
						 			nameSTML = 
						 				(extractVarName (args!!0)),
						 			varType =
						 				"fvar",
						 			namesHas = 
						 				[sargs0]
						 		}
						})
				else
					if ((sargs0 == extracted) && (name_fun /= "cexpr"))
					then
						let 
							pos = 
								posOfNodeAnn nodeInfo
							file = 
								posFile pos
							line = 
								posRow pos
						in 
							error ("STML var " ++ (extractVarName (args!!0)) ++ " not found. (" ++ file ++ "," ++ (show line) ++ ")" )
					else
						(extracted, state2)
			--let 
			--	printExprGen state0 arg
			--	tryExtractNameHas state0 (args!!0)
			--case 
			--	(extractNameHas dict (args!!0), state0)
		False -> 
			printCallGen state0 name_fun ident args
printExprGen state0 (CAssign op lhs rhs _) = 
	let 
		(slhs, state1) = (printExprGen state0 lhs) 
		(srhs, state2) = (printExprGen state1 rhs)
	in
		("(CAssign " ++ (show op)
		++ " " ++  slhs
		++ " " ++  srhs
		++ " undefNodeAnn)",
		state2)
printExprGen state0 (CCond con then_ else_ _) = 
	let 
		(scon, state1) = (printExprGen state0 con)
		(sthen_, state2) = (printMaybeState printExprGen state1 then_)
		(selse_, state3) = (printExprGen state2 else_) 
	in 
		("(CCond " ++ scon
		++ " " ++ sthen_
		++ " " ++ selse_
		++ " undefNodeAnn)",
		state3)
printExprGen state0 (CBinary bop lop rop _) = 
	let
		(slop, state1) = (printExprGen state0 lop)
		(srop, state2) = (printExprGen state1 rop)
	in
		("(CBinary " ++ (show bop) 
		++ " " ++ slop
		++ " " ++ srop
		++ " undefNodeAnn)",
		state2)
		
printExprGen state0 (CUnary uop op _) = 
	let
		(sop, state1) = (printExprGen state0 op)  
	in 
		("(CUnary "++ (show uop) 
		++ " " ++ sop
		++ " undefNodeAnn)",
		state1)
printExprGen state0 (CIndex v i _) = 
	let 
		(sv, state1) = (printExprGen state0 v) 
		(si, state2) = (printExprGen state1 i) 
	in
		("(CIndex " ++ sv
		++ " " ++ si
		++ " undefNodeAnn)",
		state2)
printExprGen state0 (CVar (Ident name_var ident _) _) = 
	("(CVar (Ident " ++ (show name_var) ++ " " ++ (show ident) ++ " undefNode) undefNodeAnn)",
	state0)
printExprGen state0 (CConst (CIntConst number _)) = 
	("(CConst (CIntConst (cInteger " ++ (show number) ++ " ) undefNodeAnn))",
	state0)
printExprGen state0 (CConst (CFloatConst number _)) = 
	("(CConst (CFloatConst (CFloat \"" ++ (show number) ++ "\" ) undefNodeAnn))",
	state0)


printStmtGen state0@PrintState{dictVars = dict} (CExpr e node) =
	let 
		prev@(se_, state1)  = 
			case e of 
				Nothing ->
					("Nothing", state0)
				(Just e_) ->
					printExprGen state0 e_
		(se, state2) = 
			case e of 
				(Just (CCall (CVar (Ident "cstmts" _ _) _) _ _)) ->
					prev
				(Just (CCall (CVar (Ident "cstmt" _ _) _) _ _)) ->
					prev
				(Just (CCall (CVar (Ident "subs" _ _) _) args _)) ->
					case (extractType dict (args!!0)) of 
						(Just "cexpr") -> 
							let 
								se__ =  "(Just " ++ se_ ++ ")"
							in
								(printExprAsStmt se__  "undefNodeAnn", state1)
						_ ->
							prev
				(Just (CCall (CVar (Ident "inline_stmt" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "inline_expr" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "fresh" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "change_access" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "count_no_decl" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "filter_no_decl" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "filter_decl" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "change_array" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "decl_from_struct" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "lhs" _ _) _) args _)) ->
					prev
				(Just (CCall (CVar (Ident "rhs" _ _) _) args _)) ->
					prev
				_ ->
					let 
						se__ = 
							case e of 
								Just e_ ->
									"(Just " ++ se_ ++ ")"
								Nothing ->
									se_
					in
						(printExprAsStmt se__  "undefNodeAnn", state1)
				--	let 
				--		se__ = 
				--			case e of 
				--				Just e_ ->
				--					"(Just " ++ se_ ++ ")"
				--				Nothing ->
				--					se_
				--	in
				--		(se__, state1)
				--_ ->
				--	prev
	in 
		(se, state2)
printStmtGen state0 (CCompound _ bi _) =
	let
		(sbi, state1) = (printComBlockItemGenList state0 bi) 
	in 
		("(CCompound [] "
		++ case bi of 
			[] ->
				"[]"
			_ ->
				"("++ sbi ++ ")"
		++ " undefNodeAnn)",
		state1)
printStmtGen state0 (CIf cond then_ else_ _) =
	let 
		(scond, state1) = (printExprGen state0 cond)
		(sthen_, state2) = (printStmtGen state1 then_) 
		(selse_, state3) = (printMaybeState printStmtGen state2 else_)
	in
		("(CIf " ++ scond
		++ " " ++ sthen_
		++ " " ++ selse_
		++ " undefNodeAnn)",
		state3)
printStmtGen state0 (CWhile cond body isDoWhile _) =
	let 
		(scond, state1) = (printExprGen state0 cond)
		(sbody, state2) = (printStmtGen state1 body) 
	in 
		("(CWhile  " ++ scond
		++ " " ++ sbody
		++ " " ++ (show isDoWhile) 
		++ " undefNodeAnn)",
		state2)
printStmtGen state0 (CFor assign cond inc body node) =
	let 
		(sassignL, state1) = 
			case assign of 
				Left assignL ->
					let 
						(sassignL_, state1) = (printMaybeState printExprGen state0 assignL)
					in  
						("(Left " ++ sassignL_ ++ ")", state1)
			--other -> 
			--	(trace (show other) "")
		(scond, state2) = (printMaybeState printExprGen state1 cond)
		(sinc, state3) = (printMaybeState printExprGen state2 inc) 
		(sbody, state4) = (printStmtGen state3 body) 
		(nodeInfo,state5) = (printNodeGen state4 node)
	in 
		("(CFor " ++ sassignL
		++ " " ++ scond
		++ " " ++ sinc
		++ " " ++ sbody
		++ " " ++ nodeInfo
		++ ")",
		state5)

--printStmtPat state0 (CFor assign cond inc body node) =
--	let
--		(sassign,state1) = 
--			case assign of 
--				Left assignL ->
--					let 
--						(sassignL,state1_) = 
--							printMaybeState printExprPat state0 assignL
--					in 
--						("(Left " ++ sassignL ++ ")",state1_)
--		(scond,state2) = (printMaybeState printExprPat state1 cond)
--		(sinc,state3) = (printMaybeState printExprPat state2 inc)
--		(sbody,state4) = (printStmtPat state3 body)
--		(nodeInfo,state5) = (printNodePat state4 (nameOfNode node))
--	in 
--		("(CFor " ++ sassign
--		++ " " ++ scond
--		++ " " ++ sinc 
--		++ " " ++ sbody 
--		++ " " ++ nodeInfo
--		++  ")",
--		state5)

printCallGen state0@PrintState{dictVars = dict, currentFree = freeVar0} name_fun ident args = 
	case name_fun of 
		"subs" ->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
				(nargs1,state2) = (tryExtractNameHas state1 (args!!1))
				(nargs2,state3) = (printExprGen state2 (args!!2))
			in
				("(substitute " ++ nargs0 
				++ " " ++ nargs1
				++ " " ++ nargs2 
				++ ")", 
				state3)
		"inline_stmt" ->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
				callStr = "(inline state0 " ++ nargs0 ++ ")"
				freshVar = buildVarName "" freeVar0
			in
				(freshVar, 
				state1
				 {
				 	currentFree = freeVar0 + 1,
				 	funsChangingState = 
				 		((freshVar, callStr):(funsChangingState state1))
				 })
		"inline_expr" ->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
				(nargs1,state2) = (tryExtractNameHas state1 (args!!1))
				callStr = "(inlineExpr state0 " ++ nargs0 ++ " " ++ nargs1 ++ ")"
				freshVar = buildVarName "" freeVar0
			in
				(freshVar, 
				state1
				 {
				 	currentFree = freeVar0 + 1,
				 	funsChangingState = 
				 		((freshVar, callStr):(funsChangingState state2))
				 })
		"fresh" ->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
				callStr = "(fresh_vars state0 " ++ nargs0 ++ ")"
				freshVar = buildVarName "" freeVar0
			in
				(freshVar, 
				state1
				 {
				 	currentFree = freeVar0 + 1,
				 	funsChangingState = 
				 		((freshVar, callStr):(funsChangingState state1))
				 })
		--change_access(cstmts(end), cexpr(v), cpos(1)*cexpr(l2)+cpos(2));
		"change_access"->
			let 
				(nargs0,state1) = 
					case (args!!0) of 
						(CCall _ _ _) ->
							(printExprGen state0 (args!!0))
						_ ->
							(tryExtractNameHas state0 (args!!0))
				(nargs1,state2) = 
					case (args!!1) of 
						(CCall (CVar (Ident "all" _ _) _) _ _) ->
							("",state1)
						_ ->
							(tryExtractNameHas state1 (args!!1))
							-- let 
							-- 	(CVar ivar _, nstate1) = (tryExtractNameHas state1 (args!!1))
							-- in 
							-- 	(ivar, nstate1)
				(nargs2,state3, nameFun) =
					case (args!!2) of 
						(CCall (CVar (Ident "flatten" _ _) _) argsCA _) ->
							let 
								(nargsCS0,state3) = (tryExtractNameHas state2 (argsCA!!0))
							in 
								("(0, " ++ nargsCS0 ++ ")", state3, "changeAccessFlatten")
						(CCall (CVar (Ident "add" _ _) _) argsCA _) ->
							let 
								(nargsCS0,state3) = (printExprGen state2 (argsCA!!0))
							in 
								(nargsCS0, state3, "changeAccessAdd")
				--(nargs2,state3) = (printExprGen state2 (args!!2))
			in
				("(" ++ nameFun ++ " " ++ nargs0 
				++ " " ++ nargs1
				++ " " ++ nargs2
				++ ")", 
				state3)
		--"change_array"->
		--	let 
		--		(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
		--		(nargs1,state2) = (tryExtractNameHas state1 (args!!1))
		--		(nargs2,state3) = (tryExtractNameHas state2 (args!!2))
		--		(nargs3,state4) = (printExprGen state3 (args!!3))
		--		(nargs4,state5) = (printExprGen state4 (args!!4))
		--	in
		--		("(changeArray" 
		--		++ " " ++ nargs0 
		--		++ " " ++ nargs1
		--		++ " " ++ nargs2 
		--		++ " " ++ nargs3
		--		++ " " ++ nargs4 
		--		++ ")", 
		--		state5)
		"change_array"->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
				(nargs1,state2) = (tryExtractNameHas state1 (args!!1))
				(CCall (CVar (Ident "from_struct" _ _) _) argsCA _) = (args!!2)
				(nargsCS0,state3) = (tryExtractNameHas state2 (argsCA!!0))
			in
				("(changeArrayFromStruct" 
				++ " " ++ nargs0 
				++ " " ++ nargs1
				++ " " ++ nargsCS0 
				++ " state0"
				++ ")", 
				state3)
		"count_no_decl"->
			let 
				(nargs0, state1) = (tryExtractNameHas state0 (args!!0))
			in
				("[CBlockStmt (CExpr (Just  (intConstant (fromIntegral (length [x | x <- " 
					++ nargs0
					++ ", not (fst (isBlockDecl x))] )))) undefNodeAnn)]", 
				state1)
		"filter_no_decl"->
			let 
				(nargs0, state1) = (tryExtractNameHas state0 (args!!0))
			in
				("[x | x <- " ++ nargs0 ++ ", not $ fst $ isBlockDecl x]", 
				state1)
		"filter_decl"->
			let 
				(nargs0, state1) = (tryExtractNameHas state0 (args!!0))
			in
				("[x | x <- " ++ nargs0 ++ ", fst $ isBlockDecl x]", 
				state1)
		"decl_from_struct"->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
				(nargs1,state2) = (tryExtractNameHas state1 (args!!1))
				(nargs2,state3) = (printExprGen state2 (args!!2))
			in
				("(declFromStruct" 
				++ " " ++ nargs0 
				++ " " ++ nargs1
				++ " " ++ nargs2 
				++ " state0"
				++ ")", 
				state3)
		"lhs" ->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
			in
				--trace (show nargs0)
				("(getLHS" 
				++ " " ++ nargs0 
				++ ")", 
				state1)
		"rhs" ->
			let 
				(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
			in
				("(getRHS" 
				++ " " ++ nargs0 
				++ ")", 
				state1)
		_ ->
			let 
				(sargs, state1) = (printExprGenList state0 args) 
			in
				("(CCall (CVar (Ident " ++ (show name_fun)
				++ " " ++ (show ident) 
				++ " undefNode) undefNodeAnn) [" ++ sargs
				++ "] undefNodeAnn)",
				state1)


---------------------------------------------------------
-- Print condition
---------------------------------------------------------

printCond (state@PrintState{dictVars = dict})  (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_writes" _ _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
		(Just containerType1) = extractType dict (args!!1)
		nargs1 = transformToStmts containerType1 (nargs!!1)
		(Just containerType0) = extractType dict (args!!0)
		nargs0 = transformToStmts containerType0 (nargs!!0)
	in
		case length args of 
			2 -> 
				"(noWrites " ++ (nargs!!0) ++ " " ++ nargs1 ++ ")"
				--"(null (defs " ++ (nargs!!0)
				-- ++ " " ++ nargs1 
				-- ++ "))"	
			1 -> 
				"(writesNothing " ++ nargs0 ++ ")"	
				--"(null (allDefs " 
				-- ++ nargs0 ++ "))"	
printCond (state@PrintState{dictVars = dict})  (CBlockStmt (CExpr (Just (CCall (CVar (Ident "pure" ident _) _) args _) ) _) ) = 
	(printCond state (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_writes" ident undefNode) undefNodeAnn) args undefNodeAnn) ) undefNodeAnn) ))
--printCond (state@PrintState{dictVars = dict})  (CBlockStmt (CExpr (Just (CCall (CVar (Ident "pure_exp" ident _) _) args _) ) _) ) = 
--	let 
--		nargs = map (extractNameHas dict) args
--		(Just containerType0) = extractType dict (args!!0)
--		nargs0 = transformToStmts containerType0 (nargs!!0)
--	in
--		"(nuller (allDefs " ++ nargs0 ++ "))"
-- TODO: remove next two clauses
--printCond (state@PrintState{dictVars = dict})  (CBlockStmt (CExpr (Just (CCall (CVar (Ident "fake1" ident _) _) args _) ) _) ) = 
--	"(fake1 " ++ (extractNameHas dict (args!!0)) ++ ")"
--printCond (state@PrintState{dictVars = dict})  (CBlockStmt (CExpr (Just (CCall (CVar (Ident "fake2" ident _) _) args _) ) _) ) = 
--	"(fake2 " ++ (extractNameHas dict (args!!0)) ++ ")"		
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_reads" _ _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
		(Just containerType) = extractType dict (args!!1)
		nargs1 = transformToStmts containerType (nargs!!1)
	in
		"(noReads " ++ (nargs!!0) ++ " " ++ nargs1 ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "reads" _ _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
		(Just containerType) = extractType dict (args!!1)
		nargs1 = transformToStmts containerType (nargs!!1)
	in
		"(readsExp " ++ (nargs!!0) ++ " " ++ nargs1 ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_reads_in_written" _ _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
		(Just containerType) = extractType dict (args!!1)
		nargs1 = transformToStmts containerType (nargs!!1)
		(Just containerType0) = extractType dict (args!!0)
		nargs0 = transformToStmts containerType0 (nargs!!0)
	in
		"(noReadInWritten " ++ nargs0 ++ " " ++ nargs1 ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_writes_in_read" _ _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
		(Just containerType) = extractType dict (args!!1)
		nargs1 = transformToStmts containerType (nargs!!1)
		--(Just containerType0) = extractType dict (args!!0)
		--nargs0 = transformToStmts containerType0 (nargs!!0)
	in
		"(noWritesInRead " ++ (nargs!!0) ++ " " ++ nargs1 ++ ")"
		--"(null (uses " ++ (nargs!!0)
		-- ++ " " ++ nargs1
		-- ++ "))"	
-- printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_is_decl" _ _) _) args _) ) _) ) = 
-- 	let 
-- 		nargs = map (extractNameHas dict) args
-- 		(Just containerType) = extractType dict (args!!0)
-- 		nargs0 = transformToStmts containerType (nargs!!0)
-- 	in
-- 		"(notCond $ isBlockDecl $ head " ++ nargs0 ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "all_are_decl" _ _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
	in
		"(all (\\x -> fst $ isBlockDecl x) " ++ (nargs!!0) ++ ", concat $ map (\\x -> snd $ isBlockDecl x) " ++ (nargs!!0) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_cons" _ _) _) args _) ) _) ) = 
	"(isConstant " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_var" _ _) _) args _) ) _) ) = 
	"(isVariable " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_array" _ _) _) args _) ) _) ) = 
	"(isArray " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond state (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_rw" ident _) _) args _) ) _) ) = 
		 "(checkCondUnkList ["
	++ 	(printCond state (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_writes" ident undefNode) undefNodeAnn) args undefNodeAnn) ) undefNodeAnn) ))
	++ 	" , "
	++ 	(printCond state (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_reads" ident undefNode) undefNodeAnn) args undefNodeAnn) ) undefNodeAnn) ))
	++ 	"])"
printCond state (CBlockStmt (CExpr (Just (CCall (CVar (Ident "not" ident _) _) args _) ) _) ) = 
	"(notCond " ++ (printCond state (CBlockStmt (CExpr (Just (args!!0)) undefNodeAnn)) )   ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_block" ident _) _) args _) ) _) ) = 
	"(isBlock " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "has_calls" ident _) _) args _) ) _) ) = 
	let 
		nargs = map (extractNameHas dict) args
		(Just containerType) = extractType dict (args!!0)
		nargs0 = transformToStmts containerType (nargs!!0)
	in 
		"(hasCalls state0 " ++ nargs0 ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_rollable" ident _) _) args _) ) _) ) = 
	"(isRollable " ++ (extractNameHas dict (args!!0)) ++ " " ++ (extractNameHas dict (args!!1)) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "no_empty" ident _) _) args _) ) _) ) = 
	"(noEmpty " ++ (extractNameHas dict (args!!0)) ++ ")"	
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "has_calls_stmt" ident _) _) args _) ) _) ) = 
	"(hasCallsOld " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_expr" ident _) _) args _) ) _) ) = 
	"(isExpr " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond (state@PrintState{dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_call" ident _) _) args _) ) _) ) = 
	"(isCall " ++ (extractNameHas dict (args!!0)) ++ ")"
printCond (state@PrintState{dictOperator = dictOp,dictVars = dict}) (CBlockStmt (CExpr (Just (CCall (CVar (Ident "is_identity" ident _) _) args _) ) _) ) = 
	let 
		opSel = head [(op entryDict) | entryDict  <- dictOp, (nameOp entryDict) == (extractVarName (args!!0))] 
	in 
		"(isIdenity " ++ (show opSel) ++ " " ++ (extractNameHas dict (args!!1)) ++ ")"

transformToStmts varType name = 
	case  varType of 
		"cstmt" ->
			"[(CBlockStmt " ++ name ++ ")]"
		"cexpr" ->
			"[(CBlockStmt (CExpr (Just " ++ name ++ ") undefNodeAnn))]"
		"cstmts" ->
			name

---------------------------------------------------------
-- Print guard of the main clause
---------------------------------------------------------

-- Join a list of conditions
buildConditionHaskell l state@PrintState{currentFree = freeVar}  = 
	let 
		nfreeVar = freeVar + (length l)
		varsIds = [freeVar..(nfreeVar - 1)]
		nameVars = (map (\x -> "var_cond_" ++ (show x)) varsIds)
		unknownConds = (map (\x -> "unknown_" ++ (show x)) varsIds)
		intercaleteInList funEval x = "(" ++ funEval ++ " [" ++ (intercalate ", " x) ++ "])"
	in
	(
			intercaleteInList "and" l
		,	intercaleteInList "and" nameVars
		,	intercaleteInList "concat" unknownConds 
		,	zip (zip nameVars unknownConds) l 
		, 	state{currentFree = nfreeVar}
	)
--buildConditionHaskell [] = 
--	"True"
--buildConditionHaskell [x] =
--	x 
--buildConditionHaskell (x:xs) = 
--	x ++ " && " ++ (buildConditionHaskell xs)


-- Builds the conditions for the pattern variables and constants
buildConsDict [] acc = 
	acc ++ "True"
buildConsDict [DictVarEntry{nameSTML = nameSTML,varType = vtype, namesHas = namesVar}] acc  
	| vtype == "integer" = 
		acc ++ (buildIntDictEntryConst (head namesVar) nameSTML)
	| vtype == "float" = 
		acc ++ (buildFloatDictEntryConst (head namesVar) nameSTML)
	| otherwise = 
		case length namesVar of 
			1 -> 
				acc ++ "True"
			_ ->
				acc ++ (buildConsDictEntry namesVar) 
buildConsDict ((DictVarEntry{nameSTML = nameSTML,varType = vtype,namesHas = namesVar}):dict) acc 
	| vtype == "integer" = 
		buildConsDict dict (acc ++ (buildIntDictEntryConst (head namesVar) nameSTML) ++ " && ")
	| vtype == "float" = 
		buildConsDict dict (acc ++ (buildFloatDictEntryConst (head namesVar) nameSTML) ++ " && ")
	| otherwise = 
		case length namesVar of 
			1 -> 
				buildConsDict dict acc
			_ ->
				buildConsDict dict (acc ++ (buildConsDictEntry namesVar) ++ " && ")

buildConsDictEntry [v1,v2] =
	buildOneConsDictEntry v1 v2
buildConsDictEntry (v1:v2:rest) =
	(buildOneConsDictEntry v1 v2) ++ " && " ++ (buildConsDictEntry (v2:rest))


buildOneConsDictEntry v1 v2 = 
	let 
		nv1 = 
			case (drop ( (length v1) - (length "_ident") ) v1) of 
				"_ident" ->
					"(CVar " ++ v1 ++ " undefNodeAnn)"
				_ ->
					v1 
		nv2 = 
			case (drop ( (length v2) - (length "_ident") ) v2) of 
				"_ident" ->
					"(CVar " ++ v2 ++ " undefNodeAnn)"
				_ ->
					v2 
	in 
		"(exprEqual " ++ nv1 ++ " " ++ nv2 ++ ")"

buildIntDictEntryConst nameVar num = 
	"(getCInteger " ++ nameVar ++")" ++ " == " ++ num

buildFloatDictEntryConst nameVar num = 
	"(show " ++ nameVar ++")" ++ " == \"" ++ num ++ "\""

buildConsNode PrintState{condForNode = []} =
	"True"
buildConsNode (state@PrintState{condForNode = (varCond):tail_}) = 
	varCond ++ " && "
	++ (buildConsNode state{condForNode = tail_})


---------------------------------------------------------
-- Search substitutions
---------------------------------------------------------

searchSubs :: PrintState -> CExprAnn -> [(String,String,String)]
searchSubs state0 (CCall (CVar (Ident "subs" _ _) _) args _) = 
	let 
		(nargs0,state1) = (tryExtractNameHas state0 (args!!0))
		(nargs1,state2) = (tryExtractNameHas state1 (args!!1))
		--type0 = (tryExtractType state2 (args!!0))
		--type1 = (tryExtractType state2 (args!!1))
		--type2 = (tryExtractType state2 (args!!2))
		--(nargs2,_) = (tryExtractNameHas state2 (args!!2))
		(nargs2,_) = (printExprGen state2 (args!!2))
	in 
		case (args!!2) of 
			-- TODO: Problem if args!!2 contains a a free var. This is a temporal ad-hoc solution
			(CCall (CVar (Ident "cexpr" _ _) _) _ _) -> 
				[]
			_ -> 
				[(nargs0,nargs1,nargs2)]
searchSubs _ other =
	[]

---------------------------------------------------------
-- Build where of generator
---------------------------------------------------------

buildWhereGenerator state@PrintState{dictVars = dict, funsChangingState = funs} = 
	let 
		free_vars = 
			[(head (namesHas entryDict), nameSTML entryDict) | entryDict  <- dict, (varType entryDict) == "fvar"]
		(resFree, last_state, patPragmaDicts1) = 
			foldl buildWhereVarDecl  ("", 0, []) free_vars
		(result, nlast_state, patPragmaDicts2) = 
			foldl buildWhereFunChangeState (resFree, last_state, patPragmaDicts1) funs
	in 
		case patPragmaDicts2 of 
			[] ->
				(result, nlast_state)
			concatDict ->
				(result ++ "\n\t\tpatPragmaDict = " ++ (printList id " ++ " concatDict), nlast_state)


buildWhereVarDecl (acc, last_state, patPragmaDicts) (nvHas, nvSTML) =
	(acc ++ "\n\t\t" ++ "(" ++ nvHas ++ ", "  ++ nvHas ++ "_ident, patPragmaDict" 
		++ (show (last_state + 1)) ++ ", " ++ " state" ++ (show (last_state + 1)) 
		++ ") = buildFreeVar \"" ++ nvSTML ++ "\" state" ++ (show last_state),
	 last_state + 1,
	 ("patPragmaDict" ++ (show (last_state + 1))):patPragmaDicts)

buildWhereFunChangeState (acc, last_state, patPragmaDicts) (lhs, rhs) =
	let 
		rewritenRhs = 
			Txt.unpack (Txt.replace (Txt.pack "state0") (Txt.pack ("state" ++ (show last_state))) (Txt.pack rhs))
		rewritenLhs = 
			Txt.unpack (Txt.replace (Txt.pack "patPragmaDict") (Txt.pack ("patPragmaDict" ++ (show (last_state + 1)))) (Txt.pack lhs))
		npatPragmaDicts = 
			if (rewritenLhs == lhs)
			then patPragmaDicts
			else ("patPragmaDict" ++ (show (last_state + 1))):patPragmaDicts
	in 
		(acc ++ "\n\t\t" ++ "(" ++ rewritenLhs ++ ", state" ++ (show (last_state + 1)) ++ ") = " ++ rewritenRhs, 
		 last_state + 1, 
		 npatPragmaDicts)

---------------------------------------------------------
-- Search generator/pattern blocks
---------------------------------------------------------

getInternalGenFunctions state ast = 
	let 
		(nodes,_) = unzip (applyRulesGeneral (searchGenFunctions state) ast)
	in
		nodes

-- TODO: Maybe it (and all of this) have the same problem and solution than pat_list
searchGenFunctions state (CLabel (Ident "gen_list" _ _) (CCompound _ bi _) _ nI) = 
	[(nI,[(g,"True",(getInternalGenFunctions state g)) | g <- bi])]
-- TODO: pat_list can be placed in a expression pattern; A solution could be to introduce a function where its body is the list of all the possible patterns; e.g. pat_list(pats_assign()) and the body of pats_assign is the possible expressions
searchGenFunctions state (CLabel (Ident "pat_list" _ _) (CCompound _ bi _) _ nI) = 
	[(nI,[(g,"True",(getInternalGenFunctions state g)) | g <- bi])]
-- TODO: This two expression do not work well with "where" section of the rule
searchGenFunctions state (CLabel (Ident "if_then" _ _) (CCompound _ bi _) _ nI) = 
	[(nI,[(bi!!1, (printCond state (bi!!0)), (getInternalGenFunctions state (bi!!1) ))])]
searchGenFunctions state (CLabel (Ident "if_then_else" _ _) (CCompound _ bi _) _ nI) = 
		[(nI,
			[(bi!!1,(printCond state (bi!!0)),getInternalGenFunctions state (bi!!1)), 
			 (bi!!2,("(notCond " ++ (printCond state (bi!!0)) ++ ")" ),getInternalGenFunctions state (bi!!2))])]
searchGenFunctions state other =
	[]

---------------------------------------------------------
-- Create all possible generators/patterns
---------------------------------------------------------

--getAllGenerators :: (Data a,Data b) => PrintState -> a -> [(a,[b])]
getAllGenerators state ast = 
	let
		allGenFunctions = applyRulesGeneral (searchGenFunctions state) ast
		cleanedGenFunctions = cleanGenFunction allGenFunctions allGenFunctions
		allCombinations = combinations cleanedGenFunctions
	in
		case allGenFunctions of 
			[] -> 
				[(ast,[])]
			_ -> 
				createAllGeneratorsForCombs state ast allCombinations

--combinations :: [(NodeInfo,[a])] -> [[(NodeInfo,a)]]
combinations [] = 
	[]
combinations [(nI,opts)] = 
	[[(nI,e)] | e <- opts]
combinations ((nI,opts):t) =
	[((nI,e):comb) | e <- opts, comb <- (combinations t)]

changeASTOpts ast0 ((nI,(g,cond,_)):t) = 
	let
		ast1 = everywhere (mkT (selectOpt (nI,g))) ast0
		(ast2,listCond) = changeASTOpts ast1 t
	in 
		(ast2,cond:listCond)
changeASTOpts ast0 [] = 
	(ast0,[])

createAllGeneratorsForCombs state ast (comb:t) = 
	let
		(nast,conds1) = changeASTOpts ast comb
		gens1_ = getAllGenerators state nast
		gens1 = [(g, condsg ++ conds1) | (g,condsg) <- gens1_]
		gens2 = createAllGeneratorsForCombs state ast t
	in 
		gens1 ++ gens2
createAllGeneratorsForCombs _ _ [] = 
	[]

selectOpt (nISearched,g) ori@(CBlockStmt (CLabel _ _ _ nIFound)) = 
	case (geq nISearched nIFound) of 
		True -> g
		False -> ori
selectOpt _ other = 
	other


cleanGenFunction allGF ((item@(nI,_)):t) = 
	case any id [(any (geq nI) des) |  (_,opts) <- allGF,(_,_,des) <- opts] of 
		True -> 
			cleanGenFunction allGF t
		False -> 
			item:(cleanGenFunction allGF t)
cleanGenFunction _ [] = 
	[]

---------------------------------------------------------
-- Commutative operators support
---------------------------------------------------------

-- Search opeartion of the AST that has the commutative property
searchCommOp :: CExprAnn -> [(NodeAnn,Int)]
searchCommOp (CBinary bop lop rop nodeInfo) = 
	case bop `elem` [CAddOp,CMulOp,CEqOp,CNeqOp,CAndOp,COrOp,CXorOp,CLndOp,CLndOp] of 
		True ->
			[(nodeInfo,0)]
		False -> 
			[]
searchCommOp _ = 
	[]

-- Change the order in the AST of all the operations that are stated in list ops
changeOrderOp ops ast =
	everywhere (mkT (changeOrderOp_ ops)) ast


changeOrderOp_ :: [(NodeAnn,Int)] -> CExprAnn -> CExprAnn
changeOrderOp_ ops ori@(CBinary bop lop rop nodeInfo) = 
	case [nodeInfo_ | (nodeInfo_,0) <- ops, nodeInfo_ == nodeInfo] of 
		[] -> ori
		_ -> (CBinary bop rop lop nodeInfo)
changeOrderOp_ _ other = 
	other


---------------------------------------------------------
-- all & some constructors in patterns support
---------------------------------------------------------

-- Search pattern all
searchAllCons :: CStatAnn -> [[(NodeAnn,Int)]]
searchAllCons  (CLabel (Ident "all" _ _) (CCompound _ bi _) _ nI) = 
	let 
		permBi = permutations bi
	in
		[[(nI, pbi) | pbi <- [0..((length permBi) - 1)] ]]
searchAllCons _ = 
	[]

-- Search pattern some
searchSomeCons (CLabel (Ident "some" _ _) (CCompound _ bi _) _ nI) = 
	[[(nI, pbi) | pbi <- [0..((length bi) - 1)] ]]
searchSomeCons _ = 
	[]

-- Calculate combiantions for all and some constructors
combinationsAllSomeCons :: [[a]] -> [[a]]
combinationsAllSomeCons [] = 
	[[]]
combinationsAllSomeCons [list] = 
	[[e] | e <- list]
combinationsAllSomeCons (list:t) =
	[(e:comb)  | e <- list, comb <- (combinationsAllSomeCons t)]

-- Change the order in the AST of all the pattern inside a all constructor
changeOrderAllCons ops ast =
	everywhere (mkT (changeOrderAllConsBIs ops)) ast

changeOrderAllConsBIs:: [(NodeAnn,Int)] -> [CBlockItemAnn] -> [CBlockItemAnn]
changeOrderAllConsBIs _ [] = 
	[]
changeOrderAllConsBIs ops ((CBlockStmt (CLabel (Ident "all" _ _) (CCompound _ bi _) _ nodeInfo)):rest) = 
	let
		permBi = permutations bi
		fromrest = changeOrderAllConsBIs ops rest
		[selected] = [selected_ | (nodeInfo_,selected_) <- ops, nodeInfo_ == nodeInfo] 
	in 
		(permBi!!selected) ++ fromrest
changeOrderAllConsBIs ops (bi:rest) = 
	let
		fromrest = changeOrderAllConsBIs ops rest
	in 
		(bi:fromrest)

-- Change the order in the AST of all the pattern inside a all constructor
selectOneSomeCons ops ast =
	everywhere (mkT (selectOneSomeConsBIs ops)) ast

selectOneSomeConsBIs:: [(NodeAnn,Int)] -> [CBlockItemAnn] -> [CBlockItemAnn]
selectOneSomeConsBIs _ [] = 
	[]
selectOneSomeConsBIs ops ((CBlockStmt (CLabel (Ident "some" _ _) (CCompound _ bi _) _ nodeInfo)):rest) = 
	let
		fromrest = selectOneSomeConsBIs ops rest
		[selected] = [selected_ | (nodeInfo_,selected_) <- ops, nodeInfo_ == nodeInfo] 
	in 
		((bi!!selected):fromrest)
selectOneSomeConsBIs ops (bi:rest) = 
	let
		fromrest = selectOneSomeConsBIs ops rest
	in 
		(bi:fromrest)


---------------------------------------------------------
-- Unfolding for bin_op
---------------------------------------------------------

binOp = [CMulOp, CDivOp, CRmdOp, CAddOp, CSubOp, CShlOp, CShrOp, CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp, CAndOp, CXorOp, COrOp, CLndOp, CLorOp]

searchBinOp :: CExprAnn -> [(NodeAnn,[(String,CBinaryOp,CExprAnn,CExprAnn)])]
searchBinOp (CCall (CVar (Ident "bin_op" _ _) _) args nI) = 
	let 
		opName = extractVarName (args!!0) 
	in 
		[(nI,[(opName,op,(args!!1),(args!!2)) | op <- binOp])]
searchBinOp other = 
	[]

getAllBinOperators ast =
	let
		allBinOps = applyRulesGeneral searchBinOp ast
		allCombinations = combinations allBinOps
	in 
		case allBinOps of 
			[] -> 
				[(ast,[])]
			_ -> 
				getAllBinOperatorsForCombs ast allCombinations

getAllBinOperatorsForCombs ast (comb:t) = 
	let
		binOp1 = changeASTBinOp ast comb
		binOp2 = getAllBinOperatorsForCombs ast t
	in 
		binOp1:binOp2
getAllBinOperatorsForCombs _ [] = 
	[]

changeASTBinOp ast0 ((nI,(name,op,op1,op2)):t) = 
	let
		ast1 = everywhere (mkT (selectBinOp (nI,(CBinary op op1 op2 nI))) ) ast0
		(ast2,dict) = changeASTBinOp ast1 t
		dictEntry = 
			DictOpsEntry 
			{
				nameOp = name,
				op = op
		    }
	in 
		(ast2,dictEntry:dict)
changeASTBinOp ast0 [] = 
	(ast0,[])

selectBinOp (nISearched,g) ori@(CCall (CVar (Ident "bin_op" _ _) _) _ nIFound) = 
	case (geq nISearched nIFound) of 
		True -> g
		False -> ori
selectBinOp _ other = 
	other
		

---------------------------------------------------------
-- Auxiliary functions
---------------------------------------------------------

-- Generic list printing with a printing function fun and with separtor sep
printList _ _ [] =
	""
printList fun _ [e] = 
	fun e
printList fun sep (e:es) = 
	(fun e) ++ sep ++ (printList fun sep es)

-- To print a Maybe expression
printMaybe fun e = 
	case e of 
		Nothing -> "Nothing"
		Just e_ -> "(Just " ++ (fun e_) ++ ")"

-- To print a Maybe expression with a function carrying a state
printMaybeState fun state0 e = 
	case e of 
		Nothing -> ("Nothing",state0)
		Just e_ -> 
			let (se,state1) = (fun state0 e_)
			in ("(Just " ++ se ++ ")",state1)

-- Get the name from a call
extractVarName ::  CExprAnn -> String 
extractVarName (CCall (CVar (Ident name_fun _ _) _) args _) = 
	case name_fun `elem` ["cstmts","cstmt","cexpr","cop"] of 
		True -> extractVarName (args!!0)
		False -> ""
extractVarName (CVar (Ident name_var _ _) _) = 
	name_var
extractVarName other = 
	error ("Not possible to extract a name from: " ++ (prettyMyAST (fmap (\(Ann nI _) -> nI) other)) ++ "\n" ++(show (fmap (\(Ann nI _) -> nI) other)))
	--error ("Not possible to extract a name from: " ++ (show other) )
	
--extractVarName other = 
--	trace ("\n\n\n\n" ++ (show other) ++"\n\n\n\n") ""

-- Search the name given to a block of statements in the dictionary
searchCompoundName nInfo (state@PrintState{dictInternals = dict}) =
	head [(name entry) | entry <- dict, (node entry) == nInfo ]

-- Search the name of the function in charge to match the patterns of a block of statements
searchRuleName nInfo (state@PrintState{dictFunInternals = dict}) =
	head [(nameFun entry) | entry <- dict, (nodeFun entry) == nInfo ]

-- Builds a var name
buildVarName varRuleName free = 
	"var_" ++ varRuleName ++ "_"  ++ (show free)

-- Builds a internal var name
buildVarIntName free = 
	"var_internal_"  ++ (show free)

-- Given a STML var name returns some of its Haskell names
findNameHas name dictVar = 
	case [head (namesHas entryDict) | entryDict  <- dictVar, (nameSTML entryDict) == name ] of 
		[] ->
			Nothing 
		list ->
			Just (head list)

-- Given a STML var name returns its type
findType name dictVar = 
	case [(varType entryDict) | entryDict  <- dictVar, (nameSTML entryDict) ==name]  of 
		[] ->
			Nothing 
		list ->
			Just (head list)

-- extract the Haskell name from the argument of the call to cexpr, cstmt...
extractNameHas dict arg = 
	case 
		--trace (show (extractVarName arg)) $ 
		findNameHas (extractVarName arg) dict of 
		Just name -> name

-- extract the Haskell name from the argument of the call to cexpr, cstmt... 
-- If it is not found, returns the expression printed
tryExtractNameHas state0@PrintState{dictVars = dict} arg = 
	case findNameHas (extractVarName arg) dict of 
		Nothing -> 
			printExprGen state0 arg
		Just other ->
			(other, state0)

tryExtractType state0@PrintState{dictVars = dict} arg = 
	case findType (extractVarName arg) dict of 
		Nothing -> 
			"none"
		Just other ->
			other

-- extract the type from the argument of the call to cexpr, cstmt...
extractType dict arg = 
	findType (extractVarName (arg)) dict

-- Extract the statement from a block
extractStmt (CBlockStmt s) =
	s

-- Print a expression as a statement
printExprAsStmt se strNodeInfo = 
	"(CExpr " ++ se ++ " " ++ strNodeInfo ++ ")"	

-- Build a cstmts var 
buildVarCStmts name id = 
	(CBlockStmt 
		(CExpr (Just
		 	(CCall 
				(CVar (Ident "cstmts" id undefNode) undefNodeAnn) 
				[(CVar (Ident name id undefNode) undefNodeAnn)] 
				undefNodeAnn)) 
		undefNodeAnn) 
	)

-- True if the given ast is correspontds to a cstmt
isCstmtVar (CBlockStmt (CExpr (Just (CCall (CVar (Ident "cstmt" _ _) _) _ _)) _)) = 
	True
isCstmtVar _ = 
	False

-- True if the given ast is correspontds to a cstmts
isCstmtsVar (CBlockStmt (CExpr (Just (CCall (CVar (Ident "cstmts" _ _) _) _ _)) _)) = 
	True
isCstmtsVar _ = 
	False

-- Inserts an entry in the variables dictionary
insertInDictVar dictVar entry = 
	case [entryDict | entryDict  <- dictVar, (nameSTML entryDict) == (nameSTML entry)] of 
		[] -> 
			(entry:dictVar)
		_ -> 
		  [entryDict{namesHas = ((namesHas entry) ++ (namesHas entryDict))} 
		   | entryDict  <- dictVar, (nameSTML entryDict) == (nameSTML entry)] 
		  ++ [entryDict | entryDict  <- dictVar, (nameSTML entryDict) /= (nameSTML entry)] 

-- Inserts an entry in the internal variables dictionary
insertInDictInternals dictInternals entry = 
	case [entryDict | entryDict  <- dictInternals, (node entryDict) == (node entry)] of 
		[] -> 
			(entry:dictInternals)
		_ ->
			(entry:[entryDict | entryDict  <- dictInternals, (node entryDict) /= (node entry)])

-- Build a case expression when there are substition in the consquent
-- This is done to avoid fire the rule when the code where the substitution is performed does not change at all
buildCaseSubs subs defaultBody = 
	case subs of 
		[] -> 
			defaultBody
		_ -> 
			"case " ++ (buildCondSubs subs) ++ " of\n\t\t"
			++ "True -> []\n\t\t" 
			++ "False -> " ++ defaultBody

-- Build the conditions for a list of substitutions
buildCondSubs ([]) = 
	"True"
buildCondSubs ([(where_,from,to)]) = 
	"(geq (substitute " ++ where_ ++ " " ++ from ++ " " ++ to ++ ") " ++ where_ ++ " )" 
buildCondSubs ((where_,from,to):tail_) = 
	"(geq (substitute " ++ where_ ++ " " ++ from ++ " " ++ to ++ ") " ++ where_ ++ " ) && " 
	++ buildCondSubs (tail_) 

-- Function that print a pattern with identifier in case some pragmas are associated with the corresponding statement, storing its name 
--printNodePat state Nothing = 
--	("_", state)
printNodePat state@PrintState{
		currentFree = freeVar0, 
		pragmaNode = pragmas, 
		condForNode = condsNodes,
		funsChangingState = currentFCS} 
	(Ann nI nP) = 
	case (extractPolcaPragmas nP) of 
		[] ->
			("_", state)
		_ ->
			let 
				nodeStr = buildVarName "node" freeVar0
				varCond = buildVarName "cond" (freeVar0 + 1)
			in
				(nodeStr, 
				state
				{
					currentFree = freeVar0 + 2,
					condForNode = (varCond):condsNodes,
					funsChangingState = currentFCS ++ [("(" ++ varCond ++ ", patPragmaDict)", "(checkPragmas state0 (" ++ (show nP) ++ ") " ++ nodeStr ++ ")")]
				})

--printNodePat state Nothing = 
--	("_", state)
--printNodePat state@PrintState{
--		currentFree = freeVar0, 
--		pragmaNode = pragmas, 
--		condForNode = condsNodes} 
--	(Just name) = 
--	case [pragma | (name1,pragma)  <- pragmas, name1 == name] of 
--		[] ->
--			("_", state)
--		_ ->
--			let 
--				nodeStr = buildVarName "node" freeVar0
--			in
--				(nodeStr, 
--				state
--				{
--					currentFree = freeVar0 + 1,
--					condForNode = ((nodeStr, name):condsNodes)
--				})

-- Function that print a pattern with identifier in case some pragmas are associated with the corresponding statement, storing its name 
--printNodeGen state Nothing = 
--	("undefNodeAnn", state)
printNodeGen state@PrintState{
		currentFree = freeVar0, 
		pragmaNode = pragmas, 
		funsChangingState = currentFCS} 
	(Ann nI nP) = 
	case (extractPolcaPragmas nP) of 
		[] ->
			("undefNodeAnn", state)
		_ ->
			let 
				nodeStr = buildVarName "node" freeVar0
			in
				(nodeStr, 
				state
				{
					currentFree = freeVar0 + 1,
					--funsChangingState = currentFCS ++ [(nodeStr, "(processPragmas state0 pragmasRules patPragmaDict (" ++ (show name) ++ ") old)")]
					funsChangingState = currentFCS ++ [(nodeStr, "(processPragmas state0 patPragmaDict (" ++ (show nP) ++ ") old)")]
				})

-- TODO
-- Link with pragmas (conditions,etc...)
-- Allow to expres that certain expression is inside a statement or statments
-- Rules for the whole program (e.g. void i(){...} main(){...i();...} )

-- TODO NEW:
-- Implement all the cases of exprEqual function in RulesLib.hs
-- Implement all the cases for printing patterns and generators
-- Allow to define a comparation between expressions or statements (to check equailty or inequality)
-- Syntactic sugar to define that a variable is not modified in the whole pattern
-- Allow to introduce free (i.e. unique) variable names in the generation
-- Add conditions for operations (in the same way that it is check that each variable with the same name is actually the same)

-- TODO OTHER:
-- Think how to define sequences of rules in the search tree and give a simple example
-- Small example for each rule

