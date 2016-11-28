-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

-- {-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, DeriveGeneric, DeriveDataTypeable, TemplateHaskell, FlexibleContexts  #-}

module RulesLib where

import System.IO  
import System.IO.Unsafe

import Language.C
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Data.Node

import Data.Generics
import Data.Data
import Data.Char (isSpace)
import Data.Binary
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Set (Set)
import Data.String.Utils
--import Data.DeriveTH
--import Data.Dynamic

import Control.Exception
import Control.DeepSeq
import Control.Lens


import GHC.Generics as GHCG

--import Text.PrettyPrint.HughesPJ

import Debug.Trace

-- 

type NodeAnn = Annotation NodeInfo NodeProperties
data Annotation nI nP = Ann nI nP
	deriving (Show, Data, Typeable, Eq)

data PragmaType = 
	STML | POLCA | NONE
	deriving (Show, Data, Typeable, Eq)


printPragmaType STML = "stml"
printPragmaType POLCA = "polca"
printPragmaType NONE = "none"

data DefinerType = 
	USER | CETUS
	deriving (Show, Data, Typeable, Eq)


printWithoutQuoutes :: String -> String
printWithoutQuoutes s = 
	sq (show s)

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
	    | otherwise	     = s
sq ('\'':s) | last s == '\'' = init s
	    | otherwise	     = s
sq s                         = s

data PropertyInfo a = 
	PropertyInfo
	{ 
		_pragmaType :: PragmaType,
		_definedBy  :: DefinerType,
		_value :: Maybe a
	} 
	deriving (Show, Data, Typeable, Eq)

propertyInfoDefault = 
	PropertyInfo
	{ 
		_pragmaType = STML,
		_definedBy = CETUS,
		_value = Nothing
	}

data NodeProperties = 
	NodeProperties
	{ 
		_term_position :: String,
		_hasSideEffects :: PropertyInfo Bool,
		_readIn ::  PropertyInfo [String],
		_writeIn ::  PropertyInfo [String],
		_localSymbols :: PropertyInfo [String],
		_rangeInfo :: PropertyInfo [String],
		_isCanonical :: PropertyInfo Bool,
		_isPerfectNest :: PropertyInfo Bool,
		_hasLoops :: PropertyInfo Bool,
		_hasFunctionCalls :: PropertyInfo Bool,
		_hasControlFlowModifiers :: PropertyInfo Bool,
		_scalarDependences :: PropertyInfo [String],
		_polcaPragmas :: PropertyInfo [[String]],
		_allPragmas :: PropertyInfo [String]
	} 
	deriving (Show, Data, Typeable, Eq)



nodePropertiesDefault = 
	NodeProperties 
	{ 
		_term_position = "",
		_hasSideEffects = propertyInfoDefault,
		_readIn =  propertyInfoDefault,
		_writeIn =  propertyInfoDefault,
		_localSymbols =  propertyInfoDefault,
		_rangeInfo =  propertyInfoDefault,
		_isCanonical = propertyInfoDefault,
		_isPerfectNest = propertyInfoDefault,
		_hasLoops = propertyInfoDefault,
		_hasFunctionCalls = propertyInfoDefault,
		_hasControlFlowModifiers = propertyInfoDefault,
		_scalarDependences = propertyInfoDefault,
		_polcaPragmas =  PropertyInfo{_value = (Just []), _pragmaType = POLCA, _definedBy = USER},
		_allPragmas = propertyInfoDefault{_pragmaType = NONE}
	}

makeLenses ''NodeProperties
makeLenses ''PropertyInfo



addPragmaType ann = 
	let typeAnn = 
		case head (split " " ann) of 
			"input" ->
				"stml"
			"output" ->
				"stml"
			"iteration_independent" ->
				"stml"
			_ ->
				"polca"
	in 
		typeAnn ++ " " ++ ann


--instance Show NodeProperties where
prettyPragma nodeProperties = 
	let 
		-- Allow include definers as a previous comment
		printListStrings l = 
			"[" ++ (join ", " l) ++ "]"
		printListStringsPolca l = 
			case l of 
				[] ->
					""
				_ ->
						"#pragma " 
					++ 	(join "\n#pragma " $ map addPragmaType l) 
					++ 	"\n"
		propertyStr prop printFun strProp = 
			case nodeProperties^.prop^.value of 
				Nothing ->
					""
				(Just value) ->
					case strProp of 
						"allPragmas" ->
							(printFun value)
						_ ->	
							"#pragma " ++ (printPragmaType (nodeProperties^.prop^.pragmaType))
							++ " " ++  strProp 
							++ " " ++ (printFun value) ++ "\n"

	in 
			(propertyStr hasSideEffects show "side_effects")
		++ 	(propertyStr readIn printListStrings "reads")
		++ 	(propertyStr writeIn printListStrings "writes")
		++ 	(propertyStr localSymbols printListStrings "local_symbols")
		++ 	(propertyStr rangeInfo printListStrings "range_info")
		++ 	(propertyStr isCanonical show "is_canonical")
		++ 	(propertyStr hasLoops show "has_loops")
		++ 	(propertyStr hasFunctionCalls show "has_function_calls")
		++ 	(propertyStr isPerfectNest show "is_perfect_nest")
		++ 	(propertyStr scalarDependences show "scalar_dependences")
		-- ++  (propertyStr allPragmas printListStringsPolca "allPragmas")
		++ (prettyPragmaPolca nodeProperties)

prettyPragmaPolca nodeProperties = 
	let 
		propertyStr prop = 
			case nodeProperties^.prop^.value of 
				Nothing ->
					""
				(Just []) ->	
					""
				(Just value) ->	
					(join "\n"
						[	"#pragma " ++ (printPragmaType (nodeProperties^.prop^.pragmaType))
							++ " " ++ (join " " l) 
						| l <- value]) 
					++ "\n" 

	in 
		(propertyStr polcaPragmas)

extractPolcaPragmas properties =
	let 
		extractPolcaPragmasProperties properties = 
			case properties^.polcaPragmas.value of 
				Nothing -> 
					[]
				Just pp ->
					pp
		--unplain = (map.map) (\(PlainString s) -> s)
		unplain = id
	in 
		unplain (extractPolcaPragmasProperties properties)

type CTypeSpecAnn = CTypeSpecifier NodeAnn 
type CDeclAnn = CDeclaration NodeAnn
type CDeclSpecAnn = CDeclarationSpecifier NodeAnn
type CDeclrAnn = CDeclarator NodeAnn
type CStatAnn = CStatement NodeAnn
type CExtDeclAnn = CExternalDeclaration NodeAnn
type CExprAnn = CExpression NodeAnn
type CBlockItemAnn = CCompoundBlockItem NodeAnn
type CTranslUnitAnn = CTranslationUnit NodeAnn



undefNodeAnn = Ann undefNode nodePropertiesDefault

data TransState = 
	TransState 
	{
		free_node_id :: Int,
		-- pragmas :: [(Name, String)],
		-- current_ast :: CTranslUnit,
		freeVar :: Int,
		includes :: [String],
		fun_defs :: [(String, (CTypeSpecAnn, [CDeclAnn], ([CDeclSpecAnn], CDeclrAnn, [CDeclAnn], NodeAnn)), CStatAnn)],
		no_fun_defs :: [CExtDeclAnn],
		last_changed :: String,
		previous_changes :: ([(String, ((String,CStatAnn,CStatAnn), TransState, [(String, CStatAnn)]))], [(String, ((String,CExprAnn,CExprAnn), TransState, [(String, CStatAnn)]))]),
		applied_rules :: [String],
		applicable_rules :: Set String,
		trans_with_anns :: Bool,
		ast_to_transform :: Maybe (String, CStatAnn, CStatAnn),
		print_id :: Int,
		seq_id :: Int,
		acc_steps :: [(String, Int)],
		oracle :: String,
		nameFile :: String
    }
    deriving (Show, Data, Typeable)


-- TODO: rewrite instances so they have sense
instance Eq TransState where
  (TransState f1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == (TransState f2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = f1 == f2

instance Ord TransState where
  (TransState f1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` (TransState f2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = f1 `compare` f2


getAnnotation (CLabel _ _ _ (Ann _ np)) = np 
getAnnotation (CCase _ _ (Ann _ np)) = np
getAnnotation (CCases _ _ _ (Ann _ np)) = np
getAnnotation (CDefault _ (Ann _ np)) = np
getAnnotation (CExpr _ (Ann _ np)) = np
getAnnotation (CCompound _ _ (Ann _ np)) = np
getAnnotation (CIf _ _ _ (Ann _ np)) = np
getAnnotation (CSwitch _ _ (Ann _ np)) = np
getAnnotation (CWhile _ _ _ (Ann _ np)) = np
getAnnotation (CFor _ _ _ _ (Ann _ np)) = np
getAnnotation (CGoto _ (Ann _ np)) = np
getAnnotation (CGotoPtr _ (Ann _ np)) = np
getAnnotation (CCont (Ann _ np)) = np
getAnnotation (CBreak (Ann _ np)) = np
getAnnotation (CReturn _ (Ann _ np)) = np
getAnnotation (CAsm _ (Ann _ np)) = np

setAnnotation nnp (CLabel a b c (Ann ni _)) = (CLabel a b c (Ann ni nnp)) 
setAnnotation nnp (CCase a b (Ann ni _)) = (CCase a b (Ann ni nnp))
setAnnotation nnp (CCases a b c (Ann ni _)) = (CCases a b c (Ann ni nnp))
setAnnotation nnp (CDefault a (Ann ni _)) = (CDefault a (Ann ni nnp))
setAnnotation nnp (CExpr a (Ann ni _)) = (CExpr a (Ann ni nnp))
setAnnotation nnp (CCompound a b (Ann ni _)) = (CCompound a b (Ann ni nnp))
setAnnotation nnp (CIf a b c (Ann ni _)) = (CIf a b c (Ann ni nnp))
setAnnotation nnp (CSwitch a b (Ann ni _)) = (CSwitch a b (Ann ni nnp))
setAnnotation nnp (CWhile a b c (Ann ni _)) = (CWhile a b c (Ann ni nnp))
setAnnotation nnp (CFor a b c d (Ann ni _)) = (CFor a b c d (Ann ni nnp))
setAnnotation nnp (CGoto a (Ann ni _)) = (CGoto a (Ann ni nnp))
setAnnotation nnp (CGotoPtr a (Ann ni _)) = (CGotoPtr a (Ann ni nnp))
setAnnotation nnp (CCont (Ann ni _)) = (CCont (Ann ni nnp))
setAnnotation nnp (CBreak (Ann ni _)) = (CBreak (Ann ni nnp))
setAnnotation nnp (CReturn a (Ann ni _)) = (CReturn a (Ann ni nnp))
setAnnotation nnp (CAsm a (Ann ni _)) = (CAsm a (Ann ni nnp))

nameOfNodeAnn (Ann ni _) = 
	nameOfNode ni

posOfNodeAnn (Ann ni _) = 
	posOfNode ni


deriving instance GHCG.Generic (CTypeQualifier a)
deriving instance Binary a => Binary (CTypeQualifier a)
deriving instance GHCG.Generic (CStringLiteral a)
deriving instance Binary a => Binary (CStringLiteral a)
deriving instance GHCG.Generic CBinaryOp
deriving instance Binary CBinaryOp
deriving instance GHCG.Generic CUnaryOp
deriving instance Binary CUnaryOp
deriving instance GHCG.Generic (CFunctionDef a)
deriving instance Binary a => Binary (CFunctionDef a)
deriving instance GHCG.Generic PragmaType
deriving instance Binary PragmaType

deriving instance GHCG.Generic (CAttribute a)
deriving instance Binary a => Binary (CAttribute a)
deriving instance GHCG.Generic CString
deriving instance Binary CString
deriving instance GHCG.Generic (CPartDesignator a)
deriving instance Binary a => Binary (CPartDesignator a)
deriving instance GHCG.Generic NodeInfo
deriving instance Binary NodeInfo
deriving instance GHCG.Generic (CStructureUnion a)
deriving instance Binary a => Binary (CStructureUnion a)
deriving instance GHCG.Generic (CStorageSpecifier a)
deriving instance Binary a => Binary (CStorageSpecifier a)
deriving instance GHCG.Generic (CDerivedDeclarator a)
deriving instance Binary a => Binary (CDerivedDeclarator a)
deriving instance GHCG.Generic DefinerType
deriving instance Binary DefinerType
--deriving instance GHCG.Generic Position
--deriving instance Binary Position
instance Binary Position where
    put p | isNoPos p = putWord8 0 
          | isBuiltinPos p = putWord8 1
          | isInternalPos p = putWord8 2
          | isSourcePos p = putWord8 3 >> put (posOffset p) >> put (posFile p) >> put (posRow p) >> put (posColumn p)

    get = do
        marker <- getWord8
        case marker of
            0 -> return nopos
            1 -> return builtinPos
            2 -> return internalPos
            3 -> position <$> get <*> get <*> get <*> get
deriving instance GHCG.Generic CStructTag
deriving instance Binary CStructTag
deriving instance GHCG.Generic (CArraySize a)
deriving instance Binary a => Binary (CArraySize a)
deriving instance GHCG.Generic (CEnumeration a)
deriving instance Binary a => Binary (CEnumeration a)
deriving instance GHCG.Generic (CAssemblyStatement a)
deriving instance Binary a => Binary (CAssemblyStatement a)
deriving instance GHCG.Generic (CAssemblyOperand a)
deriving instance Binary a => Binary (CAssemblyOperand a)
deriving instance GHCG.Generic (CConstant a)
deriving instance Binary a => Binary (CConstant a)
deriving instance GHCG.Generic CAssignOp
deriving instance Binary CAssignOp
deriving instance GHCG.Generic CInteger
deriving instance Binary CInteger
deriving instance GHCG.Generic CFloat
deriving instance Binary CFloat
deriving instance GHCG.Generic (CBuiltinThing a)
deriving instance Binary a => Binary (CBuiltinThing a)

deriving instance GHCG.Generic (CInitializer a)
deriving instance Binary a => Binary (CInitializer a)
deriving instance GHCG.Generic Ident
deriving instance Binary Ident
deriving instance GHCG.Generic (CDeclaration a)
deriving instance Binary a => Binary (CDeclaration a)
deriving instance GHCG.Generic (CTypeSpecifier a)
deriving instance Binary a => Binary (CTypeSpecifier a)
deriving instance GHCG.Generic (CDeclarationSpecifier a)
deriving instance Binary a => Binary (CDeclarationSpecifier a)
deriving instance GHCG.Generic (CDeclarator a)
deriving instance Binary a => Binary (CDeclarator a)
deriving instance GHCG.Generic (CStatement a)
deriving instance Binary a => Binary (CStatement a)
deriving instance GHCG.Generic (CExternalDeclaration a)
deriving instance Binary a => Binary (CExternalDeclaration a)
deriving instance GHCG.Generic (CExpression a)
deriving instance Binary a => Binary (CExpression a)
deriving instance GHCG.Generic (CCompoundBlockItem a)
deriving instance Binary a => Binary (CCompoundBlockItem a)
deriving instance GHCG.Generic (CTranslationUnit a)
deriving instance Binary a => Binary (CTranslationUnit a)
deriving instance GHCG.Generic (Flags a)
deriving instance Binary a => Binary (Flags a)
deriving instance GHCG.Generic CIntFlag
deriving instance Binary CIntFlag
deriving instance GHCG.Generic CIntRepr
deriving instance Binary CIntRepr
deriving instance GHCG.Generic CChar
deriving instance Binary CChar
deriving instance GHCG.Generic Name
deriving instance Binary Name

deriving instance GHCG.Generic TransState
deriving instance Binary TransState
deriving instance GHCG.Generic (Annotation a b)
deriving instance (Binary a, Binary b) => Binary (Annotation a b)
deriving instance GHCG.Generic NodeProperties
deriving instance Binary NodeProperties
deriving instance GHCG.Generic (PropertyInfo a)
deriving instance Binary a => Binary (PropertyInfo a)


--applyRulesGeneral :: Data a => (a -> [(String,a,a)]) -> CTranslUnit -> [(String,a,a)]
applyRulesGeneral fun ast =
	 (everything (++) ( [] `mkQ` fun) ast)

prettyMyAST ctu = show (pretty ctu)

prettyMyASTAnn ctu = prettyMyAST (fmap (\(Ann nI _) -> nI) ctu)

prettyMyASTIncludes ctu = show (prettyUsingInclude ctu)

intConstant int =  (CConst (CIntConst (cInteger int) undefNodeAnn))

--replace expr@(CVar (Ident iter _ _) _) iter' nite 
--	| iter == iter' = CBinary CAddOp (CBinary CMulOp nite (intConstant size_chunk) undefNode) expr undefNode
--	| otherwise = expr
--replace expr _ _ = expr 

size_chunk = 3

incName (Name {nameId = id}) =
	 (Name {nameId = (id + 1)}) 

rebuildAst state = 
	CTranslUnit ((no_fun_defs state) ++ (map rebuildFunDefs (fun_defs state))) undefNodeAnn
	--CTranslUnit [] undefNode

exprEqual e1 e2 = 
	case e1 of 
		(CVar (Ident name1 _ _) _) ->
			case e2 of 
				(CVar (Ident name2 _ _) _) ->
					name1 == name2 
				_ -> 
					False
		(CConst (CIntConst num1 _)) -> 
			case e2 of 
				(CConst (CIntConst num2 _))  ->
					num1 == num2 
				_ -> 
					False
		(CBinary op1 e1_1 e1_2 _) ->
			case e2 of 
				(CBinary op2 e2_1 e2_2 _)  ->
					op1 == op2 &&  (exprEqual e1_1 e2_1) &&  (exprEqual e1_2 e2_2)
				_ -> 
					False
		(CIndex e1_1 e1_2 _) ->
			case e2 of 
				(CIndex e2_1 e2_2 _)  ->
					(exprEqual e1_1 e2_1) &&  (exprEqual e1_2 e2_2)
				_ -> 
					False
		_ ->
			False


changeASTFun (fun, old, new) state =
	[
		case fun_state == fun of 
			True -> 
				(fun_state,info_fun,changeAST (old, new) body );
			False -> 
				item 
	 | item@(fun_state,info_fun,body) <- (fun_defs state)] 
	-- ++ [item | item@(fun_state,_,_) <- (fun_defs state), fun_state /= fun ]

changeAST (old, new) =
	everywhere (mkT (changeAST_ (old, new)))

--changeAST_ :: (CStat,CStat) -> CStat -> CStat
changeAST_ (pre, new) stmt
	| geq pre stmt = 
		-- trace ((show pre) ++ " " ++ (show stmt) ++ " " ++ (show new)) new
		new
	| otherwise =
		stmt

substitute ::  (Data b) => b -> CExprAnn -> CExprAnn -> b
substitute ast expr_searched expr_substitution =
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral (substituteExpr expr_searched expr_substitution) ast)
	in 
		--trace (show substitutions) 
		(foldl (\current_ast change -> changeAST change current_ast) 
			ast substitutions)

substituteExpr expr_searched expr_substitution expr_found
	| (exprEqual expr_found expr_searched) =
	[(expr_found,expr_substitution)]
substituteExpr _ _ _ = []


substituteInPragmas ::  (Data b) => b -> CExprAnn -> CExprAnn -> b
substituteInPragmas ast expr_searched expr_substitution =
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral (substituteInPragmasExpr expr_searched expr_substitution) ast)
	in 
		--trace (show substitutions) 
		(foldl (\current_ast change -> changeAST change current_ast) 
			ast substitutions)

substituteInPragmasExpr :: CExprAnn -> CExprAnn -> CStatAnn -> [(CStatAnn, CStatAnn)]
substituteInPragmasExpr (CVar (Ident v1 _ _) _) new stmt = 
	let  
		prop = getAnnotation stmt
	in 
		case (prop^.polcaPragmas.value, prop^.allPragmas.value) of 
			(Just pp, _) ->
				let 
					npp = changeInAnnPolcaPragmas v1 (prettyMyASTAnn new) pp
					-- nap = changeInAnnSinglePragma v1 (prettyMyASTAnn new) ap
					-- nprop = (allPragmas.value .~ (Just nap)) $ (polcaPragmas.value .~ (Just npp)) prop
					nprop = (polcaPragmas.value .~ (Just npp)) prop
				in 
					[(stmt, setAnnotation nprop stmt)]
			(Nothing, _) -> 
				[]
substituteInPragmasExpr _ _ _ = 
	[]

changeInAnnPolcaPragmas old new pp = 
	[changeInAnnSinglePragma old new lp | lp <- pp]

changeInAnnSinglePragma old new pp =
	[	
	let 
		(sWO, sAccess) = woArrayAccess (trim s)
	in 
		-- trace ((trim old)++ " " ++ sWO) $ 
		case (trim old) == sWO of 
			True ->
				new ++ sAccess
			False -> 
				case length sAccess > 2 of 
					True -> 
						case (trim old) == (trim $ tail $ init $ sAccess) of 
							True -> 
								sWO ++ "[" ++ new ++ "]"
							False -> 
								s
					False ->
						s
	| s <- pp]

woArrayAccess str = 
	removeArrayAccess [] $ reverse str 

removeArrayAccess prev ('[':rest) = 
	(reverse rest, ('[':prev))
removeArrayAccess prev (other:rest) = 
	removeArrayAccess (other:prev) rest
removeArrayAccess prev [] = 
	(prev,[]) 





changeAccessFlatten ::  (Data b) => b -> CExprAnn -> (Int, CExprAnn) -> b
-- TODO: This kind of functions (very repeated) should be generalized
changeAccessFlatten stmts (CVar v _) (0, limit) = 
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral (substituteAccessFlatten v limit) stmts)
	in 
		--trace (show substitutions) 
		(foldl (\current_ast change -> changeAST change current_ast) 
			stmts substitutions)	
	

substituteAccessFlatten :: Ident -> CExprAnn -> CExprAnn -> [(CExprAnn,CExprAnn)]
substituteAccessFlatten (Ident v1 _ _) limit expr_found@(CIndex (CIndex identv@(CVar (Ident v2 _ _) _) i1 _) i2 nI2) | v1 == v2 =
	[(expr_found,(CIndex identv (CBinary CAddOp (CBinary CMulOp i1 limit undefNodeAnn) i2 undefNodeAnn) nI2))]
substituteAccessFlatten _ _ _ = []

changeAccessAdd ::  (Data b) => b -> CExprAnn -> b
changeAccessAdd stmts to_add = 
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral (substituteAccessAdd to_add) stmts)
	in 
		--trace (show substitutions) 
		(foldl (\current_ast change -> changeAST change current_ast) 
			stmts substitutions)	

substituteAccessAdd :: CExprAnn -> CExprAnn -> [(CExprAnn,CExprAnn)]
substituteAccessAdd expr_add expr_found@(CIndex v i nI) =
	[(expr_found,(CIndex v (CBinary CAddOp i expr_add undefNodeAnn) nI))]
substituteAccessAdd _ _ = []



changeArray :: [CBlockItemAnn] -> CExprAnn -> CExprAnn -> CExprAnn -> CExprAnn -> [CBlockItemAnn]
changeArray stmts v nv m1 m2 = 
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral (substituteArray v nv m1 m2) stmts)
	in 
		--trace (show substitutions) 
		(foldl (\current_ast change -> changeAST change current_ast) 
			stmts substitutions)

substituteArray :: CExprAnn -> CExprAnn -> CExprAnn -> CExprAnn -> CExprAnn -> [(CExprAnn,CExprAnn)]
substituteArray (CVar (Ident v1 _ _) _) varnv@(CVar (Ident nv _ _) _) m1 m2 expr_found@(CIndex (CIndex (CVar (Ident v2 _ _) _) i1 nI1) i2 nI2) | v1 == v2 =
	[(expr_found,(CIndex (CIndex varnv (CBinary CAddOp m1 i1 undefNodeAnn) nI1) (CBinary CAddOp m2 i2 undefNodeAnn) nI2))]
substituteArray _ _ _ _ _ = []

--uses :: Data a => CExpr -> [a] -> [CExpr]
useExp :: CExprAnn -> [CBlockItemAnn] -> [CExprAnn]
useExp expr_searched stmts = 
	let nexpr = everywhere (mkT removeLHS) (CCompound [] stmts undefNodeAnn)
	in applyRulesGeneral (searchExpr expr_searched) nexpr

removeLHS :: CStatAnn -> CStatAnn
removeLHS e@(CExpr (Just (CAssign _ _ val _)) _) =
	--trace ("RLHS: " ++ (prettyMyASTAnn e))
	(CExpr (Just val) undefNodeAnn)
removeLHS other =
	other

getLHS :: CStatAnn -> CExprAnn
getLHS (CExpr (Just (CAssign _ lhs _ _)) _) =
	lhs
getLHS _ =
	(intConstant 0)

getRHS :: CStatAnn -> CExprAnn
getRHS (CExpr (Just (CAssign _ _ rhs _)) _) =
	rhs
getRHS _ =
	(intConstant 0)


searchExpr expr_searched expr_found 
	| (exprEqual expr_found expr_searched) =
		[expr_found]
searchExpr _ _ = 
	[]

allDefs :: [CBlockItemAnn] -> [CExprAnn]
allDefs stmts = 
	--nodeProperties^.prop^.value
	--case stmts
	(applyRulesGeneral searchAssignGeneral stmts)
	++ 
	(applyRulesGeneral searchAssignGeneralDecl stmts)


searchAssignGeneral (CAssign _ v _ _)  =
		[v]
searchAssignGeneral (CUnary op v _) 
	| (elem op [CPreIncOp,CPreDecOp,CPostIncOp,CPostDecOp]) =
		[v]
	| otherwise = 
		[]
searchAssignGeneral _ = 
	[]

	 

searchAssignGeneralDecl (CDeclr (Just ide) _ _ _ nI)  =
	--trace (show v) [v]
	[(CVar ide nI)]
searchAssignGeneralDecl _  =
	[]


defs :: CExprAnn -> [CBlockItemAnn] -> [CExprAnn]
defs expr_searched stmts = 
	applyRulesGeneral (searchAssign expr_searched) stmts


searchAssign expr_searched (CAssign _ v _ _) 
	| (exprEqual v expr_searched) =
		[v]
	| otherwise = 
		[]
searchAssign expr_searched (CUnary op v _) 
	| (exprEqual v expr_searched) && (elem op [CPreIncOp,CPreDecOp,CPostIncOp,CPostDecOp]) =
		[v]
	| otherwise = 
		[]
searchAssign _ _ = 
	[]

--isConstant :: CExprAnn -> Bool
isConstant expr = 
	(null (applyRulesGeneral searchVars expr), [])

isVariable expr = 
	(not $ null $ searchVars expr, [])

searchVars :: CExprAnn ->[CExprAnn]
searchVars v@(CVar _ _) =
		[v]
searchVars _ = 
	[]

--isBlock:: CStatAnn -> Bool
isBlock (CCompound _ _ _) = 
	(True, [])
isBlock _ = 
	(False, [])

--isIdenity :: CBinaryOp -> CExprAnn -> Bool
isIdenity CMulOp (CConst (CIntConst v _))
	| (getCInteger v) == 1 = 
		(True, [])
isIdenity CDivOp (CConst (CIntConst v _)) 
	| (getCInteger v) == 1 = 
		(True, [])
isIdenity CAddOp (CConst (CIntConst v _))
	| (getCInteger v) == 0 = 
		(True, [])
isIdenity CSubOp (CConst (CIntConst v _)) 
	| (getCInteger v) == 0 = 
		(True, [])
isIdenity CShlOp (CConst (CIntConst v _)) 
	| (getCInteger v) == 0 = 
		(True, [])
isIdenity CShrOp (CConst (CIntConst v _)) 
	| (getCInteger v) == 0 = 
		(True, [])
isIdenity _ _ = 
	(False, [])

--nuller list = 
--	(null list, [])

--fake1 v = 
--	(True, [("hasSideEffects", v)])

--fake2 v = 
--	(True, [("hasLoops", v)])

noWrites v es = (null $ defs v es, [])

writesNothing es = (null $ allDefs es, [])

noReads v es = (null $ useExp v es, [])

readsExp v es = (not $ null $ useExp v es, [])

notCond (c, u) = 
	(not c, u)


isExpr (CExpr (Just (CCall _ _ _)) _) = (False, [])
isExpr (CExpr _ _) = (True, [])
isExpr _ = (False, [])

isCall (CCall _ _ _) = (True, [])
isCall _ = (False, [])

noReadInWritten sts1 sts2 = 
	let 
		noReadFun = 
			(\d -> null $ useExp d sts2) 
		written = 
			(allDefs sts1)
		noReadInWritten = 
			map noReadFun written
				--(trace ( show $ map (\x -> (prettyMyASTAnn x)) written) written)
	in
		(all (\x -> x) noReadInWritten, [])

isBlockDecl (CBlockDecl _) = 
	(True, [])
isBlockDecl _ = 
	(False, [])

noWritesInRead expr sts = 
	let 
		noWritesFun = 
			(\d -> null $ defs d sts) 
		read = 
			(applyRulesGeneral searchVars expr)
		noWritesInRead = 
			map noWritesFun read
	in
		(all (\x -> x) noWritesInRead, [])

checkCondUnkList condUnkList = 
	foldl 
		(\(accB,accU) (b,u) -> (accB && b, accU ++ u))
		(True, [])
		condUnkList


--[CRmdOp, CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp]
--[CAndOp, CXorOp, COrOp, CLndOp, CLorOp]


buildFreeVar nameSTML state0@TransState{freeVar = free} =
	let 
		nameC = "iv_" ++ nameSTML ++ "_" ++ (show free)
		ident = internalIdent nameC
	in  
		((CVar ident undefNodeAnn), ident, 
			[("code_var", [nameSTML], [["iv_" ++ nameSTML ++ "_" ++ (show free)]], (Name{nameId = 0}))],
			state0{freeVar = free + 1})
		--((CVar ident undefNode), ident, [], state0{freeVar = free + 1})


-- hasCalls state (stmt@(CExpr (Just _) _)) = 
-- 	let 
-- 		calls = (applyRulesGeneral searchCalls stmt)
-- 		--funDefs = (applyRulesGeneral (searchFunDefsOld calls) (current_ast state))
-- 		funDefs = searchFunDefs calls (fun_defs state)
-- 	in 
-- 		(((length calls) == (length funDefs)) && (length calls) > 0, [])
-- hasCalls state stmt = 
-- 	(False, [])

hasCallsBlock:: TransState -> CBlockItemAnn -> (Bool, [(String, CStatAnn)])
hasCallsBlock state (CBlockStmt block) = 
	-- hasCall block
	let 
		calls = applyRulesGeneral searchCalls block
		funDefs = searchFunDefs calls (fun_defs state)
	in 
		(((length calls) == (length funDefs)) && (length calls) > 0, [])
hasCallsBlock _ _ = 
	(False, [])

-- hasCalls _ list =
-- 	checkCondUnkList (map hasCallsBlock list)
hasCalls state list =
	checkCondUnkList (map (hasCallsBlock state) list)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

mapfoldl f current (x:xs) = 
	let 
		(nx, ncurrent) = f x current
		(nxs, fcurrent) = mapfoldl f ncurrent xs
	in 
		((nx:nxs), fcurrent)
mapfoldl f current [] = 
	([], current)

noEmpty [] = 
	(False, [])
noEmpty _ = 
	(True, [])

hasCall stmt =  
	let
		astProperties = 
			getAnnotation stmt 
	in
		case astProperties^.hasFunctionCalls.value of 
				Nothing -> 
					--trace ("\n****\nhasCalls:\n" ++ (prettyMyASTAnn stmt) ++ "\nUnknown\n") 
					(True,[("hasFunctionCalls", stmt)])
				Just True ->
					--trace ("\n****\nhasCalls:\n" ++ (prettyMyASTAnn stmt) ++ "\nTrue\n") 
					(True, [])
				Just False ->
					--trace ("\n****\nhasCalls:\n" ++ (prettyMyASTAnn stmt) ++ "\nFalse\n") 
					(False, [])
					

searchCalls :: CExprAnn -> [(String, [CExprAnn], CExprAnn)]
searchCalls call@(CCall (CVar (Ident name _ _) _) args _) =
	[(name, args, call)]
searchCalls _ = 
	[]

searchFunDefs calls (item@(fun_name, _, _):tailFunDefs) = 
	let 
		rest = searchFunDefs calls tailFunDefs
	in 
		case elem fun_name (map (\(name,_,_) -> name) calls) of 
			True -> item:rest
			False -> rest
searchFunDefs calls [] = 
	[]


--searchFunDefsGeneral :: CExtDecl -> [(String, (CTypeSpec, [CDecl], ([CDeclSpec], CDeclr, [CDecl], NodeInfo)), CStat)]
searchFunDefsGeneral 	
	(CFDefExt 
		(CFunDef 
			whole_type@((CTypeSpec fun_type):_)
			whole_decl@(CDeclr (Just (Ident fun_name _ _)) [(CFunDeclr (Right (fun_params, _)) _ _)] _ _ _) 
			whole_decls fun_body node_info)) = 
	[(fun_name, (fun_type, fun_params, (whole_type, whole_decl, whole_decls, node_info)), fun_body)]
searchFunDefsGeneral _ = 
	[]


--searchNoFunDefsGeneral :: CExtDecl -> [CExtDecl]
searchNoFunDefsGeneral 	
	(CFDefExt 
		(CFunDef 
			((CTypeSpec _):_)
			(CDeclr (Just (Ident _ _ _)) [(CFunDeclr (Right (_, _)) _ _)] _ _ _) 
			_ _ _)) = 
	[]
searchNoFunDefsGeneral other = 
	[other]

rebuildFunDefs (_,(_,_,(typefun, declfun, adddeclfun, node_info)), fun_body) = 
	(CFDefExt 
		(CFunDef 
			typefun
			declfun
			adddeclfun fun_body node_info))

inlineExpr state lhs rhs@(CCall (CVar (Ident name _ _) _) args _) = 
	let 
		callTuple = (name, args, rhs)
		funDefs = searchFunDefs [callTuple] (fun_defs state)
		(result, fstate) = 
			inlineCallAssign lhs funDefs callTuple state
	in 
		(result, fstate)

inline state stmt = 
	--(stmt, state)
	let 
		calls = 
			--trace ("\nEnters:\n" ++ (prettyMyASTAnn stmt) ++ "\n") 
			(applyRulesGeneral searchCalls stmt)
		--funDefs = (applyRulesGeneral (searchFunDefsOld calls) (current_ast state))
		funDefs = searchFunDefs calls (fun_defs state)
		(result, fstate) = 
			foldl 
				(\(nstmt,nstate) call -> 
					(inlineCall nstmt funDefs call nstate)) 
				(stmt,state) 
				calls
	in 
		--trace  ("\n*********\nInstance:\n" ++ (prettyMyASTAnn stmt) ++ "\n-" ++ (intercalate "\n-" (map (\(_,_,call) -> prettyMyASTAnn call) calls )) ++ "\nResult:\n" ++ (prettyMyASTAnn result) ) 
		(result, fstate)

--inlineCall :: CStat -> [(String, (CTypeSpec, [CDecl], ([CDeclSpec], CDeclr, [CDecl], NodeInfo)), CStat)] -> (String, [CExpr], CExpr) -> TransState -> (CStat, TransState)
inlineCall stmt defs (name, args, call) state =
	case [(fun_params, annFun, fun_body) | item@(fun_name, ((CVoidType _), fun_params, (_, _, _, annFun)), fun_body) <- defs, fun_name == name] of 
		((pars, (Ann _ nP), body):_) ->
			let
				ncall0 = substituteParArgs body pars args 
				ncall1 = substituteReturn (intConstant 0) ncall0
				ncall = setAnnotation nP ncall1
				-- If we decide inlining should rename vars, uncomment these two lines
				--varDecl = (applyRulesGeneral searchDeclr body)
				--(nstmt, nstate) = foldl (\(nstmt,nstate) var -> renameVars var nstmt nstate) (body,state) varDecl
			in 
				(substituteCall stmt call ncall, state)
		[] ->
			(stmt, state)

inlineCallAssign lhs defs (name, args, call) state =
	case [(fun_params, fun_body) | item@(fun_name, (_, fun_params, _), fun_body) <- defs, fun_name == name] of 
		((pars,body):_) ->
			let
				ncall0 = substituteParArgs body pars args 
				ncall = substituteReturn lhs ncall0
			in 
				(ncall, state)
		[] ->
			((CExpr (Just lhs) undefNodeAnn), state)

substituteParArgs body (par:pars) (arg:args) = 
	let 
		(CDecl _ [((Just (CDeclr (Just identPar) _ _ _ _) ), _, _)] _) = par 
		parVar = (CVar identPar undefNodeAnn)
		nbody0 = substitute body parVar arg
		nbody = substituteInPragmas nbody0 parVar arg
	in 
		substituteParArgs nbody pars args
substituteParArgs body [] [] = 
	body

substituteCall :: CStatAnn -> CExprAnn -> CStatAnn -> CStatAnn
substituteCall ast stmt_searched stmt_substitution =
	let substitutions = 
			applyRulesGeneral (substituteCallAux stmt_searched stmt_substitution) ast
	in foldl (\current_ast change -> changeAST change current_ast) 
			ast substitutions

substituteCallAux stmt_searched stmt_substitution (stmt_found@(CExpr (Just call_found) _))
	| (geq call_found stmt_searched) =
	[(stmt_found,stmt_substitution)]
substituteCallAux _ _ _ = []

substituteReturn sub stmt =
	everywhere (mkT $ substituteReturnAux sub) stmt

substituteReturnAux :: CExprAnn -> CStatAnn -> CStatAnn
substituteReturnAux  (CConst (CIntConst _ _)) (CReturn _ nI) = 
	CExpr (Just (intConstant 0)) nI
substituteReturnAux lhs (CReturn (Just v) nI) = 
	CExpr (Just (CAssign CAssignOp lhs v undefNodeAnn)) nI
substituteReturnAux _ other = 
	other

searchDeclr :: CDeclrAnn -> [(String, Ident)]
searchDeclr (CDeclr (Just (ident@(Ident name _ _))) _ _ _ _) = 
	[(name,ident)]
searchDeclr _ = 
	[]

renameVars (name,ident@(Ident oldName a _)) stmt state = 
	let 
		(freeVar, (Ident freename _ _), _, nstate) =  buildFreeVar name state 
		nident = (Ident freename a undefNode)
		nstmt = changeAST (ident, nident) stmt 
		nnstmt = substitute nstmt (CVar ident undefNodeAnn) freeVar
	in 
		((cvar oldName, cvar freename), (nnstmt, nstate))


fresh_vars state stmt = 
	let 
		(changes, (renamedStmt, fstate)) = 
			mapfoldl 
				(\var (nstmt,nstate) -> renameVars var nstmt nstate) 
				(stmt,state) 
				(applyRulesGeneral searchDeclr stmt)
		fstmt = 
			foldl
				(\nstmt (vold, vnew) -> substituteInPragmas nstmt vold vnew)
				renamedStmt
				changes
	in 
		(fstmt, fstate)


cvar name = 
	(CVar (Ident name 0 undefNode) undefNodeAnn)

createFreshNodeInfo state0@TransState{free_node_id = nId} = 
	-- (Ann (mkNodeInfo nopos (head (namesStartingFrom nId))) nodePropertiesDefault,
	((mkNodeInfo nopos (head (namesStartingFrom nId))),
	 state0
	 {
	 	free_node_id = nId + 1
	 })





processPragmas state0 patPragmaDict ruleProperties matchedAST = 
	let 
		--plain = (map.map) (\s@(_:_) -> PlainString s)
		plain = id
		(nI,state1) = createFreshNodeInfo state0
		--extractPolcaPragmasProperties properties = 
		--	case properties^.polcaPragmas.value of 
		--		Nothing -> 
		--			[]
		--		Just pp ->
		--			pp
		--unplain = (map.map) (\(PlainString s) -> s)
		--plain = (map.map) (\s@(_:_) -> PlainString s)
		--(nI,state1) = createFreshNodeInfo state0
		--astProperties = getAnnotation matchedAST
		--polcaPragmasRule = 
		--	unplain (extractPolcaPragmasProperties ruleProperties)
		--polcaPragmasAst = 
		--	unplain (extractPolcaPragmasProperties astProperties)
		astProperties = 
			getAnnotation matchedAST 
		polcaPragmasRule = 
			extractPolcaPragmas ruleProperties 
		polcaPragmasAst = 
			extractPolcaPragmas astProperties
		polcaPragmasForNewNode0 = 
			concat (map (buildPragmas patPragmaDict matchedAST polcaPragmasAst) polcaPragmasRule)
		polcaPragmasForNewNode = 
			-- trace ((show polcaPragmasRule) ++ " " ++ (show polcaPragmasAst) ++ " " ++ (show polcaPragmasForNewNode0))plain $ 
			correctPragmas polcaPragmasForNewNode0
		nP = 
			-- trace (show polcaPragmasForNewNode) 
			((polcaPragmas.value .~ (Just polcaPragmasForNewNode)) astProperties)
	in 
		((Ann nI nP), state1)

--checkPragmas state0 pragmasRules nameRule node = 
--	let
--		res = has_pragmas state0 pragmasRules nameRule node
--	in
--		(res, state0)

--has_pragmas TransState{pragmas = pragmasState} pragmasRules nameRule node =
--	case nameOfNodeAnn node of 
--		(Just nameNode) -> 
--			matchPragmas nameNode [words pragma | (name, pragma) <- pragmasRules, name == nameRule] [words pragma | (name, pragma) <- pragmasState, name == nameNode]
--		Nothing -> 
--			(False, [])

--matchPragmas nameNode ((prall@(hpr:pr)):pragmasRule) pragmasNode 
--	-- "def" annotation is not be mandatory. Just to identify in STML. 
--	| hpr == "def" =
--		let 
--			(matchs, matching) = matchPragmas nameNode pragmasRule pragmasNode 
--			pn = [pn | (hpn:pn) <- pragmasNode, (hpr == hpn)]
--		in 
--			(matchs ,(hpr, pr, pn, nameNode):matching)
--	| otherwise =  
--		--case trace ((show (hpr:pr)) ++ " " ++ (show pragmasNode)) [pn | (hpn:pn) <- pragmasNode, (hpr == hpn)]  of 
--		case [pn | pnall@(_:pn) <- pragmasNode, (prall == pnall)]  of 
--			[] ->
--				(False,[])
--			pn ->
--				let
--					(matchs, matching) = 
--						--trace (show pn) matchPragmas nameNode pragmasRule pragmasNode 
--						matchPragmas nameNode pragmasRule pragmasNode 
--				in 
--					-- (TypePragma, VarPragma, ValuesPragma, nameNodeValues)
--					(matchs, (hpr, pr, pn, nameNode):matching)
--matchPragmas _ [] pragmasNode = 
--	(True, [])

checkPragmas state0 ruleProperties node = 
	(has_pragmas ruleProperties node, state0)

has_pragmas ruleProperties (Ann nI nP) =
	let 
		polcaPragmasRule = 
			extractPolcaPragmas ruleProperties
		polcaPragmasAst = 
			extractPolcaPragmas nP
	in 
		case nameOfNode nI of 
			(Just nameNode) -> 
				matchPragmas nameNode polcaPragmasRule polcaPragmasAst
			Nothing -> 
				-- (False, [])
				matchPragmas (head newNameSupply) polcaPragmasRule polcaPragmasAst

matchPragmas nameNode ((prall@(hpr:pr)):pragmasRule) pragmasNode 
	-- "def" annotation is not be mandatory. Just to identify in STML. 
	| hpr == "def" =
		let 
			(matchs, matching) = matchPragmas nameNode pragmasRule pragmasNode 
			pn = [pn | (hpn:pn) <- pragmasNode, (hpr == hpn)]
		in 
			(matchs ,(hpr, pr, pn, nameNode):matching)
	| otherwise =  
		-- case trace ((show (hpr:pr)) ++ " " ++ (show pragmasNode)) [pn | (hpn:pn) <- pragmasNode, (hpr == hpn)]  of 
		case [pn | (hpn:pn) <- pragmasNode, (hpr == hpn)]  of 
		-- case [pn | pnall@(_:pn) <- pragmasNode, (prall == pnall)]  of 
			[] ->
				(False,[])
			pn ->
				let
					(matchs, matching) = 
						-- trace (show pn)
						matchPragmas nameNode pragmasRule pragmasNode 
				in 
					-- (TypePragma, VarPragma, ValuesPragma, nameNodeValues)
					(matchs, (hpr, pr, pn, nameNode):matching)
matchPragmas nameNode [] pragmasNode = 
	(True, [(hpn, pn, [pn], nameNode) |(hpn:pn)  <- pragmasNode])

buildPragmas patPragmaDict matchedAST pragmasAst pragmaRule = 
	case (head pragmaRule) of 
		string 
			| ((string == "same_properties") && (length pragmaRule > 1)) ->
				case [nameNode_ | (hpr,pr,_,nameNode_) <- patPragmaDict, hpr == "def", pr == [pragmaRule!!1]] of 
					[] ->
						[]
					[nameNode] ->
						-- trace ((show pragmasAst) ++ " " ++ (show patPragmaDict)) $ 
						-- pragmasAst
						-- [["def","BLOCK_ABS"]]
						-- ("input",["inputb2"],[["pos"],["vel"]],Name {nameId = 1292})
						concat [[(hpr:p) | p <- pr] | (hpr,_,pr,_) <- patPragmaDict]
			| ((string == "chunked") && (length pragmaRule > 1)) ->
				let 
					(CCompound _ listStmt _) = matchedAST
				in 
					case (listStmt!!5) of 
						--(CCompound _ (_:(_:((CBlockStmt (CFor _ (Just (CBinary CLeOp _ bound _)) _ _ _)):_))) _) ->
						(CBlockStmt (CFor _ (Just (CBinary CLeOp _ bound _)) _ _ _)) ->
							let 
								strBound = 
									case bound of 
										(CVar _ _) -> 
											prettyMyASTAnn bound
										_ -> 
											"(" ++ (prettyMyASTAnn bound) ++ ")"
							in 
							 	[["chunked ", strBound]]
						_ -> 
							[]
			  | ((string == "prev_chunk_size") && (length pragmaRule > 1)) ->
				case [pv | (hpr,pr,pv,_) <- patPragmaDict, hpr == "code_var", ("cexpr("++ (head pr) ++ ")") == (pragmaRule!!1)] of 
					[] ->
						[]
					(hpv:_) ->
						[["prev_chunk_size ", (head (head hpv))]]
			  | ((string == "curr_chunk_size") && (length pragmaRule > 1)) ->
				case [pv | (hpr,pr,pv,_) <- patPragmaDict, hpr == "code_var", ("cexpr("++ (head pr) ++ ")") == (pragmaRule!!1)] of 
					[] ->
						[]
					(hpv:_) ->
						[["curr_chunk_size ", (head (head hpv))]]
			  | ((string == "rolled-up") && (length pragmaRule > 0)) ->
				[["rolled-up"]]
			| otherwise -> 
				[]

correctPragmas pragmas = 
	let 
		woDuplicate = nub pragmas
		pragmaDefs = [ (pragma, pragma!!1) | pragma <- pragmas ,(head pragma) == "def"]
	in 
		-- TODO: Treat the defs correctly
		--if (length pragmaDefs) > 1 
		--then
		--	["def ", (concat (map snd pragmaDefs))] ++ (woDuplicate \\ (map fst pragmaDefs))
		--else
			woDuplicate

normalize :: CTranslUnit -> CTranslUnit
normalize (CTranslUnit fundefs ann) = 
	let 
		nfundefs = map rule_func_wo_type fundefs
	in 
		--trace 
		--	(prettyMyAST (CTranslUnit nfundefs ann)) 
			trans_fp rule_move_decl_outside_for_0 (CTranslUnit nfundefs ann)


-- Apply a rule till a fix point is reached (no more rules can be applied)
trans_fp rule ast = 
	case applyRulesGeneral rule ast of 
		[] -> 
			ast 
		((_,o,n):_) ->
			let 
				nast = changeAST (o, n) ast
			in 
				trans_fp rule nast 

-- move_decl_outside_for
rule_move_decl_outside_for_0 old@(CCompound ident bs nodeInfo)=
	(concat [(rule_move_decl_outside_for_3 item ident nodeInfo old) | item@(True,_,_) <- (rule_move_decl_outside_for_2 bs [] True )]) ++ []
rule_move_decl_outside_for_0 _ = []

rule_move_decl_outside_for_3 (True,[var_ini_9,var_body_10,var_end_11], [(CBlockStmt (CFor (Right (CDecl [CTypeSpec (CIntType _)] [(Just (CDeclr (Just var_i_4_ident) [] Nothing [] _),Just (CInitExpr var_init_5 _),Nothing)] _) ) (Just var_cond_6) (Just var_mod_7) (CCompound _ var_internal_8 nIBody) nIFor))]) ident nodeInfo old | True && (and []) && True =
	concat ([[(("move_decl_outside_for", old, (CCompound ident (var_ini_9 ++ [(CBlockDecl (CDecl [CTypeSpec (CIntType undefNode)] [(Just (CDeclr (Just (var_i_4_ident)) [] Nothing [] undefNode), Nothing, Nothing)] undefNode))] ++ [(CBlockStmt (CFor (Left (Just (CAssign CAssignOp (CVar var_i_4_ident undefNode) var_init_5 undefNode))) (Just var_cond_6) (Just var_mod_7) (CCompound [] (var_body_10) nIBody) nIFor))] ++ var_end_11) nodeInfo)))] | (and [])] ++ [])
rule_move_decl_outside_for_3 _ _ _ _ = []

rule_move_decl_outside_for_2 [] acc False =
	[(True,[acc],[])]
rule_move_decl_outside_for_2 [] acc _ =
	[(False,[acc],[])]
rule_move_decl_outside_for_2 (stat@(CBlockStmt (CFor (Right (CDecl [CTypeSpec (CIntType _)] [(Just (CDeclr (Just var_i_4_ident) [] Nothing [] _),Just (CInitExpr var_init_5 _),Nothing)] _) ) (Just var_cond_6) (Just var_mod_7) (CCompound _ var_internal_16 _) _)):tail_) accsl True =
	let
		listItems = rule_move_decl_outside_for_2 tail_ [] False 
		listItems17 = rule_move_decl_outside_for_17 var_internal_16 [] 
	in [(True, (accsl:(inter17 ++ inter)),(stat:(pats17 ++ pats))) | (True,inter,pats) <- listItems, (True, inter17, pats17) <- listItems17] ++ (rule_move_decl_outside_for_2 tail_ (accsl ++ [stat]) True)
rule_move_decl_outside_for_2 (other:tail_) acc  bool1  =
	rule_move_decl_outside_for_2 tail_ (acc ++ [other]) bool1

rule_move_decl_outside_for_17 all [] =
	[(True,[all],[])]


--func_wo_type
rule_func_wo_type :: CExtDecl -> CExtDecl
rule_func_wo_type old@(CFDefExt (CFunDef [] decl decls (CCompound l body anncom) ann)) = 
	CFDefExt (CFunDef [(CTypeSpec (CIntType undefNode))] decl decls (CCompound l (body++[(CBlockStmt (CReturn (Just (CConst (CIntConst (cInteger 0) undefNode))) undefNode))]) anncom) ann)
rule_func_wo_type other = 
	other

declFromStruct (CVar v _) struct_name size state = 
	let
		(struct_name, typefields, numfields) = 
			searchStructProp struct_name (no_fun_defs state)
	in 
		--trace (show (prop_struct)) [CBlockStmt (CExpr (Just (intConstant 0)) undefNodeAnn)]
		[CBlockDecl 
			(CDecl 
				[CTypeSpec typefields] 
				[(Just 
					(CDeclr 
						(Just v) 
						[(CArrDeclr 
							[] 
							(CArrSize 
								False 
								(CBinary 
									CMulOp 
									size 
									(intConstant (toInteger (length numfields))) 
									undefNodeAnn
								)
							) 
							undefNodeAnn
						)]
						Nothing [] undefNodeAnn
					),
					Nothing, Nothing	
				)]
				undefNodeAnn
			)
		]

changeArrayFromStruct stmts (CVar v _) struct_name state = 
	let
		(struct_name, typefields, fields) = 
			searchStructProp struct_name (no_fun_defs state)
		dict = 
			(zip fields (map (intConstant.toInteger) [0..((length fields) - 1)]))
		substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral (substituteStructArrays v dict (length fields)) stmts)
	in 
		(foldl (\current_ast change -> changeAST change current_ast) 
			stmts substitutions)		


searchStructProp searched_name list = 
	case list of 
		(CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident name _ _)) (Just [CDecl [CTypeSpec ctype] fields _]) [] _) _)] [] _):rest) ->
	 		case searched_name of 
	 			name -> 
	 				(searched_name, ctype, extract_field_names fields)
	 			_ ->
	 				searchStructProp searched_name rest
	 	(_:rest) ->
	 		searchStructProp searched_name rest
	 	[] ->
 			(searched_name, (CIntType undefNodeAnn), [])

extract_field_names ((Just (CDeclr (Just (Ident field_name _ _)) [] Nothing [] _),Nothing,Nothing):t) = 
		(field_name:(extract_field_names t))
extract_field_names [] = 
		[]

--substituteStructArrays :: Ident -> CExprAnn -> CExprAnn -> [(CExprAnn,CExprAnn)]
substituteStructArrays (Ident v1 _ _) dict size_struct expr_found@(CMember (CIndex identv@(CVar (Ident v2 _ _) _) i1 _) (Ident field _ _) _ nI2) | v1 == v2 =
	[(expr_found,(CIndex identv (CBinary CAddOp (CBinary CMulOp i1 ((intConstant.toInteger) size_struct) undefNodeAnn) (head [val  | (fieldD, val) <- dict, fieldD == field]) undefNodeAnn) nI2))]
substituteStructArrays _ _ _ _ = []

-- TODO: Check that the steps have a difference of 1 
--     	OR 	try to generalize it to bigger steps through the rule 
--		 	(now the rule is only considering steps of 1)
--isRollable s1@(CExpr (Just (CAssign CAssignOp _ _ _)) _) s2@(CExpr (Just (CAssign CAssignOp _ _ _)) _) = 
--isRollable :: (Data b) => b -> b -> (Bool, [(String, CStatAnn)])
isRollable :: CExprAnn -> CExprAnn -> (Bool, [(String, CStatAnn)])
isRollable s1 s2 = 
	let 
		--[tb1, tb2] = map (removeNodeInfo.removeNodeInfoAnns.removeAddsInArrayAccesses.removeLHS) [s1, s2]
		[tb1, tb2] = map (removeNodeInfo.removeNodeInfoAnns.removeAddsInArrayAccesses) [s1, s2]
	in
		--trace ((prettyMyASTAnn s1) ++ " " ++ (prettyMyASTAnn s2) ++ "\n" ++ (prettyMyASTAnn tb1) ++ " " ++ (prettyMyASTAnn tb2)) 
		((geq tb1 tb2), [])
isRollable _ _ = 
	(False, [])

removeAddsInArrayAccesses :: (Data b) => b -> b
removeAddsInArrayAccesses ast  =
	let 
		substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral searchAddsInArrayAccesses ast)
	in 
		(foldl (\current_ast change -> changeAST change current_ast) 
			ast substitutions)

searchAddsInArrayAccesses :: CExprAnn -> [(CExprAnn, CExprAnn)]
searchAddsInArrayAccesses expr_found@(CIndex v (CBinary CAddOp e1 _ _) nI) =
	let 
		substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(searchAddsInArrayAccesses (CIndex v e1 nI))
		res = 
			foldl 
				(\current_ast change -> changeAST change current_ast) 
				(CIndex v e1 nI)
				substitutions
	in 
		-- trace ((prettyMyASTAnn expr_found) ++ " " ++ (prettyMyASTAnn res)) 
		[(expr_found, res)]
searchAddsInArrayAccesses expr_found@(CIndex v (CConst (CIntConst _ _)) nI) =
	[(expr_found, (CIndex v (intConstant 0) nI))]
searchAddsInArrayAccesses expr_found@(CIndex v (CVar _ _) nI) =
	[(expr_found, (CIndex v (intConstant 0) nI))]
searchAddsInArrayAccesses _ = []

removeNodeInfoAnns :: (Data b) => b -> b
removeNodeInfoAnns ast  =
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral searchNodeInfoAnns ast)
	in 
		(foldl (\current_ast change -> changeAST change current_ast) 
			ast substitutions)

searchNodeInfoAnns expr_found@(Ann _ _) =
	[(expr_found, undefNodeAnn)]
searchNodeInfoAnns _ = []

removeNodeInfo :: (Data b) => b -> b
removeNodeInfo ast  =
	let substitutions = 
			nubBy 
				(\(o1,n1) (o2,n2) -> (geq o1 o2) && (geq n1 n2)) 
				(applyRulesGeneral searchNodeInfo ast)
	in 
		(foldl (\current_ast change -> changeAST change current_ast) 
			ast substitutions)

searchNodeInfo expr_found@(NodeInfo _ _ _) =
	[(expr_found, undefNode)]
searchNodeInfo _ = []


getIdent (CVar ident _) =
	ident

isArray (CIndex _ _ _) = (True, [])
isArray _ = (False, [])


addPositionInfoState :: TransState -> TransState
addPositionInfoState state = 
	-- state{fun_defs = (map addPositionInfoDef (fun_defs state))}
	let 
		nfun_defs = 
			[ (fun_state,info_fun,addPositionInfoStmt fun_state body )
			 | item@(fun_state, info_fun, body) <- (fun_defs state)] 
	in 
		state{fun_defs = nfun_defs}

addPositionInfo :: CTranslUnitAnn -> CTranslUnitAnn
addPositionInfo (CTranslUnit fundefs ann) = 
	CTranslUnit (map addPositionInfoDef fundefs) ann

addPositionInfoDef :: CExtDeclAnn -> CExtDeclAnn
addPositionInfoDef 	
	(CFDefExt 
		(CFunDef 
			whole_type
			whole_decl@(CDeclr (Just (Ident fun_name _ _)) _ _ _ _) 
			whole_decls fun_body ann)) = 
	let
		(CCompound idB stmtB annB) = 
			changePositions fun_name fun_body
		(_,nstmtB) = 
			-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
			mapAccumL (addPositionInfoBI fun_name) 1 stmtB
	in
		(CFDefExt 
			(CFunDef 
				whole_type
				whole_decl
				whole_decls 
				(CCompound idB nstmtB annB)
				ann))
addPositionInfoDef other = 
	other

changePositionAnn label (Ann nI nP) = 
	Ann nI nP{_term_position = label}

changePositions label ast = 
	(fmap (changePositionAnn label) ast)

addPositionInfoBI label i (CBlockStmt stmt) = 
	(i + 1, CBlockStmt (addPositionInfoStmt (label ++ "." ++ (show i)) stmt))
addPositionInfoBI label i other = 
	(i, other)

addPositionInfoStmt label ostmt@(CLabel _ _ _ _) = 
	let 
		(CLabel ident stmt atts ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CLabel ident nstmt atts ann)
addPositionInfoStmt label ostmt@(CCase _ _ _) = 
	let 
		(CCase exp stmt ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CCase exp nstmt ann)
addPositionInfoStmt label ostmt@(CCases _ _ _ _) = 
	let 
		(CCases e1 e2 stmt ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CCases e1 e2 nstmt ann)
addPositionInfoStmt label ostmt@(CDefault _ _) = 
	let 
		(CDefault stmt ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CDefault nstmt ann)
addPositionInfoStmt label ostmt@(CCompound _ _ _) = 
	let 
		(CCompound ident bis ann) = changePositions label ostmt
		(_,nbis) = mapAccumL (addPositionInfoBI label) 1 bis
	in 
		(CCompound ident nbis ann)
addPositionInfoStmt label ostmt@(CIf _ _ _ _) = 
	let 
		(CIf exp thenstmt elsestmt ann) = changePositions label ostmt
		nthenstmt = addPositionInfoStmt (label ++ ".1") thenstmt
		nelsestmt = 
			case elsestmt of 
				Nothing -> 
					Nothing 
				(Just stmt) ->
					Just $ addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CIf exp nthenstmt nelsestmt ann)
addPositionInfoStmt label ostmt@(CSwitch _ _ _) = 
	let 
		(CSwitch exp stmt ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CSwitch exp nstmt ann)
addPositionInfoStmt label ostmt@(CWhile _ _ _ _) = 
	let 
		(CWhile exp stmt b ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CWhile exp nstmt b ann)
addPositionInfoStmt label ostmt@(CFor _ _ _ _ _) = 
	let 
		(CFor init comp inc stmt ann) = changePositions label ostmt
		nstmt = addPositionInfoStmt (label ++ ".1") stmt
	in 
		(CFor init comp inc nstmt ann)
addPositionInfoStmt label stmt = 
	changePositions label stmt 

isPrefix pre str = 
	case stripPrefix pre str of 
		Nothing ->
			False 
		(Just _) ->
			True

---------------------------------------------------------
-- Simplification
---------------------------------------------------------

simplifyExprs state = 
	state{fun_defs = ([simplifyExprsFun fun | fun <- fun_defs state])}


simplifyExprsFun (fun_state, info_fun, body) = 
	let 
		nbody0 = fixPointSimplify body searchSimplicableOperations simplifyOperation
		nbody = fixPointSimplify nbody0 searchSimplicableTernaries simplifyTernary
	in
		(fun_state, info_fun, nbody)


fixPointSimplify :: CStatAnn -> (CExprAnn -> [CExprAnn]) -> (CExprAnn -> CExprAnn) -> CStatAnn
fixPointSimplify stmt funSearch funTrans = 
	let 
		simplificable = applyRulesGeneral funSearch stmt
		nstmt = 
			foldl 
				(\ast change -> changeAST change ast)
				stmt
				[(sim, funTrans sim) | sim <- simplificable]
	in 
		case (geq nstmt stmt) of 
			True -> 
				nstmt 
			False ->
				fixPointSimplify nstmt funSearch funTrans

searchSimplicableOperations :: CExprAnn -> [CExprAnn]
searchSimplicableOperations expr@(CBinary _ (CConst (CIntConst _ _)) (CConst (CIntConst _ _)) _) = 
	[expr]
searchSimplicableOperations expr@(CBinary _ (CConst (CIntConst v _)) _ _) | (getCInteger v) == 0 = 
	[expr]
searchSimplicableOperations expr@(CBinary _ (CConst (CIntConst v _)) _ _) | (getCInteger v) == 1 = 
	[expr]
searchSimplicableOperations expr@(CBinary _ _ (CConst (CIntConst v _)) _) | (getCInteger v) == 0 = 
	[expr]
searchSimplicableOperations expr@(CBinary _ _ (CConst (CIntConst v _)) _) | (getCInteger v) == 1 = 
	[expr]
searchSimplicableOperations other = 
	[]

searchSimplicableTernaries :: CExprAnn -> [CExprAnn]
searchSimplicableTernaries expr@(CCond cond1 (Just (CCond cond2 (Just t) e1 _)) e2 _) = 
	case (exprEqual e1 e2) of 
		True ->
			[expr]
		False ->
			[]
searchSimplicableTernaries other = 
	[]

simplifyOperation (CBinary CAddOp (CConst (CIntConst x _)) (CConst (CIntConst y _)) nI) = 
	(CConst (CIntConst (cInteger (toInteger ((getCInteger x) + (getCInteger y)))) nI))
simplifyOperation (CBinary CMulOp (CConst (CIntConst x _)) (CConst (CIntConst y _)) nI) = 
	(CConst (CIntConst (cInteger (toInteger ((getCInteger x) * (getCInteger y)))) nI))
simplifyOperation (CBinary CSubOp (CConst (CIntConst x _)) (CConst (CIntConst y _)) nI) = 
	(CConst (CIntConst (cInteger (toInteger ((getCInteger x) - (getCInteger y)))) nI))
simplifyOperation expr@(CBinary op e1 e2 nI) = 
	case (e1, e2, op) of 
		((CConst (CIntConst v _)), _, CAddOp) | (getCInteger v) == 0 ->
			e2
		((CConst (CIntConst v _)), _, CSubOp) | (getCInteger v) == 0 ->
			e2
		((CConst (CIntConst v _)), _, CMulOp) | (getCInteger v) == 1 ->
			e2
		(_, (CConst (CIntConst v _)), CAddOp) | (getCInteger v) == 0 ->
			e1
		(_, (CConst (CIntConst v _)), CSubOp) | (getCInteger v) == 0 ->
			e1
		(_, (CConst (CIntConst v _)), CMulOp) | (getCInteger v) == 1 ->
			e1
		_ ->
			expr
simplifyOperation other = 
	other

simplifyTernary (CCond cond1 (Just (CCond cond2 (Just t) e1 _)) e2 nI) = 
	(CCond (CBinary CLndOp cond1 cond2 undefNodeAnn) (Just t) e2 nI)
simplifyTernary other = 
	other

---------------------------------------------------------
-- OLD CODE
---------------------------------------------------------


--data NodePropertyInfo = 
--		HasSideEffects Bool 
--	|	Reads [String]
--	| 	AllPragmas [String]
--	deriving (Show, Data, Typeable)

--applyRulesExpr fun ast =
--	(everything (++) ( [] `mkQ` fun) ast)

--applicableChangesFun rule state  	
--	(CFDefExt 
--		(CFunDef _
--			(CDeclr (Just (Ident fun_name _ _)) _ _ _ _) 
--			_ fun_body _)) = 
--	let 
--		changes = applyRulesGeneral (rule state) fun_body
--	in 
--		[(fun_name, change) | change <- changes]
--applicableChangesFun _ _ _ = 
--	[]


--searchFunDefsOld :: [(String, [CExpr], CExpr)] -> CExtDecl -> [(String, CTypeSpec, [CDecl], CStat)]
--searchFunDefsOld calls 	
--	(CFDefExt 
--		(CFunDef 
--			((CTypeSpec fun_type):_)
--			(CDeclr (Just (Ident fun_name _ _)) [(CFunDeclr (Right (fun_params, _)) _ _)] _ _ _) 
--			_ fun_body _)) = 
--	case elem fun_name (map (\(name,_,_) -> name) calls) of 
--		True -> 
--			[(fun_name, fun_type, fun_params, fun_body)]
--		False ->
--			[]
--searchFunDefsOld _ _ = 
--	[]

--usesInStmt :: CExpr -> CBlockItem -> [CExpr]
--usesInStmt expr_searched stmt = 
--	uses expr_searched [stmt]

--usesInExpr :: CExpr -> CExpr -> [CExpr]
--usesInExpr expr_searched expr = 
--	uses expr_searched [(CBlockStmt (CExpr (Just expr) undefNode) )]

--getNodeFromName name = 
--	applyRulesGeneral (getNodeFromName_ name)


--getNodeFromName_ name node =  
--	case (nameOfNode (nodeInfo node)) of
--		Nothing ->
--			[]
--		(Just nameNode) ->
--			if  (geq name nameNode)
--			then [node]
--			else []

--getPolcaAnn _ = unsafePerformIO (getPolcaAnn_)
--getPolcaAnn_ =
--	do 
--		linesFile <- readLinesFile "state"
--		let polcaAnn =  read (linesFile!!2) :: [(Name,[String])]
--		return polcaAnn


--getCurrentInfo ann = unsafePerformIO (getCurrentInfo_ ann)
--getCurrentInfo_ ann =
--	do 
--		--handle <- openFile "state" ReadMode
--		--contents <- hGetContents handle 
--		--let linesFile = lines contents
--		--rnf linesFile `seq` hClose handle
--		linesFile <- readLinesFile "state"
--		--putStrLn ("Lines: " ++ (show linesFile))
--		let currentIte = read (linesFile!!0) :: Int
--		let currentNode = read (linesFile!!1) :: Name
--		let polcaAnn = read (linesFile!!2) ::  [(Name,[String])]
--		let newState = ((show (currentIte + 1))++"\n" ++ (show (incName currentNode)) ++ "\n" ++ (show (polcaAnn ++ [(currentNode,ann)]) ) ++ "\n")
--		writeFile_ "state" newState
--		return ((mkNodeInfo nopos currentNode), currentIte)

--writeFile_ file string = 
--	do 
--		handleW <- openFile file WriteMode
--		hPutStr handleW string 
--		rnf string `seq` hClose handleW

--readLinesFile file = 
--	do
--	    result <- try (openFile file ReadMode) :: IO (Either SomeException Handle)
--	    case result of
--	        Left ex  -> 
--	        	--trace ("Can't read") (readLinesFile file)
--	        	(readLinesFile file)
--	        Right handle -> 
--	        	readLineFile_ handle 

--readLineFile_ handle =
--	do 
--		contents <- hGetContents handle 
--		let linesFile = lines contents
--		-- force the whole file to be read, then close
--		rnf linesFile `seq` hClose handle
--		return linesFile

-- $( derive makeBinary ''CStatement )
-- $( derive makeBinary ''Ident )
-- $( derive makeBinary ''CExpression )
-- $( derive makeBinary ''CDeclaration )
-- $( derive makeBinary ''CDeclarator )
-- $( derive makeBinary ''CBinaryOp )
-- $( derive makeBinary ''NodeInfo )
-- $( derive makeBinary ''CCompoundBlockItem )
-- $( derive makeBinary ''CDeclaration )
-- $( derive makeBinary ''Position )

-- $( derive makeBinary ''CDeclaration )
-- $( derive makeBinary ''CDeclaration )
-- $( derive makeBinary ''CDeclaration )
-- $( derive makeBinary ''CDeclaration )
-- $( derive makeBinary ''TransState )


--newtype PlainString = PlainString String
--	deriving (Show, Data, Typeable, Eq)

--instance Show PlainString where
--  --show (PlainString s) = (show s)
---- TODO: changed to allow corect rule creation
--  --show (PlainString s) = 
--  --	"PlainString " ++ (show s)

--type PB = PropertyInfo Bool
--type PS = PropertyInfo [String]
--type PSS = PropertyInfo [[String]]


--data MyTypes a b c = Type1 a | Type2 b | Type3 c

--data  PropertyType a =  
--		O1 (PropertyInfo Bool)
--	| 	O2 (PropertyInfo [String])
--	| 	O3 (PropertyInfo [[String]])
--  deriving (Show, Data, Typeable, Eq) 

--instance Show PragmaType where
--  show STML = "stml"
--  show POLCA = "polca"
--  show NONE = "none"

--class PropertyData a where


--type PropertyDataTypeBool = Bool
--type PropertyDataTypeListString = [String]
--type PropertyDataTypeListListString = [[String]]



--xxxx :: Functor f => [(PropertyInfo d -> f (PropertyInfo d)) -> NodeProperties -> f NodeProperties]
--xxxx = [hasSideEffects, rangeInfo]

------ TODO: enable a list of multiple types
--allProperties :: [Dynamic]
--allProperties = 
--	[	  
--			toDyn hasSideEffects 
--		, 	toDyn readIn
--		, 	toDyn writeIn
--		, 	toDyn localSymbols
--		, 	toDyn rangeInfo
--		, 	toDyn isCanonical
--		, 	toDyn isPerfectNest
--		, 	toDyn hasLoops
--		, 	toDyn hasControlFlowModifiers
--		, 	toDyn scalarDependences
--		, 	toDyn polcaPragmas
--		, 	toDyn allPragmas 
--	]


--allPlainProperties = 
--	allProperties \\ [polcaPragmas, allPragmas]

--prettyProp hasSideEffects = "side_effects"
--prettyProp readIn = "reads"
--prettyProp writeIn = "writes"
--prettyProp localSymbols = "local_symbols"
--prettyProp rangeInfo = "range_info"
--prettyProp isCanonical = "is_canonical"
--prettyProp isPerfectNest = "is_perfect_nest"
--prettyProp hasLoops = "has_loops"
--prettyProp hasControlFlowModifiers = "has_control_flow_modifiers"
--prettyProp scalarDependences = "scalar_dependences"

--unplainProperties np0 = 
--	case np0^.polcaPragmas.value of 
--		Nothing -> 
--			np0
--		Just pp ->
--			(polcaPragmas.value .~ (Just (map.map) (\(PlainString s) -> s) pp)) np0

--instance Pretty NodeProperties where
--    pretty nodeProperties = text (show nodeProperties)

--processPragmas state0@TransState{pragmas = pragmasState} pragmasRules patPragmaDict nameRule matchedAST = 
--	--(undefNode, state0)
--	let 
--		(nI,state1) = createFreshNodeInfo state0
--		pragmasRule = [words pragma | (name, pragma) <- pragmasRules, name == nameRule]
--		pragmasForNode =  concat (map (buildPragmas patPragmaDict pragmasState matchedAST) pragmasRule)
--		pragmasCorrected = correctPragmas pragmasForNode
--	in 
--		(nI,
--		 state1
--		 {
--		 	pragmas = pragmasState ++ [(fromJust (nameOfNodeAnn nI), pragma) | pragma <- pragmasCorrected]
--		 })
