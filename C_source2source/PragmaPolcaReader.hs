-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module PragmaPolcaReader where 

import PragmaPolcaLib
import RulesLib

import System.Environment
import System.IO

import Data.Aeson
import Data.Text as DT
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Applicative
import Control.Monad

import Debug.Trace

import qualified Text.Groom as Gr

data PragmaPolca = 
	PragmaPolca
	{ 
		pragma :: String,
		-- codePP  :: String,
		pragmaLine :: Int,
		-- start :: Int,
		-- len :: Int,
		startLine :: Int,
		startCol :: Int,
		endLine :: Int,
		endCol :: Int,
		code :: String,
		funInfo :: FunInfo
	} deriving Show

data FunInfo = 
	FunInfo
	{
		funName :: String,
		paramPos :: [VarPos],
		pragmaPos :: [PragmaVarPos]
	} deriving Show

data VarPos = 
	VarPos
	{
		varPos :: Int,
		varName :: String

	} deriving Show

data PragmaVarPos = 
	PragmaVarPos
	{
		parPosFun :: Int,
		parPosPragma :: Int,
		parName :: String

	} deriving Show

data CallPolca = 
	CallPolca
	{ 
		line :: Int,
		fun :: String,
		args :: [VarPos]
	} deriving Show

data ListPragmaPolca = 
	ListPragmaPolca
	{ 
		pragmas :: [PragmaPolca],
		calls :: [CallPolca]
	} deriving Show

instance FromJSON PragmaPolca where
    parseJSON (Object v) = PragmaPolca <$>
                           v .: DT.pack "pragma" <*>
                           -- v .: DT.pack "codePP" <*>
                           v .: DT.pack "pragmaLine" <*>
                           -- v .: DT.pack "start" <*>
                           -- v .: DT.pack "len" <*>
                           v .: DT.pack "startLine" <*>
                           v .: DT.pack "startCol" <*>
                           v .: DT.pack "endLine" <*>
                           v .: DT.pack "endCol" <*>
                           v .: DT.pack "code" <*> 
                           v .: DT.pack "funInfo" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON CallPolca where
    parseJSON (Object v) = CallPolca <$>
                           v .: DT.pack "line" <*>
                           v .: DT.pack "fun" <*>
                           v .: DT.pack "args" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON FunInfo where
    parseJSON (Object v) = FunInfo <$>
                           v .: DT.pack "funName" <*>
                           v .: DT.pack "paramPos" <*>
                           v .: DT.pack "pragmaPos" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON VarPos where
    parseJSON (Object v) = VarPos <$>
                           v .: DT.pack "varPos" <*>
                           v .: DT.pack "varName" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON PragmaVarPos where
    parseJSON (Object v) = PragmaVarPos <$>
                           v .: DT.pack "parPosFun" <*>
                           v .: DT.pack "parPosPragma" <*>
                           v .: DT.pack "parName" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON ListPragmaPolca where
    parseJSON (Object v) = ListPragmaPolca <$>
                           v .: DT.pack "pragmas" <*>
                           v .: DT.pack "calls"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance ToJSON PragmaPolca where
    toJSON (PragmaPolca pragma pragmaLine startLine startCol endLine endCol code funInfo) = 
    	object [
    			DT.pack "pragma" .= pragma, 
    			-- DT.pack "codePP" .= codePP, 
    			DT.pack "pragmaLine" .= pragmaLine, 
    			-- DT.pack "start" .= start, 
    			-- DT.pack "len" .= len,
    			DT.pack "startLine" .= startLine, 
    			DT.pack "startCol" .= startCol,
    			DT.pack "endLine" .= endLine, 
    			DT.pack "endCol" .= endCol,
    			DT.pack "code" .= code,
    			DT.pack "funInfo" .= funInfo
    			]

instance ToJSON FunInfo where
    toJSON (FunInfo funName paramPos pragmaPos) = 
    	object [
    			DT.pack "funName" .= funName, 
    			DT.pack "paramPos" .= paramPos, 
    			DT.pack "pragmaPos" .= pragmaPos
    			]

instance ToJSON VarPos where
    toJSON (VarPos varPos varName) = 
    	object [
				DT.pack "varPos" .= varPos,
    			DT.pack "varName" .= varName
    			]

instance ToJSON PragmaVarPos where
    toJSON (PragmaVarPos parPosFun parPosPragma parName) = 
    	object [
				DT.pack "parPosFun" .= parPosFun,
				DT.pack "parPosPragma" .= parPosPragma,
    			DT.pack "parName" .= parName
    			]

instance ToJSON CallPolca where
    toJSON (CallPolca line fun args) = 
    	object [
    			DT.pack "line" .= line, 
    			DT.pack "fun" .= fun, 
    			DT.pack "args" .= args
    			]

instance ToJSON ListPragmaPolca where
    toJSON (ListPragmaPolca pragmas calls) = 
    	object [DT.pack "pragmas" .= pragmas, DT.pack "calls" .= calls]

detFileName args = 
	case (Prelude.take 2 (Prelude.reverse (args!!0))) of 
		('c':('.':_)) ->
			(Prelude.reverse (Prelude.drop 2 (Prelude.reverse (args!!0)))) 
		_ ->
			(args!!0)

main = 
	do
		args <- getArgs
		let filename' =  detFileName args
		let filename = filename' ++ ".c"
		--handle <- openFile  filename ReadMode
		polcaAnn' <- parsePolcaAnn filename False
		ast <- parseMyFile filename
		writeFile (filename' ++ ".ast") (Gr.groom ast)
		let (errors,polcaAnn) = (errorsNclean polcaAnn') 
		linkedPolcaAnn <- linkPolcaAnnAst ast polcaAnn filename
		if errors /= [] 
		then error (Prelude.unlines errors)
		else putStr ""
		-- else putStrLn ("Pragmas polca successfully read from " ++ filename)
		-- putStrLn ("AST successfully read from " ++ filename)
		--putStrLn (show [(pp,(prettyMyAST ast_node)) | (ast_node, pp) <- linkedPolcaAnn])
		let jsonContent = 
			ListPragmaPolca
			{
				pragmas = 
					[(PragmaPolca
					  {
						pragma = pp, 
						-- codePP = (prettyMyAST ast_node), 
						pragmaLine = line, 
						-- start = minCodeLine, 
						-- len = (Prelude.length (Prelude.lines (prettyMyAST ast_node))),
						startLine = sL,
						startCol = sC,
						endLine = eL,
						endCol = eC,
						code = codeOri,
						funInfo = 
							FunInfo
							{
								funName = fN,
								paramPos = [VarPos{varName = par, varPos = pos} | (pos, par) <- ppar],
								pragmaPos = [PragmaVarPos{parName = par, parPosFun = posF, parPosPragma = posP}  | (par, posF, posP) <- pprag]
							}
					}) 
					| (ast_node, pp, line, minCodeLine, (sL, sC), (eL, eC), codeOri, (fN, ppar, pprag)) <- linkedPolcaAnn],
				calls =
					[
					CallPolca
					{
						line = lineC,
						fun = funC,
						args = [VarPos{varName = par, varPos = pos} | (pos, par) <- argsC]
					}
					| (lineC, funC, argsC) <- readCallsToPragmedFuns linkedPolcaAnn ast]
			}
		--let res = decode "{\"pragma\":\"zip\",\"code\":\"Some C code\"}" :: Maybe PragmaPolca
		-- let jsonFilename = filename' ++ ".json"
		let jsonStr = BSL.unpack (encode jsonContent)
		putStrLn jsonStr
		-- writeFile jsonFilename jsonStr
		-- putStrLn ("JSON content :\n" ++ jsonStr)
		-- putStrLn ("JSON content stored in " ++ jsonFilename)

