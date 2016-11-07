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
		code :: String
	} deriving Show

data ListPragmaPolca = 
	ListPragmaPolca
	{ 
		pragmas :: [PragmaPolca]
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
                           v .: DT.pack "code" 
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON ListPragmaPolca where
    parseJSON (Object v) = ListPragmaPolca <$>
                           v .: DT.pack "pragmas"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance ToJSON PragmaPolca where
    toJSON (PragmaPolca pragma pragmaLine startLine startCol endLine endCol code) = 
    	object [DT.pack "pragma" .= pragma, 
    			-- DT.pack "codePP" .= codePP, 
    			DT.pack "pragmaLine" .= pragmaLine, 
    			-- DT.pack "start" .= start, 
    			-- DT.pack "len" .= len,
    			DT.pack "startLine" .= startLine, 
    			DT.pack "startCol" .= startCol,
    			DT.pack "endLine" .= endLine, 
    			DT.pack "endCol" .= endCol,
    			DT.pack "code" .= code]

instance ToJSON ListPragmaPolca where
    toJSON (ListPragmaPolca pragmas) = 
    	object [DT.pack "pragmas" .= pragmas]

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
		let (errors,polcaAnn) = (errorsNclean polcaAnn') 
		linkedPolcaAnn <- linkPolcaAnnAst ast polcaAnn filename
		if errors /= [] 
		then error (Prelude.unlines errors)
		else putStr ""
		-- else putStrLn ("Pragmas polca successfully read from " ++ filename)
		-- putStrLn ("AST successfully read from " ++ filename)
		--putStrLn (show [(pp,(prettyMyAST ast_node)) | (ast_node, pp) <- linkedPolcaAnn])
		let jsonContent = 
			(ListPragmaPolca 
				[(PragmaPolca{
					pragma = pp, 
					-- codePP = (prettyMyAST ast_node), 
					pragmaLine = line, 
					-- start = minCodeLine, 
					-- len = (Prelude.length (Prelude.lines (prettyMyAST ast_node))),
					startLine = sL,
					startCol = sC,
					endLine = eL,
					endCol = eC,
					code = codeOri
				}) 
				| (ast_node, pp, line, minCodeLine, (sL, sC), (eL, eC), codeOri) <- linkedPolcaAnn])
		--let res = decode "{\"pragma\":\"zip\",\"code\":\"Some C code\"}" :: Maybe PragmaPolca
		-- let jsonFilename = filename' ++ ".json"
		let jsonStr = BSL.unpack (encode jsonContent)
		putStrLn jsonStr
		-- writeFile jsonFilename jsonStr
		-- putStrLn ("JSON content :\n" ++ jsonStr)
		-- putStrLn ("JSON content stored in " ++ jsonFilename)

