module PragmaPolcaReader where 

-- # LANGUAGE OverloadedStrings, ScopedTypeVariables #
--  Copyright (C) 2015 The IMDEA Software Institute 
-- 	            and the Technical University of Madrid

-- All rights reserved.
-- For additional copyright information see below.

-- This file is part of the polca-transformation-rules package

-- License: This work is licensed under the Creative Commons
-- Attribution-NonCommercial-NoDerivatives 4.0 International
-- License. To view a copy of this license, visit
-- http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter
-- to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
--
-- Main author: Salvador Tamarit
-- Please address questions, etc. to: <polca-project-madrid@software.imdea.org>



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
		code  :: String,
		pragmaLine :: Int,
		start :: Int,
		len :: Int
	} deriving Show

data ListPragmaPolca = 
	ListPragmaPolca
	{ 
		pragmas :: [PragmaPolca]
	} deriving Show

instance FromJSON PragmaPolca where
    parseJSON (Object v) = PragmaPolca <$>
                           v .: DT.pack "pragma" <*>
                           v .: DT.pack "code" <*>
                           v .: DT.pack "pragmaLine" <*>
                           v .: DT.pack "start" <*>
                           v .: DT.pack "len"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance FromJSON ListPragmaPolca where
    parseJSON (Object v) = ListPragmaPolca <$>
                           v .: DT.pack "pragmas"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance ToJSON PragmaPolca where
    toJSON (PragmaPolca pragma code pragmaLine start len) = 
    	object [DT.pack "pragma" .= pragma, DT.pack "code" .= code, 
    			DT.pack "pragmaLine" .= pragmaLine, DT.pack "start" .= start, DT.pack "len" .= len]

instance ToJSON ListPragmaPolca where
    toJSON (ListPragmaPolca pragmas) = 
    	object [DT.pack "pragmas" .= pragmas]

main = 
	do
		args <- getArgs
		let (filename':_) = args
		let filename = filename' ++ ".c"
		--handle <- openFile  filename ReadMode
		polcaAnn' <- parsePolcaAnn filename
		ast <- parseMyFile filename
		let (errors,polcaAnn) = (errorsNclean polcaAnn') 
		let linkedPolcaAnn = linkPolcaAnnAst ast polcaAnn
		if errors /= [] 
		then error (Prelude.unlines errors)
		else putStr ""
		-- else putStrLn ("Pragmas polca successfully read from " ++ filename)
		-- putStrLn ("AST successfully read from " ++ filename)
		--putStrLn (show [(pp,(prettyMyAST ast_node)) | (ast_node, pp) <- linkedPolcaAnn])
		let jsonContent = 
			(ListPragmaPolca [(PragmaPolca {pragma = pp, code = (prettyMyAST ast_node), pragmaLine = line, start = minCodeLine, len = (Prelude.length (Prelude.lines (prettyMyAST ast_node))) }) 
							  | (ast_node, pp, line, minCodeLine) <- linkedPolcaAnn])
		--let res = decode "{\"pragma\":\"zip\",\"code\":\"Some C code\"}" :: Maybe PragmaPolca
		-- let jsonFilename = filename' ++ ".json"
		let jsonStr = BSL.unpack (encode jsonContent)
		putStrLn jsonStr
		-- writeFile jsonFilename jsonStr
		-- putStrLn ("JSON content :\n" ++ jsonStr)
		-- putStrLn ("JSON content stored in " ++ jsonFilename)

