-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

{-# LANGUAGE BangPatterns #-}

module ChangesExtractor where 

import Main as M (changesExtract)

import PragmaPolcaLib as PPL (trim)

import System.Environment( getArgs )

import Text.Read as TR

import Data.List.Split as DLS

import Debug.Trace 

detFileName args = 
	case (Prelude.take 2 (Prelude.reverse (args!!0))) of 
		('c':('.':_)) ->
			(Prelude.reverse (Prelude.drop 2 (Prelude.reverse (args!!0)))) 
		_ ->
			(args!!0)

errorMsg = 
	putStrLn "Usage:\n\tpolca_changes filename [polca_block]\nThe source file is needed.\nThere are one optional parameters:\n\t- A polca block where the transformation will be done.\n\t  If it is not provided, then the transformation scope is the whole program."

main = do
	args <- getArgs
	case length args of 
		0 -> 
			errorMsg
		1 ->
			changesExtract (detFileName args)  Nothing
		2 ->
			changesExtract (detFileName args)  (Just $ args!!1)
			 