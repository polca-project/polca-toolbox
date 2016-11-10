-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

{-# LANGUAGE BangPatterns #-}

module FeaturesExtractor where 

import Main as M (featuresExtract)

import Rules as R (nameRules)

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
detListRules args = 
	let 
		allRules = R.nameRules
	in 
		case (args!!1) of 
			"ALL" ->
				allRules
			other ->
				[(PPL.trim r) | r <- (DLS.splitOn " " other), elem (PPL.trim r) allRules]

errorMsg = 
	putStrLn "Usage:\n\tpolca_features filename (ALL | list_of_rules) [polca_block]\nThe source file is needed.\nThe list of rules that are going to be considered is needed. In case one wanted to consider all the rules, then it should write ALL.\nThere are one optional parameters:\n\t- A polca block where the transformation will be done.\n\t  If it is not provided, then the transformation scope is the whole program."

main = do
	args <- getArgs
	case length args of 
		0 -> 
			errorMsg
		1 ->
			errorMsg
		2 ->
			featuresExtract (detFileName args) (detListRules args) Nothing
		3 ->
			featuresExtract (detFileName args) (detListRules args) (Just $ args!!2)
			 