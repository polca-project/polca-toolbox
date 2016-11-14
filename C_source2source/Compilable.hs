-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship


{-# LANGUAGE BangPatterns #-}

module Compilable where 

import Main as M (trans_to_platform, trans_to_platformInt)

import System.Environment( getArgs )

import Text.Read as TR

import Debug.Trace 



main = do
	args <- getArgs
	case length args of 
		0 -> 
			putStrLn "Usage:\n\tpolca_s2s filename ([chain_id] [step_id] | [step_id]) [polca_block]\nThe source file is needed.\nThere are three optional parameters, two integers and a string.\n\t- In case only one integer is provided:\n\t\t1st - Number of the inital transformation step (default is 0).\n\t- In case two integer are provided:\n\t\t1st - Number of the transformation chain.\n\t\t2nd - Number of the inital transformation step (default is 0).\n\t- The string is a polca block where the transformation will be done.\n\t  If it is not provided, then the transformation scope is the whole program."
		1 ->
			case (take 2 (reverse (args!!0))) of 
				('c':('.':_)) ->
					M.trans_to_platform (reverse (drop 2 (reverse (args!!0))))
				_ ->
					M.trans_to_platform (args!!0)	
		2 ->
			do 
				let filename =
					case (take 2 (reverse (args!!0))) of 
						('c':('.':_)) ->
							(reverse (drop 2 (reverse (args!!0)))) 
						_ ->
							(args!!0)
				let mBseqId = 
					(TR.readMaybe (args!!1))::(Maybe Int)
				let (seqId, polcaBlock) =
					case mBseqId of 
						Nothing -> 
							(0, (args!!1)) 
						(Just v) ->
							(v, "")
				M.trans_to_platformInt filename (-1) seqId polcaBlock
		3 ->
			do 
				let filename =
					case (take 2 (reverse (args!!0))) of 
						('c':('.':_)) ->
							(reverse (drop 2 (reverse (args!!0)))) 
						_ ->
							(args!!0)
				let mBseqId = 
					(TR.readMaybe (args!!2))::(Maybe Int)
				let (chainId, polcaBlock) =
					case mBseqId of 
						Nothing -> 
							(-1, (args!!2)) 
						(Just v) ->
							(v, "")
				M.trans_to_platformInt filename chainId ((read (args!!1))::Int) polcaBlock
		_ ->
			case (take 2 (reverse (args!!0))) of 
				('c':('.':_)) ->
					M.trans_to_platformInt (reverse (drop 2 (reverse (args!!0)))) ((read (args!!1))::Int) ((read (args!!2))::Int) (args!!3)
				_ ->
					M.trans_to_platformInt (args!!0) ((read (args!!1))::Int) ((read (args!!2))::Int) (args!!3)
			 