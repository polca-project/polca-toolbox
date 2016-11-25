-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module RandomOracle where 

import Main as M  (CodeAndChanges, changes)

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL

import System.Environment( getArgs )
import System.Random 



main = 
	do
		(jsonFile:_) <- getArgs 
		contents <- readFile jsonFile
		case JSON.decode (BSL.pack contents) of 
			Nothing ->
				do 
					putStrLn "A problem occured while reading the JSON expression"
					putStrLn "-1"
			(Just codeAndChanges) ->
				do 
					selected <- (randomRIO (0, (length $ M.changes codeAndChanges)  - 1) :: IO Int)
					stop <- (randomRIO (0, 20) :: IO Int)
					case stop of 
						0 ->
							putStrLn "-1"
						_ ->
							putStrLn $ show $ selected

			 