-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module TransExe where 

import Main as M (only_translate)

import System.Environment( getArgs )

main = do
    args <- getArgs
    case length args of 
        0 -> 
            putStrLn "A source file is needed."
        _ ->
            case (take 2 (reverse (args!!0))) of 
                ('c':('.':_)) ->
                    M.only_translate (reverse (drop 2 (reverse (args!!0))))
                _ ->
                    M.only_translate (args!!0)  
             