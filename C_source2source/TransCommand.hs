-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

module TransCommand where 

import Main as M (translateWOUser)

import System.Environment( getArgs )

main = do
    args <- getArgs
    case length args of 
        0 -> 
            putStrLn "A source file and a platform is needed."
        1 -> 
            putStrLn "A source file and a platform is needed."
        _ ->
            case (take 2 (reverse (args!!0))) of 
                ('c':('.':_)) ->
                    M.translateWOUser (reverse (drop 2 (reverse (args!!0)))) (args!!1)  
                _ ->
                    M.translateWOUser (args!!0) (args!!1)  
             