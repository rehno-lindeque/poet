------------------------------------------------------------------------------
--
--    POET.HS
--
--    Copyright © 2009, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
module Main( main ) where

{-                               DOCUMENTATION                              -}
{-
    IMPLEMENTATION:

    TODO:
-}

{-                                 MODULES                                  -}
-- OpenSemanticDB
--import OSIX.SemanticDB

-- Standard modules
import Control.Monad
import Data.Char
import System.IO
import System( getArgs )


{-                              IMPLEMENTATION                              -}

main = do 
	putStrLn "Welcome to Poet, your friendly semantic translator!"
	args <- getArgs
	let fileName = head args
	putStrLn "Translating file: " 
	fileHandle <- openFile fileName ReadMode
	fileContents <- hGetContents fileHandle
	putStr fileContents
	hClose fileHandle

