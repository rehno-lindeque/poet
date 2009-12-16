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
import OSIX.SemanticDB

-- Standard modules
import Control.Monad
import Data.Char
import System.IO
import System( getArgs )
import Foreign.C.String


{-                              IMPLEMENTATION                              -}

main = do 
  putStrLn "Welcome to Poet, your friendly semantic translator!"
  args <- getArgs
  let fileName = head args
  putStrLn "Translating file: " 
  fileHandle <- openFile fileName ReadMode
  fileContents <- hGetContents fileHandle
  putStr fileContents
  semanticDBInit
  c_SemanticDBDebugInit
  withCString "dom" c_DeclareOpenDomain 
  fooSymbol <- withCString "foo" c_DeclareSymbol
  valSymbol <- withCString "val" c_DeclareSymbol
  c_DeclareRelation fooSymbol valSymbol
  c_DebugOutputEnvironment
  withCString "dom" c_CloseDomain
  hClose fileHandle



