------------------------------------------------------------------------------
--
--    POET.HS
--
--    Copyright © 2009, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Main( main ) where

{-                               DOCUMENTATION                              -}
{-
    IMPLEMENTATION:

    TODO:
-}

{-                                 MODULES                                  -}
-- OpenSemanticDB
import OSIX.SemanticDB
--foreign import ccall driver_callback :: IO ()

-- Standard modules
import System( getArgs )
import System.IO
import System.IO.Error
import Foreign.C.String
{-import Control.Monad
import Data.Char
-}



{-                              IMPLEMENTATION                              -}

-- Translate file
parseLLFile :: String -> IO ()
parseLLFile fileName = do
  fileHandle <- (openFile fileName ReadMode)
  fileContents <- hGetContents fileHandle
  putStr fileContents
  hClose fileHandle
  putStrLn ("...File " ++ fileName ++ " closed.")

-- Load a LangLang file
loadLLFile :: [String] -> IO ()

loadLLFile [] = do
  putStrLn "No arguments supplied! Continueing with an example test case."
  semanticDBInit
  withCString "dom" c_DeclareOpenDomain
  fooSymbol <- withCString "foo" c_DeclareSymbol
  valSymbol <- withCString "val" c_DeclareSymbol
  withCString "dom" c_CloseDomain
  c_DebugOutputEnvironment
  
loadLLFile args = do
  let fileName = head args
  putStrLn ("Translating file: " ++ fileName ++ "..." )
  (parseLLFile fileName) `catch` (\e -> putStrLn "Error! Could not open file for translation.")

-- The main application entry-point
main = do 
  putStrLn "Welcome to Poet, your friendly semantic translator!"
  args <- getArgs
  putStrLn (show args)
  loadLLFile args

