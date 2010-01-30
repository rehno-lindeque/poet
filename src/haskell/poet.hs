------------------------------------------------------------------------------
--
--    POET.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
module Main( main ) where

{-                               DOCUMENTATION                              -}
{-
    Poet: Your friendly semantic translator!

    TODO: This project might be changed to either
    * WebPoet: Your friendly LangLang web server
    * PoetCMS: Your friendly content management system
-}

{-                                 MODULES                                  -}
-- Standard
import System( getArgs )
import System.IO
import System.IO.Error
import Control.Monad
import Foreign.C.String

-- Debugging
--import Debug.Trace

-- Poet
import Parser

-- Langlang
import LangLang.Data
import LangLang.Parser
import LangLang.Semantic

-- OpenSemanticDB
import OSIX.SemanticDB

{-                              IMPLEMENTATION                              -}
-- Translate file
parseLLFile :: String -> IO ()
parseLLFile fileName = do
  fileHandle <- (openFile fileName ReadMode)
  fileContents <- hGetContents fileHandle
  (semanticAnalysis $ LangLang.Parser.parse fileContents)
  --putStrLn ("Remainder: " ++ (LangLang.Parser.showRemainder fileContents))
  hClose fileHandle
  putStrLn ("...File '" ++ fileName ++ "' closed.\n")
  c_SemanticDBDebugInit
  c_DebugOutputEnvironment
  return ()

-- Load a LangLang file
loadLLFile :: [String] -> IO ()

loadLLFile [] = do
  putStrLn "No arguments supplied! Continueing with an example test case."

loadLLFile args = do
  let fileName = head args
  putStrLn ("Translating file '" ++ fileName ++ "'..." )
  (parseLLFile fileName) `catch` (\e -> putStrLn "Error! Could not open file for translation.")

-- The main application entry-point
main = do
  putStrLn "Welcome to Poet, your friendly semantic translator!"
  args <- getArgs
  putStrLn (show args)
  loadLLFile args

