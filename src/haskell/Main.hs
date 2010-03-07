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
import LangLang.Debug

-- OpenSemanticDB
import OSIX.SemanticDB

{-                              IMPLEMENTATION                              -}
-- Translate file
parseLLFile :: String -> IO ()
parseLLFile fileName = do
  fileHandle <- (openFile fileName ReadMode)
  fileContents <- hGetContents fileHandle
  case LangLang.Parser.parse fileContents of
    --Just abstractSyntaxTree -> semanticAnalysis abstractSyntaxTree
    Just abstractSyntaxTree -> printAST abstractSyntaxTree >> semanticAnalysis abstractSyntaxTree
    Nothing                 -> putStrLn "Parsing failed!"
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

-- Evaluate a LangLang file
evalLLSymbol :: String -> IO ()

evalLLSymbol symbol = do
  rootId  <- withCString symbol c_GlobalSymbol
  evalObj <- c_BeginEvaluation rootId
  evalLL
  c_EndEvaluation evalObj

evalLL :: IO SemanticId

evalLL = do
  resultId <- c_Eval
  if resultId == c_SEMANTICID_INVALID then return c_SEMANTICID_INVALID else evalLL

-- The main application entry-point
main = do
  putStrLn "Welcome to Poet, your friendly semantic translator!"
  args <- getArgs
  putStrLn (show args)
  loadLLFile args
  evalLLSymbol "test"

