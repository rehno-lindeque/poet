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
---- Main functions
--parserLL :: Parser [Declaration]
--parserLL = Parser.repeating (LangLang.Parser.padding -# LangLang.Parser.declaration)
--parserLL = LangLang.Parser.declaration >-> (:[])

-- Main parsing function
--parseString :: String -> Maybe [Declaration]
--parseString input =  fst `liftM` (LangLang.Parser.parse input)

--showRem input = case snd `liftM` (parserLL input) of
--  Nothing    -> putStrLn "<nothing>"
--  (Just str) -> putStrLn (show str)

-- Translate file
parseLLFile :: String -> IO ()
parseLLFile fileName = do
  fileHandle <- (openFile fileName ReadMode)
  fileContents <- hGetContents fileHandle
  (semanticAnalysis $ LangLang.Parser.parse fileContents)
  --showRem fileContents
  hClose fileHandle
  putStrLn ("...File '" ++ fileName ++ "' closed.")

-- Load a LangLang file
loadLLFile :: [String] -> IO ()

loadLLFile [] = do
  putStrLn "No arguments supplied! Continueing with an example test case."
  semanticDBInit
  domSymbol <- withCString "dom" c_DeclareOpenDomain
  fooSymbol <- withCString "foo" c_DeclareSymbol
  valSymbol <- withCString "val" c_DeclareSymbol
  withCString "dom" c_CloseDomain
  domfoo <- ((withCString "foo" c_GlobalSymbol) >>= (c_SelectRelation domSymbol))
  domfooSymbol <- withCString "domfoo" c_GlobalSymbol
  c_DeclareRelation domfooSymbol domfoo
  c_DebugOutputEnvironment
  
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
  --case (parseString "Hello = you you galaxy!") of 
  --  Nothing              -> putStrLn "...Parsing failed"
  --  (Just (IdDecl word)) -> putStrLn ("...Done parsing. First identifier was: " ++ word)

