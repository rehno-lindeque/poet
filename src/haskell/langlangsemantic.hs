------------------------------------------------------------------------------
--
--    LANGLANGSEMANTIC.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module LangLang.Semantic where

{-                               DOCUMENTATION                              -}
{-
    Semantic analysis for the LangLang programming language
-}

{-                                 MODULES                                  -}
-- LangLang
import LangLang.Data

-- Standard
import Control.Monad
import Control.Monad.Error
import Foreign.C.String

-- OpenSemanticDB
import OSIX.SemanticDB

{-                              IMPLEMENTATION                              -}

-- Queries
query :: (SemanticId -> IO SemanticId) -> Expression -> Expression -> IO SemanticId
query queryFunction domain argument = do
  c_BeginQuery
  domainSymbol   <- (expression domain)
  argumentSymbol <- (expression argument)
  querySymbol    <- queryFunction argumentSymbol
  c_EndQuery
  return querySymbol

-- Expressions
expression :: Expression -> IO SemanticId

expression (Definition domain codomain) = do
  putStrLn $ "Definition " ++ show domain
  domainSymbol <- withCString domain c_DeclareOpenDomain
  expression codomain
  withCString domain c_CloseDomain
  return domainSymbol

expression (Atom codomain) = do
  putStrLn $ "Atom " ++ show codomain
  withCString codomain c_DeclareSymbol

expression (Set []) = return c_SEMANTICID_INVALID

expression (Set codomain) = do
  putStrLn "Set "
  sequence (map expression codomain)
  return c_SEMANTICID_INVALID

expression (Query domain SelectionDisjunct argument) = do
  putStrLn "SelectionDisjunct "
  query c_SelectionDisjunct domain argument

expression (Query domain SelectionExclusiveDisjunct argument) = do
  putStrLn "SelectionExclusiveDisjunct"
  query c_SelectionExclusiveDisjunct domain argument

expression (Query domain SelectionConjunct argument) = do
  putStrLn "SelectionConjunct"
  query c_SelectionConjunct domain argument

expression (Query domain SelectionStrictConjunct argument) = do
  putStrLn "SelectionStrictConjunct"
  query c_SelectionStrictConjunct domain argument

expression (Query domain MutationDisjunct argument) = do
  putStrLn "MutationDisjunct"
  query c_MutationDisjunct domain argument

expression (Query domain MutationExclusiveDisjunct argument) = do
  putStrLn "MutationExclusiveDisjunct"
  query c_MutationExclusiveDisjunct domain argument

expression (Query domain MutationConjunct argument) = do
  putStrLn "MutationConjunct"
  query c_MutationConjunct domain argument

expression (Query domain MutationStrictConjunct argument) = do
  putStrLn "MutationStrictConjunct"
  query c_MutationStrictConjunct domain argument

-- Semantic analysis
semanticAnalysis :: Maybe [Expression] -> IO ()
semanticAnalysis Nothing              = putStrLn("SemanticAnalysis... Parsing failed.")
semanticAnalysis (Just [])            = putStrLn("SemanticAnalysis... No declarations in the file.")
semanticAnalysis (Just declarations)  = do
  semanticDBInit
  putStrLn "SemanticAnalysis..."
  putStrLn ("Number of declarations = " ++ (show $ length declarations))
  sequence (map expression declarations)
  putStrLn "...SemanticAnalysis done"



