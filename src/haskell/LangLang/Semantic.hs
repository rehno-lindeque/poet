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

---- Queries
query :: IO () -> Expression -> Expression -> IO SemanticId
query queryFunction domain argument = do
  c_BeginQuery
  domainSymbol   <- (expression domain)
  queryFunction
  argumentSymbol <- (expression argument)
  --querySymbol    <- argumentSymbol
  c_EndQuery

setQuery :: IO () -> [Expression] -> Expression -> IO SemanticId
setQuery queryFunction domain argument = do
  c_BeginQuery
  anonDomainSymbol   <- c_AnonymousSymbol
  c_OpenHiddenDomain anonDomainSymbol
  sequence_ $ expression `map` domain
  c_CloseDomain anonDomainSymbol
  queryFunction
  argumentSymbol <- (expression argument)
  --querySymbol    <- argumentSymbol
  c_EndQuery

---- Expressions
expression :: Expression -> IO SemanticId

-- Definition expression
expression (Definition domain codomain) = do
  putStrLn $ "Definition " ++ show domain
  domainSymbol <- withCString domain c_DeclareOpenDomain
  expression codomain
  c_CloseDomain domainSymbol
  return domainSymbol

-- Atom expression
expression (Atom codomain) = do
  putStrLn $ "Atom " ++ show codomain
  withCString codomain c_DeclareSymbol

-- Set expression
expression (Set []) = return c_SEMANTICID_INVALID

expression (Set codomain) = do
  putStrLn "Set "
  sequence (expression `map` codomain)
  return c_SEMANTICID_INVALID

-- Set-Query expression
expression (SetQuery domain SelectionDisjunct argument) = do
  putStrLn "Set-SelectionDisjunct"
  setQuery c_SelectionDisjunct domain argument

expression (SetQuery domain SelectionExclusiveDisjunct argument) = do
  putStrLn "Set-SelectionExclusiveDisjunct"
  setQuery c_SelectionExclusiveDisjunct domain argument

expression (SetQuery domain SelectionConjunct argument) = do
  putStrLn "Set-SelectionConjunct"
  setQuery c_SelectionConjunct domain argument

expression (SetQuery domain SelectionStrictConjunct argument) = do
  putStrLn "Set-SelectionStrictConjunct"
  setQuery c_SelectionStrictConjunct domain argument

expression (SetQuery domain MutationDisjunct argument) = do
  putStrLn "Set-MutationDisjunct"
  setQuery c_MutationDisjunct domain argument

expression (SetQuery domain MutationExclusiveDisjunct argument) = do
  putStrLn "Set-MutationExclusiveDisjunct"
  setQuery c_MutationExclusiveDisjunct domain argument

expression (SetQuery domain MutationConjunct argument) = do
  putStrLn "Set-MutationConjunct"
  setQuery c_MutationConjunct domain argument

expression (SetQuery domain MutationStrictConjunct argument) = do
  putStrLn "Set-MutationStrictConjunct"
  setQuery c_MutationStrictConjunct domain argument

-- Query expression
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
{--
semanticAnalysis :: Maybe [Expression] -> IO ()
semanticAnalysis Nothing              = putStrLn("SemanticAnalysis... Parsing failed.")
semanticAnalysis (Just [])            = putStrLn("SemanticAnalysis... No declarations in the file.")
semanticAnalysis (Just declarations)  = do--}
semanticAnalysis :: [Expression] -> IO ()
semanticAnalysis []           = putStrLn("SemanticAnalysis... No declarations in the file.")
semanticAnalysis declarations = do
  semanticDBInit
  putStrLn "SemanticAnalysis..."
  putStrLn ("Number of declarations = " ++ (show $ length declarations))
  sequence $ expression `map` declarations
  putStrLn "...SemanticAnalysis done"



