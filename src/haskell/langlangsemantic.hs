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

-- Declarations
declaration :: Declaration -> IO ()

declaration (Definition (IdDecl domain) (EqId (IdDecl codomain))) = do
  domainSymbol   <- withCString domain c_DeclareOpenDomain
  codomainSymbol <- withCString codomain c_DeclareSymbol
  withCString domain c_CloseDomain

declaration (Definition (IdDecl domain) (EqDecl codomain)) = do
  domainSymbol <- withCString domain c_DeclareOpenDomain
  declaration codomain
  withCString domain c_CloseDomain

declaration (Definition (IdDecl domain) (EqSet codomain)) = do
  domainSymbol <- withCString domain c_DeclareOpenDomain
  msum (map declaration codomain)
  withCString domain c_CloseDomain

declaration (Atom (IdDecl codomain)) = do 
  withCString codomain c_DeclareSymbol
  return ()

-- Semantic analysis
semanticAnalysis :: Maybe [Declaration] -> IO ()
semanticAnalysis Nothing              = putStrLn("SemanticAnalysis... Parsing failed.")
semanticAnalysis (Just [])            = putStrLn("SemanticAnalysis... No declarations in the file.")
semanticAnalysis (Just declarations)  = do
  semanticDBInit
  putStrLn "SemanticAnalysis..."
  putStrLn ("Number of declarations = " ++ (show $ length declarations))
  msum (map declaration declarations)
  putStrLn "...SemanticAnalysis done"



