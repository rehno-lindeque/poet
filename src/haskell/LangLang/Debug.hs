------------------------------------------------------------------------------
--
--    LANGLANG.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module LangLang.Debug( printAST ) where

{-                               DOCUMENTATION                              -}
{-
    Semantic analysis for the LangLang programming language
-}

{-                                 MODULES                                  -}
-- LangLang
import LangLang.Data
import System.IO

-- Standard

{-                              IMPLEMENTATION                              -}


{-instance Show Relation where
  show (EqId string) = "EqId " ++ string
  show (EqSet expressions) = "EqSet " ++ show expressions
  show (EqExpr expression) = "EqExpr " ++ show expression
  show (EqLit literal) = "EqLit " ++ show literal
  show (EqSetQuery expressions expression) = "EqSetQuery " ++ show expressions ++ show expression-}

instance Show Literal where
  show (LitChar char) = "Character " ++ show char
  show (LitString string) = "String " ++ string
  show (LitInt int) = "Int " ++ show int
  show (LitFloat float) = "Float " ++ show float
  show (LitDouble double) = "Double " ++ show double

instance Show QueryExpr where
  show SelectionDisjunct = " ~ "
  show SelectionExclusiveDisjunct = " ! "
  show SelectionConjunct = " . "
  show SelectionStrictConjunct = " : "
  show MutationDisjunct = " ~~ "
  show MutationExclusiveDisjunct = " !! "
  show MutationConjunct = " .. "
  show MutationStrictConjunct = " :: "

instance Show Expression where
  show (Atom string)  = "Atom " ++ string
  show (Set expressions) = "Set " ++ (show expressions)
  show (Query expressionA queryExpr expressionB) = "Query " ++ show expressionA ++ show queryExpr ++ show expressionB
  show (SetQuery expressions queryExpr expression) = "SetQuery " ++ show expressions ++ show queryExpr ++ show expression
  show (Definition string expression) = "Definition " ++ string ++ " ( " ++ show expression ++ " )"

printASTExpression :: Expression -> IO ()
printASTExpression expression = putStrLn $ show expression

printAST :: [Expression] -> IO ()
printAST expressions =
  sequence_ $ printASTExpression `map` expressions


