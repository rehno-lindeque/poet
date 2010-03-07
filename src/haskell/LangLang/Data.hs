------------------------------------------------------------------------------
--
--    LANGLANGDATA.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
module LangLang.Data where

{-                               DOCUMENTATION                              -}
{-
    Token for the LangLang programming language (represented as algebraic 
    data types)
-}

{-                                INTERFACE                                 -}
{- Grammar tokens -}

{-data Relation = EqId   String
              | EqSet  [Expression]
              | EqExpr Expression
              | EqLit  Literal
              | EqSetQuery [Expression] Expression-}

data Literal = LitChar    Char 
             | LitString  String
             | LitInt     Int
             | LitFloat   Float
             | LitDouble  Double

data QueryExpr = SelectionDisjunct          --Expression
               | SelectionExclusiveDisjunct --Expression
               | SelectionConjunct          --IdDecl
               | SelectionStrictConjunct    --IdDecl
               | MutationDisjunct           --IdDecl
               | MutationExclusiveDisjunct  --IdDecl
               | MutationConjunct           --IdDecl
               | MutationStrictConjunct     --IdDecl

data Expression = Atom        String
                | Set         [Expression]
                | Query       Expression    QueryExpr   Expression
                | SetQuery    [Expression]  QueryExpr   Expression
                | Definition  String        Expression

{-instance Show Expression where  
  show (Atom string)     = "Atom " ++ string
  show (Set expressions) = "Set " ++ show expressions
  show Query expressionA q expressionB = "Query"
  show SetQuery expressionsA q expressionB = "SetQuery"
  show Definition string expression = "Definition " ++ string-}
