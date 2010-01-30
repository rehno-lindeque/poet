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

data Relation = EqId   String
              | EqSet  [Expression]
              | EqExpr Expression
              | EqLit  Literal

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
                | Definition  String        Expression

