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
data IdDecl = IdDecl String

data Relation = EqId IdDecl 
              | EqDecl Declaration 
              | EqSet [Declaration]

data Literal = Character Char

data Expression = SelectionDisjunct          IdDecl Expression 
                | SelectionExclusiveDisjunct IdDecl Expression
                | SelectionConjunct          IdDecl Expression
                | SelectionStrictConjunct    IdDecl Expression
                | MutationDisjunct           IdDecl Declaration 
                | MutationExclusiveDisjunct  IdDecl Declaration
                | MutationConjunct           IdDecl Declaration
                | MutationStrictConjunct     IdDecl Declaration

data Declaration = Atom IdDecl 
                 | Query Expression 
                 | Definition IdDecl Relation

