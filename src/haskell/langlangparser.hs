------------------------------------------------------------------------------
--
--    LANGLANGPARSER.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
module LangLang.Parser(parse) where

{-                               DOCUMENTATION                              -}
{-
    Parsers for the LangLang programming language.
-}

{-                                 MODULES                                  -}
-- Poet
import Parser

-- LangLang
import LangLang.Data

-- Standard
import Prelude hiding (repeat, iterate)
import Data.Char
import Control.Monad(liftM)

{-                             PUBLIC INTERFACE                             -}
parse :: String -> Maybe [Declaration]


{-                             PRIVATE INTERFACE                            -}
-- Tokenizing parsers
tokenize :: Parser a -> Parser a
tokenId :: Parser IdDecl
tokenChar :: Char -> Parser Char

-- LangLang parsers
comment :: Parser String
blockcomment :: Parser String
padding :: Parser [String]
word :: Parser String
set :: Parser [Declaration]
declarationRHS :: Parser Relation
declaration :: Parser Declaration
globalDeclaration :: Parser Declaration

{-                              IMPLEMENTATION                              -}

{- Helper functions -}
isTokenChar :: Char -> Bool
isTokenChar c = (isAlphaNum c) || (c `elem` ['\'', '_', '-'])

--takePair :: (a -> b -> c) -> (a, b) -> c
--takePair func = \(x,y) -> func x y

-- Tokenizing parsers
tokenize parser = parser #- padding

tokenChar c = tokenize $ matchChar c

tokenId = tokenize word >-> IdDecl

-- LangLang parsers

comment = matchChar '#' -# Parser.repeat ( char ? (\c -> not $ isNewline c) ) #- Parser.repeat space

blockcomment = matchString "(#" -# char #?- matchString "#)"

padding = Parser.repeat ((oneOrMore space) ! comment ! blockcomment) 

word = (oneOrMore (char ? isTokenChar))

set = tokenChar '[' -# (declaration ! tokenId >-> Atom) #?- tokenChar ']'

--expression :: Parser Expression
--expression = 

declarationRHS = declaration >-> EqDecl ! tokenId >-> EqId ! set >-> EqSet

declaration = tokenId #- tokenChar '=' # declarationRHS >-> \(x,y) -> Definition x y

globalDeclaration = declaration

parse input = fst `liftM` ((padding -# (repeat globalDeclaration)) input)




