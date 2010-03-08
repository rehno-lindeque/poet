------------------------------------------------------------------------------
--
--    LANGLANGPARSER.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
module LangLang.Parser(parse, showRemainder) where

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
parse :: String -> Maybe [Expression]


{-                             PRIVATE INTERFACE                            -}
-- Tokenizing parsers
tokenize :: Parser a -> Parser a
tokenId :: Parser String
tokenChar :: Char -> Parser Char

-- LangLang parsers
comment :: Parser String
blockcomment :: Parser String
padding :: Parser [String]
word :: Parser String
set :: Parser [Expression]
--relationRHS :: Parser Relation
declaration :: Parser Expression
globalDeclaration :: Parser Expression

{-                              IMPLEMENTATION                              -}

{- Helper functions -}
isTokenChar :: Char -> Bool
isTokenChar c = (isAlphaNum c) || (c `elem` ['\'', '_', '-'])

--takePair :: (a -> b -> c) -> (a, b) -> c
--takePair func = \(x,y) -> func x ycd proj

-- Tokenizing parsers
tokenize parser = parser #- padding

tokenChar c = tokenize $ matchChar c

tokenString s = tokenize $ matchString s

tokenId = tokenize word

-- LangLang parsers

comment = matchChar '#' -# Parser.repeat ( char ? (\c -> not $ isNewline c) ) #- Parser.repeat space

blockcomment = matchString "(#" -# char ##!- matchString "#)"

padding = Parser.repeat ((oneOrMore space) ! comment ! blockcomment)

word = (oneOrMore (char ? isTokenChar))

set = tokenChar '{' -# (declaration ! tokenId >-> Atom) ##!- tokenChar '}'

queryExpr :: Parser QueryExpr
queryExpr = tokenString ".." -# (returnParser MutationConjunct)
          ! tokenString "::" -# (returnParser MutationStrictConjunct)
          ! tokenString "~~" -# (returnParser MutationDisjunct)
          ! tokenString "!!" -# (returnParser MutationExclusiveDisjunct)
          ! tokenChar '.' -# (returnParser SelectionConjunct)
          ! tokenChar ':' -# (returnParser SelectionStrictConjunct)
          ! tokenChar '~' -# (returnParser SelectionDisjunct)
          ! tokenChar '!' -# (returnParser SelectionExclusiveDisjunct)

--relationRHS = expression >-> EqExpr ! tokenId >-> EqId ! (set  >-> EqSet)

declaration = expression

subexpression :: Parser Expression
subexpression = tokenId >-> Atom ! tokenChar '(' -# expression #- tokenChar ')'

expression :: Parser Expression
expression = ((set #!>-> (queryExpr # expression)) Set (\(x,(y,z)) -> SetQuery x y z))
           ! tokenId #- tokenChar '=' # expression >-> (\(x,y) -> Definition x y)
           ! ((subexpression #!>-> (queryExpr # expression)) id (\(x,(y,z)) -> Query x y z))

--           ! subexpression # queryExpr # expression >-> (\((x,y),z) -> Query x y z)
--           ! subexpression

globalDeclaration = declaration

parse input = fst `liftM` ((padding -# (repeat globalDeclaration)) input)

showRemainder :: String -> String
showRemainder input = case snd `liftM` ((padding -# (repeat globalDeclaration)) input) of
  Nothing   -> "<nothing>"
  Just str  -> str

