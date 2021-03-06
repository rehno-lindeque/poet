------------------------------------------------------------------------------
--
--    PARSER.HS
--
--    Copyright © 2009-2010, Rehno Lindeque. All rights reserved.
--
------------------------------------------------------------------------------
module Parser where

{-                               DOCUMENTATION                              -}
{-
    USAGE:
      Parsers are built up using the basic parser functions with the
      following parser combinators:

        ?   Match: Applies a parser to the input if the condition following the combinator holds.
            (e.g. char ? (== '+'))

        !   Alternative: Tries each of the given parsers in sequence  until one or none succeeds.
            (e.g. parserA ! parserB ! parserC)

        >-> Transform: Applies a function to the result of the parsing operation.
            (e.g. word >-> convertToInt)

        #   Chain combinator: Chain together parsers and their results.
            (e.g. Char 'e' # char 'n' # char 'd')

        -#  Hide-Chain: Chain together parsers and ignoring preceding results.
            (e.g. char '?' -# space -# word )

        #-  Chain-Hide: Chain together parsers and ignoring results from the following parsers.
            (e.g. word #- space #- ')' )

        #-  Chain-Hide: Chain together parsers and ignoring results from the following parsers.
            (e.g. word #- space #- ')' )

        ##! Repeat-Chain: Repeat a parser until the parser following it can be parsed.
            (e.g. (word # ',') #? (word # ')'))

        -##! Hide-Repeat-Chain: Hide all output until the second parser is successful.
            (e.g. )

        ##!- Repeat-Chain-Hide: Repeat a parser until a hidden parser is recognized.
            (e.g. )

        #!>-> Chained-Lookahead-Transform: Look ahead using a chained parser to select one of two transform functions. 
            If the second parser fails then the first transform function is used on the output of the first parser, otherwise the second 
            transform function is executed on the output of both parsers.

    REFERENCES:

     [1] FUNCTIONAL PEARLS: Monadic Parsing in Haskell,
         Graham Hutton (University of Nottingham), Erik Meijer (University of Utrecht)
         http://www.cs.nott.ac.uk/Department/Staff/gmh/bib.html#pearl

     [2] Parsing with Haskell
         Lennart Andersson
         Computer Science
         Lund University
         October 28, 2001
-}

{-                                 MODULES                                  -}
-- Standard
import Prelude hiding (repeat, iterate)
import Control.Monad
import Data.Char

{-                                INTERFACE                                 -}

{- Parser type -}

type Parser a = String -> Maybe (a,String)

{- Parser combinators -}

-- Operator precedence
infixl 3 !
infixl 5 >->
infixl 6 #
infixl 6 -#
infixl 6 #-
infix 7 ##!
infix 7 -##!
infix 7 ##!-
infix 7 #!>->
infix 8 ?

-- Match combinator
(?) :: Parser a -> (a -> Bool) -> Parser a

-- Alternative combinator
(!) :: Parser a -> Parser a -> Parser a

-- Transform combinator
(>->) :: Parser a -> (a -> b) -> Parser b

-- Chain combinator
(#) :: Parser a -> Parser b -> Parser (a, b)

-- Hide-Chain combinator
(-#) :: Parser a -> Parser b -> Parser b

-- Chain-Hide combinator
(#-) :: Parser a -> Parser b -> Parser a

-- Repeat-Chain combinator
(##!) :: Parser a -> Parser b -> Parser ([a],b)

-- Hide-Repeat-Chain combinator
(-##!) :: Parser a -> Parser b -> Parser b

-- Repeat-Chain-Hide combinator
(##!-) :: Parser a -> Parser b -> Parser [a]


-- Chained-Lookahed-Transform combinator
(#!>->) :: Parser a -> Parser b -> (a -> c) -> ((a, b) -> c) -> Parser c

{- Parser transformers -}

-- Repeat a parser until it fails
repeat :: Parser a -> Parser [a]

-- Repeat a parser at least once
oneOrMore :: Parser a -> Parser [a]

-- Iterate the parser a fixed number of times
iterate :: Parser a -> Int -> Parser [a]

{- Standard parsers -}

-- Match any character
char :: Parser Char

-- Match a specific character
matchChar :: Char -> Parser Char

-- Match a specific string
matchString :: String -> Parser String

-- Match any whitespace character
space :: Parser Char

{-                              IMPLEMENTATION                              -}
{- Helper functions -}

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

consFirst :: (a, ([a], b)) -> ([a], b)
consFirst (a, (as, b)) = (a:as, b)

returnParser :: a -> Parser a
returnParser a cs = Just(a,cs)

--failParser :: Parser a

isNewline :: Char -> Bool
isNewline c = c `elem` ['\n', '\r']

{- Parser combinators -}
-- Match combinator
(parser ? predicate) input =
  case (parser input) of
    Nothing               -> Nothing
    Just (result, input') -> if (predicate result) then Just (result, input') else Nothing

-- Alternative combinator
(choiceA ! choiceB) input =
  case (choiceA input) of
    Nothing -> choiceB input
    resultA -> resultA

-- Transform combinator
(parser >-> transform) input =
  case (parser input) of
    Nothing               -> Nothing
    Just (result, input') -> Just (transform result, input')

-- Chain combinator
(parserA # parserB) input =
  case (parserA input) of
    Nothing                 -> Nothing
    Just (resultA, input')  ->
      case parserB input' of
        Nothing                 -> Nothing
        Just (resultB, input'') -> Just ((resultA, resultB), input'')

-- Hide-Chain combinator
(parserA -# parserB) input = (parserA # parserB >-> snd) input

-- Chain-Hide combinator
(parserA #- parserB) input = (parserA # parserB >-> fst) input

-- Repeat-Chain combinator
(parserA ##! parserB) input = ( (parserB >-> (\b-> ([],b))) ! ((parserA # (parserA ##! parserB)) >-> consFirst) ) input

-- Hide-Repeat-Chain combinator
(parserA -##! parserB) input = ((parserA ##! parserB) >-> snd) input

-- Repeat-Chain-Hide combinator
(parserA ##!- parserB) input = ((parserA ##! parserB) >-> fst) input

-- Chained-Lookahed-Transform combinator
(parserA #!>-> parserB) transformA transformB input = 
  case parserA input of
    Nothing                -> Nothing
    Just (resultA, input') -> 
      case parserB input' of
        Nothing                 -> Just (transformA resultA, input')
        Just (resultB, input'') -> Just (transformB (resultA, resultB), input'')

{- Parser implementation -}
repeat parser = ( ( parser # (repeat parser) ) >-> cons ) ! (returnParser [])

oneOrMore parser = (parser # (repeat parser)) >-> cons

iterate parser 0           = returnParser []
iterate parser nIterations = parser # iterate parser (nIterations - 1) >-> cons

char (c:cs) = Just (c, cs)
char []     = Nothing

matchChar c = char ? (==c)

matchString string = iterate char (length string) ? (== string)

space = char ? isSpace

