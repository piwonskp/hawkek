#!/usr/bin/env runhaskell
{-# LANGUAGE TupleSections #-}
import System.IO
import System.Environment
import Control.Monad
import Data.Char
import Data.Maybe
import Text.Read
import Control.Arrow


data KeywordType = SubClassOf | EquivalentClasses | DisjointClasses
  | SameIndividual | DifferentIndividuals | ObjectIntersectionOf | ObjectUnionOf
  | ObjectComplementOf | ObjectOneOf
  deriving (Show, Eq, Ord, Read)

data Token = Keyword KeywordType | Identifier String | LP | RP
  deriving (Show, Eq, Ord)

lexer :: String -> [Token]
lexer [] = []
lexer (a:c)
  | isSpace a = lexer c
  | a == '(' = LP : lexer c
  | a == ')' = RP : lexer c
  | a == ':' = lexIdentifier $ getWord c
  | isAlpha a = lexKeyword $ first (a :) $ getWord c
  where
    isKeyword a = isJust (readMaybe a :: Maybe KeywordType)
    getWord = span isAlpha
    lexKeyword (word, rest) = getKeywordToken word : lexer rest
    getKeywordToken word
      | isKeyword word = (Keyword (read word :: KeywordType))
      | otherwise = error "Unknown keyword"
    lexIdentifier (word, rest) = Identifier word : lexer rest


newtype T_Identifier = T_Identifier String
  deriving (Show)

data Expr = E_Identifier String | E_ObjectIntersectionOf Expr Expr [Expr]
  | E_ObjectUnionOf Expr Expr [Expr] | E_ObjectComplementOf Expr
  | E_ObjectOneOf T_Identifier [T_Identifier]
  deriving (Show)

data AST = T_SubClassOf Expr Expr | T_DisjointClasses Expr Expr [Expr]
  | T_SameIndividual T_Identifier T_Identifier [T_Identifier]
  | T_DifferentIndividuals T_Identifier T_Identifier [T_Identifier]
  | T_EquivalentClasses Expr Expr [Expr]
  deriving (Show)

opt :: ([Token] -> (a, [Token])) -> [Token] -> ([a], [Token])
opt _ (RP:ts) = ([], ts)
opt fun ts = (fst result : fst nextResults, remainingTokens)
  where
    result = fun ts
    nextResults = opt fun $ snd result
    remainingTokens = snd nextResults

identifier (Identifier s) = T_Identifier s
identifierFromList :: [Token] -> (T_Identifier, [Token])
identifierFromList (s:ts) = (identifier s, ts)
getOptIdentifiers :: [Token] -> ([T_Identifier], [Token])
getOptIdentifiers = opt identifierFromList

optExprs :: [Token] -> ([Expr], [Token])
optExprs = opt expr

dropRParen (RP:ts) = ts

expr :: [Token] -> (Expr, [Token])
expr (Identifier s:ts) = (E_Identifier s, ts)
expr (Keyword ObjectIntersectionOf:LP:ts) = e2 E_ObjectIntersectionOf ts
expr (Keyword ObjectUnionOf:LP:ts) = e2 E_ObjectUnionOf ts
expr (Keyword ObjectComplementOf:LP:ts) =
  (E_ObjectComplementOf *** dropRParen) (expr ts)
expr (Keyword ObjectOneOf:LP:t:ts) = (E_ObjectOneOf (identifier t) (optIds), ts')
  where
    optRes = getOptIdentifiers ts
    optIds = fst optRes
    ts' = snd optRes

sndE = fst . snd
trdE = fst . snd . snd
trdT = snd . snd . snd

expr'' = (second expr) . expr
expr''' :: [Token] -> (Expr, (Expr, ([Expr], [Token])))
expr''' = ((second >>> second) optExprs) . expr''
e2 :: (Expr -> Expr -> [Expr] -> Expr) -> [Token] -> (Expr, [Token])
e2 e ts = (e expr1 expr2 optExprs, restTokens)
  where
    ast = expr''' ts
    expr1 = fst ast
    expr2 = sndE ast
    optExprs = trdE ast
    restTokens = trdT ast
t2 :: (Expr -> Expr -> [Expr] -> AST) -> [Token] -> [AST]
t2 type_ ts = type_ expr1 expr2 optExprs : parser restTokens
  where
    ast = expr''' ts
    expr1 = fst ast
    expr2 = sndE ast
    optExprs = trdE ast
    restTokens = trdT ast

i2 typ (t:t1:ts) = typ (identifier t) (identifier t1) (optIdentifiers) : recurse
  where
    optRes = getOptIdentifiers ts
    optIdentifiers = fst optRes
    recurse = parser $ snd optRes

parser :: [Token] -> [AST]
parser [] = []
parser (Keyword SubClassOf:LP:ts) = T_SubClassOf (fst ast) (sndE ast) : recurse
  where
    ast = expr'' ts
    recurse = parser $ dropRParen $ snd $ snd ast
parser (Keyword DisjointClasses:LP:ts) = t2 T_DisjointClasses ts
parser (Keyword SameIndividual:LP:ts) = i2 T_SameIndividual ts
parser (Keyword DifferentIndividuals:LP:ts) = i2 T_DifferentIndividuals ts
parser (Keyword EquivalentClasses:LP:ts) = t2 T_EquivalentClasses ts
parser t = error $ "Token: " ++ show t

main = do
  print . (parser . lexer) =<< (readFile . head) =<< getArgs
  print "Success!"
