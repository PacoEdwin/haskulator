module Token where

import Data.Char
import Data.List

data Token = TInt Int
           | TAdd
           | TSub
           | TMult
           | TDiv
           | TOpen
           | TClose
           | TError
  deriving (Show, Eq)

-- Converts string to list of tokens
tokenize :: String -> [Token]
tokenize str = combineTokens $ map charToToken processStr
  where
    processStr = filter (/=' ') str

-- Combines consequitive Int tokens into single Int token
combineTokens :: [Token] -> [Token]
combineTokens [] = []
combineTokens list = concat $ map merger groupInts
  where
    groupInts = groupBy (\a b -> isTokenInt a && isTokenInt b) list
    merger list = if (length list > 1) then mergeInts list else list


-- Merges multiple Int Tokens into a single Int Token
-- based on https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
mergeInts :: [Token] -> [Token]
mergeInts ints 
    | and (map isTokenInt ints) = [TInt (read numStr::Int)]
        where numStr = concatMap show $ fmap (\(TInt n) -> n) ints
mergeInts _ = [TError]

-- | Function to check whether a given token is Operator Token
isTokenOp :: Token -> Bool
isTokenOp token = elem token operators

-- Checks whether a given token is Integer Token
isTokenInt :: Token -> Bool
isTokenInt (TInt n) = True
isTokenInt _      = False

-- Converts Characters into Operators Tokens
charToToken :: Char -> Token
charToToken c 
    | isDigit c = TInt (charToInt c)
charToToken '+' = TAdd
charToToken '-' = TSub
charToToken '*' = TMult
charToToken '/' = TDiv
charToToken '(' = TOpen
charToToken ')' = TClose
charToToken _   = TError

-- Converts Characters into single digit Integer Tokens
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

operators = [TAdd, TSub, TMult, TDiv]