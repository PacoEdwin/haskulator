module Calculator where

import Token
import Data.Maybe


------------------------------------------------------------------------------------------------------
data Exp = Nil | Val Int | Bracket Exp
    | Branch Exp Opr Exp 
        deriving (Show, Ord, Eq)

data Opr = Add | Sub | Mult | Div
  deriving (Show, Ord, Eq)

data Error = ErrorExp
    | BracketsError
    | OperatorError
    | TokenError
    | ExpInBracketsError
        deriving (Show, Eq)
------------------------------------------------------------------------------------------------------

evaluateExpIO :: IO ()
evaluateExpIO = do
    str <- getLine
    case parseExp str of
        Left [errorStr] -> putStr.unlines $ [("Error! " ++ errorStr)]
        Right exp -> case evaluateExp exp of
            Nothing -> putStrLn("You broke my calculator!")
            Just val -> do
                putStrLn(show val)
                evaluateExpIO

-- Evaluates expression
evaluateExp :: Exp -> Maybe Int
evaluateExp (Val n) =  Just n
evaluateExp (Bracket exp) = evaluateExp exp
evaluateExp (Branch l op r)
    = Just $ (opToArthFunc op) (fromJust $ evaluateExp l) (fromJust $ evaluateExp r)

-- Converts string to arithmetic expression
parseExp :: String -> Either [String] Exp
parseExp str = case buildExp tokenizeStr of
    Left error -> Left [getErrorStr error]
    Right exp -> Right exp
    where 
        tokenizeStr = tokenize str
        noSpaceStr = filter (/=' ') str

-- | Takes an error and returns an error message accordingly
getErrorStr :: Error -> String
getErrorStr error = case error of
    ErrorExp -> " Invalid Expression"
    BracketsError -> " Bracket Mismatch"
    TokenError -> " Invalid Operator or Character in Arithmetic Expression"
    OperatorError -> " Invalid arrangement of operators"
    ExpInBracketsError -> " Invalid Expression in Brackets"

-- | Takes an expression string and error position as Int value
-- and returns the same string with error highlighted
showError :: String -> Int -> String
showError str n = unlines $ fmap ("     "++) [str,(spaces ++ "^^^")]
    where spaces = replicate (n) ' '

-- Builds and returns arithmetic expression or error
buildExp :: [Token] -> Either Error Exp
buildExp tokens = buildHelper tokens Nil

-- Takes in list of tokens, accumulating expression 
-- and Int value representing error's position
buildHelper :: [Token] -> Exp -> Either Error Exp

buildHelper [] exp = case isEmpty exp of
    False -> Right exp
    True -> Left OperatorError
buildHelper (x:xs) exp
    | x == TOpen = processBracket
    | otherwise = processToken
    where
        processBracket = case expressionFromBrackets xs 1 of
            Nothing -> Left BracketsError
            Just guts -> case buildExp guts of
                Left error -> Left ExpInBracketsError
                Right gutExp -> case insertBracketExp gutExp exp of
                    Left error -> Left error
                    Right exp -> buildHelper (drop (succ $ length guts) xs) exp
        
        processToken = case insertToken exp x of
            Left error -> Left error
            Right exp -> buildHelper xs exp

-- Inserts expression from brackets to main one.
insertBracketExp :: Exp -> Exp -> Either Error Exp

insertBracketExp exp Nil = Right (Bracket exp)
insertBracketExp exp (Branch l op Nil) = Right (Branch l op (Bracket exp))
insertBracketExp exp (Branch l op r) = Right (Branch l op (fromRight $ insertBracketExp exp r))
insertBracketExp exp (Bracket expr) = Right (Bracket (fromRight $ insertBracketExp exp expr))
insertBracketExp _ _ = Left ErrorExp

-- Returns tokens in brackets or an error
-- ((1)) -> (1)
expressionFromBrackets :: [Token] -> Int -> Maybe [Token]

expressionFromBrackets [] _ = Nothing
expressionFromBrackets (x:xs) n
    | x == TOpen = fmap (x:) $ expressionFromBrackets xs (n + 1)
    | x == TClose && n == 1 = Just []
    | x == TClose = fmap (x:) $ expressionFromBrackets xs (n - 1)
    | otherwise = fmap (x:) $ expressionFromBrackets xs n

-- Inserts token in expression
insertToken :: Exp -> Token -> Either Error Exp

insertToken _ TError = Left TokenError
insertToken exp token
    | isTokenInt token = insertInt (tokenToInt token) exp
    | isTokenOp token  = insertOp (tokenToOpr token) exp
    | token == TOpen = Left BracketsError
    | token == TClose = Left BracketsError
    | otherwise = Left TokenError

-- Inserts int in expression
insertInt :: Int -> Exp -> Either Error Exp

insertInt n Nil = Right (Val n)
insertInt n (Branch l opr Nil) = Right (Branch l opr (Val n))
-- case when priority changes
insertInt n (Branch l opr r) =  Right (Branch l opr (fromRight $ insertInt n r))
insertInt n _ = Left ErrorExp

-- Inserts operation in expression
insertOp :: Opr -> Exp -> Either Error Exp

insertOp op (Branch l opr Nil) = Left OperatorError
insertOp op (Val n) = Right (Branch (Val n) op Nil)
insertOp op (Bracket expr) = Right (Branch (Bracket expr) op Nil)
insertOp op exp@(Branch l opr r) = case compare (priority op) (priority opr) of
    GT -> Right (Branch l opr (Branch r op Nil)) -- change priority
    _ -> Right (Branch exp op Nil)
insertOp op _ = Left OperatorError

fromRight :: Either a b -> b
fromRight (Right b) = b

-- Checks if expression or any of its branches is empty
isEmpty :: Exp -> Bool

isEmpty Nil = True
isEmpty (Val _)  = False
isEmpty (Bracket exp) = isEmpty exp
isEmpty (Branch l op Nil) = True
isEmpty (Branch l op r)
    | isEmpty l == False && isEmpty r == False = False
    | otherwise = True

-- Return priority of an operation
priority :: Opr -> Int

priority Div = 1
priority Mult = 1
priority Add = 0
priority Sub = 0

-- Converts int token into Int
tokenToInt :: Token -> Int
tokenToInt (TInt n) = n

-- Converts operator token into arithmetic operator
tokenToOpr :: Token -> Opr

tokenToOpr TAdd = Add
tokenToOpr TSub = Sub
tokenToOpr TMult = Mult
tokenToOpr TDiv = Div

-- Converts arithmetic operator to a mathematical function
opToArthFunc :: (Integral a) => Opr -> (a -> a -> a)

opToArthFunc Add = (+)
opToArthFunc Sub = (-)
opToArthFunc Mult = (*)
opToArthFunc Div = div
