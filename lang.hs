import System.Directory.Internal.Prelude
import Debug.Trace
import Data.Char
--- DELETE ANYTHING AFTER HERE BESIDES CONTROL
import Control.Applicative
--- DELETE ANYTHING AFTER HERE

{-# LANGUAGE MultiWayIf #-}

newtype FunctionCallT = FunctionCall [Expression]

data FunctionT = Function [String] Expression

data Expression =
    Binary Op Expression Expression | Literal Float | Var String
    deriving Eq

instance Show Expression where
    show (Binary op a b) = "(" ++ show op ++ " " ++ show a ++ " " ++ show b ++ ")"
    show (Literal val) = "(Literal " ++ show val ++ ")"
    show (Var name) = "(Var " ++ show name ++ ")"

data Op = Add | Sub | Mult | Div
    deriving (Eq, Show)

-- type Literal = Float

access :: String -> [(String, Float)] -> Float
access name ((key, val):xs)
    | name == key = val
    | otherwise = access name xs

evaluateFunction :: FunctionT -> FunctionCallT -> Float
evaluateFunction (Function vars expr) (FunctionCall vals) = evaluate (subFunction expr (zip vars (map evaluate vals)))

subFunction :: Expression -> [(String, Float)] -> Expression
subFunction (Var x) state = Literal (access x state)
subFunction (Binary op expr1 expr2) state = Binary op (subFunction expr1 state) (subFunction expr2 state)
subFunction (Literal x) state = Literal x


evaluateBinary :: Op -> Expression -> Expression -> Float
evaluateBinary Add a b = evaluate a + evaluate b
evaluateBinary Sub a b = evaluate a - evaluate b
evaluateBinary Mult a b = evaluate a * evaluate b
evaluateBinary Div a b = evaluate a / evaluate b


evaluateLiteral :: Float -> Float
evaluateLiteral x = x

evaluateExpression :: Expression -> Float
evaluateExpression (Binary op a b) = evaluateBinary op a b
evaluateExpression (Literal val) = evaluateLiteral val

evaluate :: Expression -> Float
evaluate (Binary op a b) = evaluateBinary op a b
evaluate (Literal val) = evaluateLiteral val

-- evaluate :: Expression -> State -> Maybe Float
-- evaluate 

data Token =
    NumTok String | OpTok String | ParenTok String | IdenTok String | Pointer | NewLine
    deriving Show

instance Eq Token where
    (==) (OpTok a) (OpTok b) = a == b
    (==) (NumTok a) (NumTok b) = a == b
    (==) (ParenTok a) (ParenTok b) = a == b
    (==) (IdenTok a) (IdenTok b) = a == b
    (==) _ _ = False

op :: String -> Op
op "+" = Add
op "-" = Sub
op "*" = Mult 
op "/" = Div

subExpr :: [Token] -> [Token] -> Int -> [Token]
subExpr ((ParenTok ")") : xs) b 0 = reverse b
subExpr ((ParenTok ")") : xs) b num = subExpr xs (ParenTok ")" : b) (num - 1)
subExpr ((ParenTok "(") : xs) b num = subExpr xs (ParenTok "(" : b) (num + 1)
subExpr (x : xs) b num = subExpr xs (x : b) num

parseFirst :: [Token] -> [Token]
parseFirst (ParenTok "(" : xs) = subExpr xs [] 0
parseFirst (x : xs) = [x]

remove :: Eq a => [a] -> [a] -> [a]
remove [] a = a
remove (x:xs) (y:ys)  
    | x == y = remove xs ys
    | otherwise = remove xs (y : ys)


addParens :: [Token] -> [Token]
addParens x = ParenTok "(" : reverse (ParenTok ")" : reverse x)

parseSecond :: [Token] -> [Token]
parseSecond x = remove (addParens $ parseFirst x) x

parse :: [Token] -> Expression
parse (ParenTok "(" : xs) = parse $ subExpr xs [] 0
parse ((OpTok x) : xs) = Binary (op x) (parse $ parseFirst xs) (parse $ parseSecond xs)
parse (NumTok x : xs) = Literal (read x :: Float)
parse x = Literal 12

isFunctionDef :: [Token] -> Bool
isFunctionDef [] = False
isFunctionDef (Pointer : xs) = True
isFunctionDef (x : xs) = isFunctionDef xs 

parseInterface :: [Token] -> Expression
parseInterface tokens
    | isFunctionDef tokens = parseFunction tokens
    | otherwise = parse tokens

float :: String -> Float
float x = read x :: Float


specialCharacter :: Char -> Bool
specialCharacter x
    | x == '+' || x == '-' || x == '*' || x == '/'
        || x == '(' || x == ')' = True
    | otherwise = False

createTok :: Char -> Token
createTok x
    | x == '+' || x == '-' || x == '*' || x == '/' = OpTok [x]
    | x == '(' || x == ')' = ParenTok [x]

append :: String -> [Token] -> [Token]
append "" b = b
append a b
    -- | not (null b) && head b /= NewLine = append [] (NewLine : b)
    | isDigit $ last a = NumTok (reverse a) : b
    | otherwise = IdenTok (reverse a) : b

containsPointer :: String -> Bool
containsPointer x = head x : [head (tail x)] == "->"
    

lexer :: String -> String -> [Token] -> [Token]
lexer [] a b = reverse $ append a b
lexer (x:xs) a b
    | x == ' ' =  lexer xs [] (append a b )
    | x == '\n' = lexer (tail xs) [] (append a b)
    | containsPointer (x:xs) = lexer (tail xs) [] (Pointer : append a b)
    | specialCharacter x = lexer xs [] (createTok x : append a b)
    | otherwise = lexer xs (x : a) b



lexerInterface :: String -> [Token]
lexerInterface x = lexer x [] []

-- runStuff :: String -> 

main = do
  s <- readFile "foo.txt"
  doSomethingWith s

doSomethingWith :: String -> IO ()
doSomethingWith x = print (lexerInterface x)

-- main = do
--     x <- getLine
--     print (evaluate (parse (lexerInterface x)))