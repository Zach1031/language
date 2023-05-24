import System.Directory.Internal.Prelude
import Debug.Trace
import Data.Char
import Control.Applicative

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

data Expression =
    Binary Op Expression Expression | Literal Float | Var String | Function String [String] Expression | FunctionCall String [Expression] | Error String
    deriving Eq

instance Show Expression where
    show (Binary op a b) = "(" ++ show op ++ " " ++ show a ++ " " ++ show b ++ ")"
    show (Literal val) = "(Literal " ++ show val ++ ")"
    show (Var name) = "(Var " ++ show name ++ ")"
    show (Function name vars expr) = show name ++ " (" ++ show vars ++ ") -> " ++ show expr
    show (FunctionCall name vars) = show name ++ " (" ++ show vars ++  ")"
    show (Error message) = show "ERROR: " ++ message

data Op = Add | Sub | Mult | Div
    deriving (Eq, Show)

access :: String -> [(String, Float)] -> Float
access name ((key, val):xs)
    | name == key = val
    | otherwise = access name xs

evaluateFunction :: Expression -> [Expression] -> [(String, Expression)] -> Float
evaluateFunction (Function name vars expr) vals state = evaluate (subFunction expr (zip vars (map (\x -> evaluate x state) vals))) state

subFunction :: Expression -> [(String, Float)] -> Expression
subFunction (Var x) state = Literal (access x state)
subFunction (Binary op expr1 expr2) state = Binary op (subFunction expr1 state) (subFunction expr2 state)
subFunction (Literal x) state = Literal x
subFunction (FunctionCall name vars) state = FunctionCall name (map (\x -> subFunction x state) vars)
subFunction x state = Error $ "Cannot parse subfunction " ++ show x


evaluateBinary :: Op -> Expression -> Expression -> [(String, Expression)] -> Float
evaluateBinary Add a b state  = evaluate a state + evaluate b state
evaluateBinary Sub a b state = evaluate a state - evaluate b state
evaluateBinary Mult a b state = evaluate a state * evaluate b state
evaluateBinary Div a b state =evaluate a state / evaluate b state


evaluateLiteral :: Float -> Float
evaluateLiteral x = x

evaluateExpression :: Expression -> [(String, Expression)] -> Float
evaluateExpression (Binary op a b) state = evaluateBinary op a b state
evaluateExpression (Literal val) state = evaluateLiteral val

findFunction :: String -> [(String, Expression)] -> Expression
findFunction request ((name, func) : xs)
    | request == name = func
    | otherwise = findFunction request xs

evaluate :: Expression -> [(String, Expression)] -> Float
evaluate (Binary op a b) state = evaluateBinary op a b state
evaluate (Literal val) state = evaluateLiteral val
evaluate (FunctionCall name exprs) state = evaluateFunction (findFunction name state) exprs state
evaluate x state = -123

data Token =
    NumTok String | OpTok String | ParenTok String | IdenTok String | Pointer | NewLine | Comma | Colon
    deriving Show

instance Eq Token where
    (==) :: Token -> Token -> Bool
    (==) (OpTok a) (OpTok b) = a == b
    (==) (NumTok a) (NumTok b) = a == b
    (==) (ParenTok a) (ParenTok b) = a == b
    (==) (IdenTok a) (IdenTok b) = a == b
    (==) Pointer Pointer = True
    (==) NewLine NewLine = True
    (==) Comma Comma = True
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
parseFirst (IdenTok x : ParenTok "(" : xs) = IdenTok x : (addParens $ subExpr xs [] 0)
parseFirst (x : xs) = [x]
parseFirst [] = []

remove :: Eq a => [a] -> [a] -> [a]
remove [] a = a
remove (x:xs) (y:ys)
    | x == y = remove xs ys
    | otherwise = remove xs (y : ys)
remove x [] = []


addParens :: [Token] -> [Token]
addParens x = ParenTok "(" : reverse (ParenTok ")" : reverse x)

parseSecond :: [Token] -> [Token]
parseSecond x = remove (addParens $ parseFirst x) x

parse :: [Token] -> Expression
parse (ParenTok "(" : xs) = parse $ subExpr xs [] 0
parse ((OpTok x) : xs) = Binary (op x) (parse $ parseFirst xs) (parse $ parseSecond xs)
parse (NumTok x : xs) = Literal (read x :: Float)
parse (IdenTok x : xs)
    | isFunctionCall (IdenTok x : xs) = parseFunctionCall (IdenTok x : xs)
    | otherwise = Var x
parse x = Error $ "Cannot parse " ++ show x

parseVars :: [Token] -> Bool -> [String] -> [String]
parseVars (ParenTok "(" : xs) reading vars = parseVars xs True vars
parseVars (ParenTok ")" : xs) reading vars = reverse vars
parseVars (IdenTok x : xs) True vars = parseVars xs True (x : vars)
parseVars (IdenTok x : xs) False vars = parseVars xs False vars

splitExpr :: [Token] -> [Token]
splitExpr (Pointer : xs) = xs
splitExpr (x : xs) = splitExpr xs

parseName :: [Token] -> String
parseName (IdenTok x : ParenTok "(": xs) = x
parseName (x : xs) = parseName xs

parseFunction :: [Token] -> Expression
parseFunction tokens = Function (parseName tokens) (parseVars tokens False []) (parseInterface (splitExpr tokens))

parseInput :: [Token] -> [Expression]
parseInput (ParenTok "(" : xs) = map parseInterface (splitByToken (init xs) Comma [])

parseFunctionCall :: [Token] -> Expression
parseFunctionCall (IdenTok name : xs) = FunctionCall name (parseInput xs)
parseFunctionCall xs = Literal (-1234)

isFunctionDef :: [Token] -> Bool
isFunctionDef [] = False
isFunctionDef (Pointer : xs) = True
isFunctionDef (x : xs) = isFunctionDef xs

isMultiFunction :: [Token] -> Bool
isMultiFunction [] = False
isMultiFunction (Colon : xs) = True
isMultiFunction (x : xs) = isMultiFunction xs

isFunctionCall :: [Token] -> Bool
isFunctionCall [] = False
isFunctionCall (Comma : xs) = True
isFunctionCall (x : xs) = isFunctionCall xs

parseInterface :: [Token] -> Expression
parseInterface tokens
    | isFunctionDef tokens = parseFunction tokens
    | otherwise = parse tokens

float :: String -> Float
float x = read x :: Float

splitByToken :: [Token] -> Token -> [[Token]] -> [[Token]]
splitByToken [] tok y = reverse $ map reverse y
splitByToken (x : xs) tok [] = splitByToken xs tok [[x]]
splitByToken (x : xs) tok (y : ys)
    | x == Comma = splitByToken xs tok ([] : y : ys)
    | otherwise = splitByToken xs tok ((x : y) : ys)

specialCharacter :: Char -> Bool
specialCharacter x
    | x == '+' || x == '-' || x == '*' || x == '/'
        || x == '(' || x == ')' || x == ',' = True
    | otherwise = False

createTok :: Char -> Token
createTok x
    | x == '+' || x == '-' || x == '*' || x == '/' = OpTok [x]
    | x == '(' || x == ')' = ParenTok [x]
    | x == ',' = Comma

append :: String -> [Token] -> [Token]
append "" b = b
append a b
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

splitByChar :: String -> [String] -> [String]
splitByChar [] y = reverse y
splitByChar (x : xs) [] = splitByChar xs [[x]]
splitByChar (x : xs) (y : ys)
    | x == '\n' && y /= [] = splitByChar xs ([] : y : ys)
    | x == '\n' = splitByChar xs (y : ys)
    | otherwise = splitByChar xs (reverse (x : reverse y) : ys)

lexerInterface :: String -> [Token]
lexerInterface x = lexer x [] []


extractName :: Expression -> String
extractName (Function name vars expr) = name

consumeStatements :: [[Token]] -> [(String, Expression)] -> Float
consumeStatements [x] state = evaluate (parseInterface x) state
consumeStatements (x:xs) a = consumeStatements xs ((extractName $ parseInterface x, parseInterface x) : a)

determineOutput :: Expression -> [(String, Expression)] -> IO()
determineOutput (Function name varse expr) state = putStr ""
determineOutput x state = print (evaluate x state)

appendState :: Expression -> [(String, Expression)] -> [(String, Expression)]
appendState (Function name expr vars) state = (name, Function name expr vars) : state
appendState x state = state

appendAll :: [Expression] -> [(String, Expression)] -> [(String, Expression)]
appendAll xs state = foldl (flip appendState) state xs

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)


runRepl :: [(String, Expression)] -> IO()
runRepl state = do
            putStr "> "
            hFlush stdout
            input <- getLine
            if input /= "quit"
                then do
                    if substring 0 4 input == "load"
                        then do
                            file <- readFile (substring 5 (length input) input ++ ".txt")
                            let statements = map lexerInterface (splitByChar file [])

                            putStrLn $ "Loaded from " ++ substring 5 (length input) input ++ "..."

                            runRepl $ foldl (flip appendState) state (map parseInterface statements)
                    else do
                        let output =  parseInterface $ lexerInterface input
                        determineOutput output state

                        runRepl $ appendState output state
            else
                print "Goodbye!"

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl []
    else
        if length args == 1
            then do
                file <- readFile $ head args
                print $ consumeStatements (map lexerInterface (splitByChar file [])) []
            else putStrLn "Usage: ./lang for the console and ./lang [FILENAME] to run file"