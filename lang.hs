import System.Directory.Internal.Prelude
import Debug.Trace
import Data.Char
import Control.Applicative

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

data Expression =
    Binary Op Expression Expression | Literal Result | Var String |
    Function String [String] Expression | FunctionCall String [Expression] | MultiLine String [String] [([Expression], Expression)] |
    Ternary Expression Expression Expression
    deriving Eq

instance Show Expression where
    show (Binary op a b) = "(" ++ show op ++ " " ++ show a ++ " " ++ show b ++ ")"
    show (Literal val) = "(Literal " ++ show val ++ ")"
    show (Var name) = "(Var " ++ show name ++ ")"
    show (Function name vars expr) = show name ++ " (" ++ show vars ++ ") -> " ++ show expr
    show (FunctionCall name vars) = show name ++ " (" ++ show vars ++  ")"
    show (MultiLine name vars conditions) = show name ++ " " ++ show vars ++ " " ++ show conditions
    --show (Error message) = show "ERROR: " ++ message
    show (Ternary cond first second) = show "If " ++ show cond ++ show " Then " ++ show first ++ " Else " ++ show second

data Result = Float Float | String String | Bool Bool | Error String deriving Eq

trimFloat :: Float -> String
trimFloat x
    | x == fromIntegral (floor x) = show $ floor x
    | otherwise = show x

instance Show Result where
    show (Float val) = trimFloat val
    show (String str) = show str
    show (Bool bool) = show bool
    show (Error message) = show message


data Op = Add | Sub | Mult | Div | Equal | NotEqual | Greater | GreaterEQ | Less | LessEQ
    deriving (Eq, Show)

access :: String -> [(String, Result)] -> Result
access name ((key, val):xs)
    | name == key = val
    | otherwise = access name xs

matchConditions :: [Expression] -> [Expression] -> Bool
matchConditions [] [] = True
matchConditions (Var x : xs) (y : ys) = matchConditions xs ys
matchConditions (x : xs) (y : ys)
    | x == y = matchConditions xs ys
    | otherwise = False

findCond :: [([Expression], Expression)] -> [Expression] -> Expression
--findCond [] x = Float (-123456789)
findCond ((currConds, val) : xs) conds
    | matchConditions currConds conds = val
    | otherwise = findCond xs conds


evaluateFunction :: Expression -> [Expression] -> [(String, Expression)] -> Result
evaluateFunction (Function name vars expr) vals state = evaluate (subFunction expr (zip vars (map (\x -> evaluate x state) vals))) state
evaluateFunction (MultiLine name vars conds) vals state = evaluate (subFunction (findCond conds vals) (zip vars (map (\x -> evaluate x state) vals))) state

subFunction :: Expression -> [(String, Result)] -> Expression
subFunction (Var x) state = Literal (access x state)
subFunction (Binary op expr1 expr2) state = Binary op (subFunction expr1 state) (subFunction expr2 state)
subFunction (Literal x) state = Literal x
subFunction (FunctionCall name vars) state = FunctionCall name (map (\x -> subFunction x state) vars)
subFunction (Ternary cond first second) state = Ternary (subFunction cond state) (subFunction first state) (subFunction second state)
subFunction x state = (trace $ show x) Literal $ Float 12345
--subFunction x state = Error $ "Cannot parse subfunction " ++ show x

evaluateBinaryNum :: Op -> Float -> Float -> [(String, Expression)] -> Result
evaluateBinaryNum Add a b state  = Float $ a + b
evaluateBinaryNum Sub a b state = Float $ a - b
evaluateBinaryNum Mult a b state = Float $ a * b
evaluateBinaryNum Div a b state = Float $ a / b
evaluateBinaryNum Equal a b state = if a == b then Bool True else Bool False
evaluateBinaryNum NotEqual a b state = if a == b then Bool True else Bool False
evaluateBinaryNum Greater a b state = if a == b then Bool True else Bool False
evaluateBinaryNum GreaterEQ a b state = if a == b then Bool True else Bool False
evaluateBinaryNum Less a b state = if a == b then Bool True else Bool False
evaluateBinaryNum LessEQ a b state = if a == b then Bool True else Bool False

evaluateBinary :: Op -> Result -> Result -> [(String, Expression)] -> Result
evaluateBinary op (Float x) (Float y) state = evaluateBinaryNum op x y state
-- evaluateBinary Add a b state  = (trace $ show a) Float $ a + b
-- evaluateBinary Sub a b state = Float $ a - b
-- evaluateBinary Mult a b state = Float $ a * b
-- evaluateBinary Div a b state = Float $ a / b 
evaluateBinary Equal a b state = if a == b then Bool True else Bool False
evaluateBinary NotEqual a b state = if a == b then Bool True else Bool False
evaluateBinary Greater a b state = if a == b then Bool True else Bool False
evaluateBinary GreaterEQ a b state = if a == b then Bool True else Bool False
evaluateBinary Less a b state = if a == b then Bool True else Bool False
evaluateBinary LessEQ a b state = if a == b then Bool True else Bool False

evaluateTernary :: Result -> Expression -> Expression -> [(String, Expression)] -> Result
evaluateTernary result first second state
    | result == Bool False = evaluate second state
    | otherwise = evaluate first state

findFunction :: String -> [(String, Expression)] -> Expression
findFunction request ((name, func) : xs)
    | request == name = func
    | otherwise = findFunction request xs

evalParam :: [Expression] -> [(String, Expression)] -> [Expression]
evalParam xs state = map (\ x -> Literal (evaluate x state)) xs

evaluate :: Expression -> [(String, Expression)] -> Result
evaluate (Binary op a b) state = evaluateBinary op (evaluate a state) (evaluate b state) state
evaluate (FunctionCall name exprs) state = evaluateFunction (findFunction name state) (map (\ x -> Literal (evaluate x state)) exprs) state
--evaluate (FunctionCall name exprs) state = evaluateFunction (findFunction name state) exprs state
evaluate (Ternary cond first second) state = evaluateTernary (evaluate cond state) first second state
evaluate (Literal val) state = val
--evaluate x state = (trace $ show "Debug | x: " ++ show x ++ " State: " ++ show state ) (Float 12345)
evaluate x state = Float 1234
--evaluate x state = (trace $ show x) Float (-123)
-- evaluate x state = 
--     case x of 
--         Result -> x

data Token =
    NumTok String | OpTok String | ParenTok String | IdenTok String |
    Pointer | NewLine | Comma | Colon | Bar |
    If | Then | Else
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
    (==) Bar Bar = True
    (==) If If = True
    (==) Then Then = True
    (==) Else Else = True
    (==) _ _ = False

op :: String -> Op
op "+" = Add
op "-" = Sub
op "*" = Mult
op "/" = Div
op "==" = Equal
op "!=" = NotEqual
op ">" = Greater
op ">=" = GreaterEQ
op "<" = Less
op "<=" = LessEQ

subExpr :: [Token] -> [Token] -> Int -> [Token]
subExpr ((ParenTok ")") : xs) b 0 = reverse b
subExpr ((ParenTok ")") : xs) b num = subExpr xs (ParenTok ")" : b) (num - 1)
subExpr ((ParenTok "(") : xs) b num = subExpr xs (ParenTok "(" : b) (num + 1)
subExpr (x : xs) b num = subExpr xs (x : b) num
subExpr x y z = [Bar]



parseFirst :: [Token] -> [Token]
parseFirst (ParenTok "(" : xs) = subExpr xs [] 0
parseFirst (IdenTok x : ParenTok "(" : xs) = IdenTok x : addParens (subExpr xs [] 0)
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

parseCond :: [Token] -> [Token]
parseCond (Then : xs) = []
parseCond (x : xs) = x : parseCond xs

parseThen :: [Token] -> Bool -> [Token]
parseThen (Else : xs) cond = []
parseThen (x : xs) cond
    | x == Then = parseThen xs True
    | cond = x : parseThen xs cond
    | otherwise = parseThen xs cond

parseElse :: [Token] -> Bool -> [Token]
parseElse [] cond = []
parseElse (x : xs) cond
    | x == Else = parseElse xs True
    | cond = x : parseElse xs cond
    | otherwise = parseElse xs cond


-- parseElse :: [Token] -> [Token]

parse :: [Token] -> Expression
parse (ParenTok "(" : xs) = parse $ subExpr xs [] 0
parse ((OpTok x) : xs) = Binary (op x) (parse $ parseFirst xs) (parse $ parseSecond xs)
parse (If : xs) = Ternary (parse $ parseCond xs) (parse $ parseThen xs False) (parse $ parseElse xs False)
parse (NumTok x : xs) = Literal $ Float (read x :: Float)
parse (IdenTok x : ParenTok "(" : xs) = parseFunctionCall (IdenTok x : ParenTok "(" : xs)
parse (IdenTok x : xs) = Var x
--parse x = Error $ "Cannot parse " ++ show x

parseVars :: [Token] -> Bool -> [String] -> [String]
parseVars (ParenTok "(" : xs) reading vars = parseVars xs True vars
parseVars (ParenTok ")" : xs) reading vars = reverse vars
parseVars (IdenTok x : xs) True vars = parseVars xs True (x : vars)
parseVars (IdenTok x : xs) False vars = parseVars xs False vars

splitExpr :: [Token] -> [Token]
splitExpr (Pointer : xs) = xs
splitExpr (x : xs) = splitExpr xs

parseName :: [Token] -> String
parseName (IdenTok x : ParenTok "(": xs) =  x
parseName (x : xs) = parseName xs

isMultiLine :: [Token] -> Bool
isMultiLine [] = False
isMultiLine (Bar : xs) = True
isMultiLine (x : xs) = isMultiLine xs

parsePattern :: [Token] -> [Expression]
parsePattern = map (\ x -> parseInterface [x])

parseConditions  :: [[Token]] -> [([Expression], Expression)] -> [([Expression], Expression)]
parseConditions xs exprs
  = foldl
      (\ exprs x
         -> (parsePattern (head $ splitByToken x Pointer []),
             parseInterface (last $ splitByToken x Pointer []))
              : exprs)
      exprs xs

parseFunction :: [Token] -> Expression
parseFunction (IdenTok "run" : xs) = Function "run" [] (parseInterface (splitExpr xs))
parseFunction tokens
    | isMultiLine tokens = MultiLine (parseName tokens) (parseVars tokens False []) (reverse $ parseConditions (tail $ splitByToken tokens Bar []) [])
    | otherwise = Function (parseName tokens) (parseVars tokens False []) (parseInterface (splitExpr tokens))

parseInput :: [Token] -> [Expression]
parseInput (ParenTok "(" : xs) = map parseInterface (splitByToken (init xs) Comma [])

parseFunctionCall :: [Token] -> Expression
parseFunctionCall (IdenTok name : xs) = FunctionCall name (parseInput xs)
--parseFunctionCall xs = Literal $ Float (-1234)

isFunctionDef :: [Token] -> Bool
isFunctionDef [] = False
isFunctionDef (Pointer : xs) = True
isFunctionDef (x : xs) = isFunctionDef xs

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
    | x == tok = splitByToken xs tok ([] : y : ys)
    | otherwise = splitByToken xs tok ((x : y) : ys)

specialCharacter :: Char -> Bool
specialCharacter x
    | x == '+' || x == '-' || x == '*' || x == '/'
        || x == '(' || x == ')' || x == ',' || x == '|'
        || x == '<' || x == '>' = True
    | otherwise = False

createTok :: Char -> Token
createTok x
    | x == '+' || x == '-' || x == '*' || x == '/' || x == '<' || x == '>' = OpTok [x]
    | x == '(' || x == ')' = ParenTok [x]
    | x == ',' = Comma
    | x == '|' = Bar

checkReserved :: String -> [Token] -> [Token]
checkReserved a b
    | a == "if" = If : b
    | a == "then" = Then : b
    | a == "else" = Else : b
    | otherwise = IdenTok a : b

append :: String -> [Token] -> [Token]
append "" b = b
append a b
    | isDigit $ head a = NumTok (reverse a) : b
    | otherwise = checkReserved (reverse a) b

containsPointer :: String -> Bool
containsPointer x = head x : [head (tail x)] == "->"

containsEqual :: String -> Bool
containsEqual x = head x : [head (tail x)] == "=="

containsNotEqual :: String -> Bool
containsNotEqual x = head x : [head (tail x)] == "!="

isNegative :: String -> Bool
isNegative (x : y : xs)
    | x == '-' && isDigit y = True
    | otherwise = False
isNegative x = False

containsDouble :: String -> Bool
containsDouble x
    | tok == "->" || tok == "==" || tok == "!="
        || tok == ">=" || tok == "<=" = True
    | otherwise = False
    where tok = head x : [head (tail x)]

createDouble :: String -> Token
createDouble x
    | tok == "->" = Pointer
    | otherwise = OpTok tok
    where tok = head x : [head (tail x)]

lexer :: String -> String -> [Token] -> [Token]
lexer [] a b = reverse $ append a b
lexer (x:xs) a b
    | x == ' ' =  lexer xs [] (append a b )
    | x == '\n' = lexer (tail xs) [] (append a b)
    | isNegative (x : xs) = lexer xs (x : a) (append a b)
    -- | containsPointer (x:xs) = lexer (tail xs) [] (Pointer : append a b)
    -- | containsEqual (x:xs) = lexer (tail xs) [] (OpTok "==" : append a b)
    -- | containsNotEqual (x:xs) = lexer (tail xs) [] (OpTok "!=" : append a b)
    | containsDouble (x : xs) = lexer (tail xs) [] (createDouble (x : xs) : append a b)
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
extractName (MultiLine name vars conditions) = name

joinFunc :: [[Token]] -> [[Token]]
joinFunc [x] = [x]
joinFunc (x : (Bar : ys) : xs) = joinFunc $ (x ++ (Bar : ys)) : xs
joinFunc (x : xs) = x : joinFunc xs

removeComments :: [String] -> [String]
removeComments [] = []
removeComments (x : xs)
    | head x == '#' = removeComments xs
    | otherwise = x : removeComments xs

run :: [(String, Expression)] -> Result
run (("run", Function name vars expr) : xs) = evaluate expr xs
run (x : xs) = run (xs ++ [x])

consumeStatements :: [[Token]] -> [(String, Expression)] -> Result
consumeStatements [] state = run state
consumeStatements (x:xs) a = consumeStatements xs ((extractName $ parseInterface x, parseInterface x) : a)

determineOutput :: Expression -> [(String, Expression)] -> IO()
determineOutput (Function name varse expr) state = putStr ""
determineOutput x state = print (evaluate x state)

appendState :: Expression -> [(String, Expression)] -> [(String, Expression)]
appendState (Function name expr vars) state
    | name /= "run" = (name, Function name expr vars) : state
    | otherwise = state
appendState x state = state

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

fileName :: String -> String
fileName name
    | last (init name) : [last name] == ".z" = name
    | otherwise = name ++ ".z"

runRepl :: [(String, Expression)] -> IO()
runRepl state = do
            putStr "> "
            hFlush stdout
            input <- getLine
            if input /= "quit"
                then do
                    if substring 0 4 input == "load"
                        then do
                            let name = fileName (substring 5 (length input) input)
                            file <- readFile name
                            let statements = map lexerInterface (splitByChar file [])

                            putStrLn $ "Loaded from " ++ name ++ "..."

                            print state

                            runRepl $ foldl (flip appendState) state (map parseInterface statements)
                    else do
                        let output =  parseInterface $ lexerInterface input
                        determineOutput output state

                        runRepl $ appendState output state
            else
                print "Goodbye!"

-- main :: IO ()
-- main = do
--   args <- getArgs
--   if null args
--     then runRepl []
--     else
--         if length args == 1
--             then do
--                 file <- readFile $ head args
--                 --print $ consumeStatements (map lexerInterface (splitByChar file [])) []
--                 print $ consumeStatements (joinFunc (map lexerInterface (splitByChar file []))) []
--             else putStrLn "Usage: ./lang for the console and ./lang [FILENAME] to run file"

--MAIN FOR TESTING
main :: IO()
main = do
        file <- readFile "foo.z"

        print $ consumeStatements (joinFunc (map lexerInterface (removeComments $ splitByChar file []))) []

        --print $  (map parseInterface (joinFunc (map lexerInterface (splitByChar file []))))