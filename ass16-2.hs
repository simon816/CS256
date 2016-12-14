{-
  CS256 Coursework 2, 2016

  Use the function parse_str to get the program syntax tree
-}


{-
  Token functions
  ===============
-}

{-
  Matches the input while the predicate returns true
  Returns the resulting list the moment the predicate fails, or reached end of list
-}
match :: (a -> Bool) -> [a] -> [a]
match p (e: l) | p e = e : match p l
match _ _ = []

{-
  All tokens
  Some tokens take a String value e.g. T_DECIMAL
-}
data Token =
    T_EOF | T_NEWLINE | T_OP_ASSIGN | T_OP_MINUS | T_OP_PLUS | T_OP_MUL
  | T_OP_OPEN_B | T_OP_CLOSE_B | T_KW_VAR | T_KW_EVAL | T_WHITESPACE
  | T_DECIMAL String | T_IDENTIFIER String 
  deriving(Show,Eq)

{-
  Get the length of a specific token instance
  Argument is the current string to tokenize (used in some special cases)
  Returns a function that accepts a token to return it's length
-}
token_length :: String -> Token -> Int
token_length _ (T_DECIMAL s) = length(s)
token_length _ (T_IDENTIFIER s) = length(s)
-- T_NEWLINE Collates all grouped newlines into one token
token_length s T_NEWLINE = length(match is_newline s)
-- Similar to T_NEWLINE in that it matches all whitespace into one token
token_length s T_WHITESPACE = length(match is_whitespace s)
token_length s T_KW_EVAL = 4
token_length s T_KW_VAR = 3
token_length _ T_OP_ASSIGN = 1
token_length _ T_OP_MINUS = 1
token_length _ T_OP_PLUS = 1
token_length _ T_OP_MUL = 1
token_length _ T_OP_OPEN_B = 1
token_length _ T_OP_CLOSE_B = 1
token_length _ T_EOF = 0
token_length _ _ = error "Token length undefined"

token_is_decimal (T_DECIMAL _) = True
token_is_decimal _ = False
token_is_identifier (T_IDENTIFIER _) = True
token_is_identifier _ = False

token_value (T_IDENTIFIER s) = s
token_value (T_DECIMAL s) = s
token_value _ = error "Token has no value"

{-
      Lexer
  =============
  Tokenizes a string
-}

is_digit :: Char -> Bool
is_digit c = c >= '0' && c <= '9'

is_whitespace :: Char -> Bool
is_whitespace c = c == ' ' || c == '\t'

is_newline :: Char -> Bool
is_newline c = c == '\n' || c == '\r'

-- The start of an identifier must be a letter a-zA-Z
is_identifier_start :: Char -> Bool
is_identifier_start c = (c >= 'a' && c <= 'z')
                     || (c >= 'A' && c <= 'Z')

-- Any non-start of an identifier can be a letter or number (alphanumeric)
is_identifier_part :: Char -> Bool
is_identifier_part c = is_identifier_start c || is_digit c

-- Any valid 'operator' type
is_operator :: Char -> Bool
is_operator c = c == '=' || c == '-' || c == '+' || c == '*' 
               || c == '(' || c == ')'

{-
  Tokenize various components
  No checking for empty strings since it's guaranteed to be non-empty
-}

tokenize_decimal :: String -> Token
tokenize_decimal s = T_DECIMAL (match is_digit s)

tokenize_whitespace :: String -> Token
tokenize_whitespace s = T_WHITESPACE

tokenize_identifier :: String -> Token
tokenize_identifier s | id_name == "var" = T_KW_VAR
                      | id_name == "eval" = T_KW_EVAL
                      | otherwise = T_IDENTIFIER (id_name)
  where
    id_name = match is_identifier_part s

tokenize_newline :: String -> Token
tokenize_newline s = T_NEWLINE

tokenize_operator :: String -> Token
tokenize_operator s = op_match (head s)
  where
    op_match :: Char -> Token
    op_match '=' = T_OP_ASSIGN
    op_match '-' = T_OP_MINUS
    op_match '+' = T_OP_PLUS
    op_match '*' = T_OP_MUL
    op_match '(' = T_OP_OPEN_B
    op_match ')' = T_OP_CLOSE_B
    op_match _ = error "is_operator returned true for invalid operator!"

{-
  Fetches the next token from the string
  String must be non-empty (guaranteed from tokenize function)
-}
next_token :: String -> Token
next_token s | is_digit (head s)           = tokenize_decimal(s)
             | is_whitespace(head s)       = tokenize_whitespace(s)
             | is_newline(head s)          = tokenize_newline(s)
             | is_identifier_start(head s) = tokenize_identifier(s)
             | is_operator(head s)         = tokenize_operator(s)
next_token _ = error "Unknown token"


{-
  Advances (skips) through a list 'n' amount of elements
-}
advance :: [a] -> Int -> [a]
advance l 0 = l
advance [] _ = error "Tried to advance passed end of list"
advance (h:t) n = advance t (n - 1)

{-
  Tokenizes a string.
  I.e converts a stream of characters to a stream of tokens
  The end of the stream is always marked with T_EOF
-}
tokenize :: String -> [Token]
tokenize [] = [T_EOF]
tokenize s = do_next(next_token(s), s)
  where
    do_next :: (Token, String) -> [Token]
    do_next (prev, s) = prev : tokenize(advance s (token_length s prev))

{-
        Parser
  =================
-}

-- An expression is a component of a statement
-- Some expressions (like BinaryOpExpr) self-reference Expression 
-- which forms a tree structure
data Expression =
    DecimalLiteral Integer
  | UnaryOpExpr Token Expression -- Token is the operation (e.g. T_OP_MINUS)
  | Identifier String
  | AssignExpr Expression Expression
  | BinaryOpExpr Token Expression Expression -- Token is the operation
  deriving(Show)

-- A statement is a top-level line of code
data Statement =
    VarStmt Expression
  | EvalStmt Expression
  | ExpressionStmt Expression
  deriving(Show)

-- Program is a sequence of statements
type Program = [Statement]

{-
    Consumes an expected head of a list
    If the list doesn't actually have the given element as it's head, an error occurs
-}
consume :: (Eq a, Show a) => (a, [a]) -> [a]
consume (v, next:rest) | v == next = rest
consume (v, []) = error ("Expected " ++ show(v) ++ " but list was empty")
consume (v, next:_) = error ("Expected " ++ show(v) ++ " but found " ++ show(next))

-- Consumes a T_NEWLINE or T_EOF
consume_line :: [Token] -> [Token]
consume_line tokens | head tokens == T_EOF = tokens
consume_line tokens = consume(T_NEWLINE, tokens)

{-
  Shorthand function to parse a string (tokenize it then parse the tokens)
-}
parse_str :: String -> Program
parse_str = parse.tokenize

{-
  Tree building takes place here
  Delegates to parse_stmt until token list is empty
  An empty list is an error, a program must terminate with T_EOF
-}
parse :: [Token] -> Program
parse [] = error "No tokens to parse"
parse [T_EOF] = []
parse (T_NEWLINE: tokens) = parse tokens
parse (T_WHITESPACE: tokens) = parse tokens
parse tokens = stmt : parse(rest)
  where
    -- Filter whitespace since we don't care about it at this point
    (stmt, rest) = parse_stmt((filter (not.(==T_WHITESPACE))) tokens)

{-
  All functions below parse parts of the program
  It is guaranteed that the stream of tokens is non-empty at this point
  hence none of the functions need to check for the empty list
-}

{-
  Shorthand for the type expressions of the below functions

  Description: Takes a list ('stream') of tokens, breaks off
  the Statement/Expression that the function is designed to do,
  then returns it along with the remaining tokens
-}
type StmtFunc = [Token] -> (Statement, [Token])
type ExprFunc = [Token] -> (Expression, [Token])

{-
  Entry point for breaking down the list of tokens
-}
parse_stmt :: StmtFunc
parse_stmt (next: tokens) | next == T_KW_VAR = parse_var_stmt(next:tokens)
                          | next == T_KW_EVAL = parse_eval_stmt(next:tokens)
                          | next == T_NEWLINE = parse_stmt(tokens)
                          | otherwise = parse_expr_stmt(next:tokens)

{-
  "var {var}"
  where {var} is a valid identifier
-}
parse_var_stmt :: StmtFunc
parse_var_stmt tokens = (VarStmt(identifier), consume_line(rest))
  where
    (identifier, rest) = parse_identifier(consume(T_KW_VAR, tokens))

{-
  "eval {exp}"
  exp is not a general Expression here, rather,
  it must be a constant (DecimalLiteral) or variable (Identifier)
  hence the use of parse_basic_expr
-}
parse_eval_stmt :: StmtFunc
parse_eval_stmt tokens = (EvalStmt (expr), consume_line(rest))
  where
    (expr, rest) = parse_basic_expr(consume(T_KW_EVAL, tokens))

{-
  Anything that doesn't start with 'var' or 'eval'
  Uses parse_assignment_expr since that's the only valid expression here
-}
parse_expr_stmt :: StmtFunc
parse_expr_stmt tokens = (ExpressionStmt(expr), consume_line(rest))
  where
    (expr, rest) = parse_assignment_expr(tokens)

{-
    "{var} = {exp}"
    No fall-though allowed, i.e there must be an assignment here and not just a loose Expression
-}
parse_assignment_expr :: ExprFunc
parse_assignment_expr tokens = (AssignExpr left right, rest2)
  where
    (left, rest1) = parse_identifier(tokens)
    (right, rest2) = parse_operation_expr(consume(T_OP_ASSIGN, rest1))

{-
  (optionally) any operation here e.g. "+ {exp} {exp}", "-{exp}"
  falls through, meaning a non-operation is valid e.g. "42" or "x"
  Added bonus: {exp} is evaluated as an operation expression itself (can be chained)
               meaning it's possible for the following: "+ - 2 2 -2"
               i.e ((2-2) + (-2)) in infix
-}
parse_operation_expr :: ExprFunc
parse_operation_expr (next: tokens) | next == T_OP_PLUS || next == T_OP_MUL = (BinaryOpExpr next first second, rest2)
                                    | next == T_OP_MINUS = parse_minus_expr(next:tokens)
                                    | otherwise = parse_basic_expr(next:tokens)
  where
    (first, rest1) = parse_operation_expr(tokens)
    (second, rest2) = parse_operation_expr(rest1)

{-
  Special handling for the T_OP_MINUS operator
  Because it can be both a unary operation and a binary operation
  This peeks ahead to see if the token stream enters a new line or otherwise terminates after the first 'operation expression' (see parse_operation_expr)
  If so, make this a unary operation, otherwise continue reading to create a binary operation
-}
parse_minus_expr :: ExprFunc
parse_minus_expr (next: tokens) | is_terminating(head rest1) = (UnaryOpExpr next first, rest1)
                                | otherwise = (BinaryOpExpr next first second, rest2)
  where
    is_terminating token = token == T_NEWLINE || token == T_EOF || token == T_OP_CLOSE_B
    (first, rest1) = parse_operation_expr(tokens)
    (second, rest2) = parse_operation_expr(rest1)

{-
  "{exp}"
  A basic expression, can be an identifier, literal, or a bracketed 'operation expression' (see parse_operation_expr)
-}
parse_basic_expr :: ExprFunc
parse_basic_expr (next:tokens) | next == T_OP_OPEN_B = (expr, consume(T_OP_CLOSE_B, rest))
                               | token_is_identifier(next) = parse_identifier(next:tokens)
                               | token_is_decimal(next) = parse_decimal_literal(next:tokens)
                               | otherwise = error ("Identifier or literal expected, found " ++ show(next))
  where
    (expr, rest) = parse_operation_expr(tokens)

{-
  Parses an identifier (Shaggy variable)
-}
parse_identifier :: ExprFunc
parse_identifier (next: tokens) | token_is_identifier(next) = (Identifier(token_value(next)), tokens)
                                | otherwise = error ("Expected identifier, found " ++ show(next))

{-
  Parses a decimal literal (Shaggy constant)
-}
parse_decimal_literal :: ExprFunc
parse_decimal_literal (next: tokens) | token_is_decimal(next) = (DecimalLiteral(str_to_int(token_value(next))), tokens)
                                     | otherwise = error ("Expected decimal literal, found " ++ show(next))
    where
        str_to_int = read -- Already in the Prelude

{-
    Evaluator (extra)
  ====================
  This is an example of how the program might be evaluated
  Note that it's very rough and might not work properly
-}
type VarMap = [(String, Integer)]

eval_str = eval.parse_str

eval :: Program -> Maybe Integer
eval l = e ([], l)
  where
    e :: (VarMap, [Statement]) -> Maybe Integer
    e (m, (VarStmt s):rest) = e(put((ident_val s, 0), m), rest)
    e (m, (EvalStmt s):rest) = eval_stmt(s, m)
    e (m, (ExpressionStmt s):rest) = e(exec_expr(s, m), rest)
    e (_, []) = Nothing

    eval_stmt :: (Expression, VarMap) -> Maybe Integer
    eval_stmt (DecimalLiteral v, m) = Just v
    eval_stmt (s, m) = get(ident_val s, m)

    exec_expr :: (Expression, VarMap) -> VarMap
    exec_expr (AssignExpr left right, m) = update((ident_val left, eval_expr(right, m)), m)

    eval_expr :: (Expression, VarMap) -> Integer
    eval_expr (DecimalLiteral v, m) = v
    eval_expr (UnaryOpExpr op expr, m) | op == T_OP_MINUS = -(eval_expr (expr, m))
    eval_expr (BinaryOpExpr op expr1 expr2, m) | op == T_OP_PLUS = (eval_expr (expr1, m)) + (eval_expr (expr2, m))
                                               | op == T_OP_MINUS = (eval_expr (expr1, m)) - (eval_expr (expr2, m))
                                               | op == T_OP_MUL = (eval_expr (expr1, m)) * (eval_expr (expr2, m))
    eval_expr (Identifier name, m) = get_opt(get(name, m))

    ident_val :: Expression -> String
    ident_val (Identifier v) = v
    ident_val _ = error "Not an identifier"

 -- Get the value of an Optional
    get_opt :: Maybe a -> a
    get_opt (Just v) = v
    get_opt _ = error "Value not present"

    -- VarMap functions

    put :: ((String, Integer), VarMap) -> VarMap
    put (entry, []) = [entry]
    put (entry, x:xs) | fst(x) == fst(entry) = entry : xs
                      | otherwise = x : put(entry, xs)

    update :: ((String, Integer), VarMap) -> VarMap
    update (entry, []) = error ("Variable " ++ fst(entry) ++ " doesn't exist")
    update (entry, x:xs) | fst(x) == fst(entry) = entry : xs
                         | otherwise = x : update(entry, xs)

    get :: (String, VarMap) -> Maybe Integer
    get (_, []) = Nothing
    get (k, (ek, ev):xs) | k == ek = Just ev
                         | otherwise = get (k, xs)
