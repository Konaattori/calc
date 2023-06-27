module Parser (calc) where
import Data.Char ( isSpace, isDigit )

calc :: String -> Maybe Double
calc = evaluate . parse . tokenize

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokLParen
           | TokRParen
           | TokNum Double
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> Maybe [Token]
tokenize [] = Just []
tokenize (c : cs)
    | c `elem` "+-*/" = fmap (TokOp (operator c) :) (tokenize cs)
    | c == '('  = fmap (TokLParen :) (tokenize cs)
    | c == ')'  = fmap (TokRParen :) (tokenize cs)
    | isDigit c = number c cs
    | isSpace c = tokenize cs
    | otherwise = Nothing

isFloat :: Char -> Bool
isFloat s
  | isDigit s = True
  | s == '.' = True
  | otherwise = False

number :: Char -> String -> Maybe [Token]
number c cs =
   let (digs, cs') = span isFloat cs in
   fmap (TokNum (read (c : digs)) :) (tokenize cs')

data Expr = SumNode Operator Expr Expr
          | ProdNode Operator Expr Expr
          | NumNode Double
    deriving Show

parseNum :: [Token] -> Maybe (Expr, [Token])
parseNum (TokOp op : TokLParen : tokens) =
  case parseSumOrProdOrNum tokens of
    Just (expr', TokRParen : tokensRest) ->
      case op of
        Minus -> Just (ProdNode Times (NumNode (-1)) expr', tokensRest)
        Plus  -> Just (ProdNode Times (NumNode 1) expr', tokensRest)
    Nothing -> Nothing

parseNum (TokNum n : tokens) = Just (NumNode n, tokens)
parseNum (TokOp Minus : TokOp Minus : tokens) = parseNum tokens
parseNum (TokOp Minus : TokNum n : tokens) = Just (NumNode (-n), tokens)

parseNum (TokLParen : tokens) =
  case parseSumOrProdOrNum tokens of
    Just (expr', TokRParen : tokensRest) -> Just (expr', tokensRest)
    Nothing -> Nothing
parseNum tokens = Nothing

parseProdOrNum :: [Token] -> Maybe (Expr, [Token])
parseProdOrNum tokens =
  case parseNum tokens of
    Just (expr', tokensRest) -> parseProdOrNumHelper expr' tokensRest
    Nothing -> Nothing
  where
    parseProdOrNumHelper :: Expr -> [Token] -> Maybe (Expr, [Token])
    parseProdOrNumHelper expr tokens' =
      case tokens' of
        (TokOp Times : tokensRest) ->
          case parseNum tokensRest of
            Just (expr', tokensRest') -> parseProdOrNumHelper (ProdNode Times expr expr') tokensRest'
            Nothing -> Nothing

        (TokOp Div : tokensRest) ->
          case parseNum tokensRest of
            Just (expr', tokensRest') -> parseProdOrNumHelper (ProdNode Div expr expr') tokensRest'
            Nothing -> Nothing

        _ -> Just (expr, tokens')

parseSumOrProdOrNum :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrNum tokens =
  case parseProdOrNum tokens of
    Just (expr', tokensRest) -> parseSumOrProdOrNumHelper expr' tokensRest
    Nothing -> Nothing
  where
    parseSumOrProdOrNumHelper :: Expr -> [Token] -> Maybe (Expr, [Token])
    parseSumOrProdOrNumHelper expr tokens' =
      case tokens' of
        (TokOp Plus : tokensRest) ->
          case parseProdOrNum tokensRest of
            Just (expr', tokensRest') -> parseSumOrProdOrNumHelper (SumNode Plus expr expr') tokensRest'
            Nothing -> Nothing

        (TokOp Minus : tokensRest) ->
          case parseProdOrNum tokensRest of
            Just (expr', tokensRest') -> parseSumOrProdOrNumHelper (SumNode Minus expr expr') tokensRest'
            Nothing -> Nothing

        _ -> Just (expr, tokens')

parse :: Maybe [Token] -> Maybe Expr
parse tokens =
  case tokens of 
    Just tokens' ->
      case parseSumOrProdOrNum tokens' of
        Just (expr,[]) -> Just expr
        Nothing -> Nothing
    Nothing -> Nothing

evaluate :: Maybe Expr -> Maybe Double
evaluate (Just (NumNode n)) = Just n
evaluate (Just (ProdNode op expr1 expr2)) =
    case op of
      Times -> (*) <$> evaluate (Just expr1) <*> evaluate (Just expr2)
      Div -> (/) <$> evaluate (Just expr1) <*> evaluate (Just expr2)

evaluate (Just (SumNode op expr1 expr2)) =
    case op of
      Plus -> (+) <$> evaluate (Just expr1) <*> evaluate (Just expr2)
      Minus -> (-) <$> evaluate (Just expr1) <*> evaluate (Just expr2)

evaluate Nothing = Nothing

