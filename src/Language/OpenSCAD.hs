{-# LANGUAGE OverloadedStrings #-}

module Language.OpenSCAD
    ( Ident(..)
    , Argument(..)
    , Expr(..)
    , ident
    , Scad(..)
    , TopLevel(..) 
    , Range(..)
    , parseFile
    , stripComments
    ) where

import Control.Applicative
import Control.Monad (guard, void)
import Data.List (foldl')
import Data.Char (ord)
import Data.Monoid ((<>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as LBS

-- | An identifier
newtype Ident = Ident String
              deriving (Show, Eq, Ord)

identChar :: Char -> Bool
identChar = inClass "a-zA-Z0-9_"

-- | Parse an identifier
ident :: Parser Ident
ident = do
    c <- satisfy $ inClass "$a-zA-Z0-9_"
    rest <- many $ satisfy identChar
    return $ Ident (c:rest)

-- | An item in an argument list
data Argument a = Argument a            -- ^ Just a plain value
                | NamedArgument Ident a -- ^ A named argument
                deriving (Show)

-- | An OpenSCAD geometry object
data Object
    = Module Ident [Argument Expr] (Maybe Object)
    | ForLoop Ident Expr Object
    | Objects [Object]  -- ^ Implicit union
    | If Expr Object (Maybe Object)
    | BackgroundMod Object
    | DebugMod Object
    | RootMod Object
    | DisableMod Object
    deriving (Show)

-- | An OpenSCAD expression
data Expr
    = EVar Ident
    | EIndex Expr Expr
    | ENum Double
    | EVec [Expr]
    | ERange (Range Expr)
    | EString String
    | EBool Bool
    | EFunc Ident [Argument Expr]
    | ENegate Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
    | EMult Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr
    | EEquals Expr Expr
    | ENotEquals Expr Expr
    | EGT Expr Expr
    | EGE Expr Expr
    | ELT Expr Expr
    | ELE Expr Expr
    | ENot Expr
    | EOr Expr Expr
    | EAnd Expr Expr
    | ETernary Expr Expr Expr
    | EParen Expr
    deriving (Show)
    
-- | @Range start end step@ denotes a list starting at @start@ and
-- stopping at @end@ with increments of @step@.
data Range a = Range a a (Maybe a)
             deriving (Show)

-- | A OpenSCAD scope
data Scad
    = ModuleDef { moduleName :: Ident
                , moduleArgs :: [(Ident, Maybe Expr)]
                , moduleBody :: [Scad]
                }
    | VarDef { varName       :: Ident
             , varValue      :: Expr
             }
    | FuncDef { funcName     :: Ident
              , funcArgs     :: [Ident]
              , funcBody     :: Expr
              }
    | Object Object
    deriving (Show)

sepByTill :: Parser delim -> Parser end -> Parser a -> Parser [a]
sepByTill delim end parser = (end *> return []) <|> go []
  where
    go xs = do x <- parser
               let xs' = x:xs
               (end *> return (reverse xs')) <|> (delim >> go xs')

betweenSepBy :: Parser delim -> Parser start -> Parser end -> Parser a -> Parser [a]
betweenSepBy delim start end parser = start >> sepByTill delim end parser

-- | Parse an argument list
arguments :: Parser [Argument Expr]
arguments = list <?> "argument list"
  where
    list = do
      char '('
      sepByTill (char ',') (char ')') (withSpaces $ try namedArg <|> arg)

    namedArg = do
      name <- skipSpace >> ident
      withSpaces $ char '='
      value <- expression
      return $ NamedArgument name value
    arg = skipSpace >> Argument <$> expression

-- | Parse a range
range :: Parser (Range Expr)
range = do
    withSpaces $ char '['
    start <- expression
    withSpaces $ char ':'
    stop <- expression
    step <- option Nothing $ do
        withSpaces $ char ':'
        Just <$> expression
    withSpaces $ char ']'
    return $ Range start stop step

-- | Accept decimals without leading zero
double' :: Parser Double
double' = notIdent $ do
    choice [ double
           , char '-' >> go negate
           , char '+' >> go id
           , go id
           ]
  where
    go f = do
      char '.'
      digits <- reverse <$> many digitOrd
      exp <- option 0 $ char 'e' >> signed decimal
      let n = foldl' (+) 0 $ zipWith (*) [10^i | i <- [0..]] digits
      return $ f $ realToFrac n / realToFrac (10^(length digits + exp))
    digitOrd = do
      d <- digit
      return $ ord d - ord '0'

-- | Parse a term of an expression
term :: Parser Expr
term = withSpaces $ choice
    [ funcRef
    , ENum <$> signed double'
    , ENegate <$> (char '-' *> term)
    , char '+' *> term
    , ENot <$> (char '!' *> term)
    , ERange <$> range
    , EVec <$> betweenSepBy (char ',') (char '[') (char ']') (withSpaces expression)
    , EString <$> string
    , EBool <$> choice [ keyword "true" >> return True
                       , keyword "false" >> return False
                       ]
    , EVar <$> ident
    , EParen <$> between (char '(') (char ')') expression
    ]
  where
    funcRef = do
      name <- ident
      skipSpace
      args <- arguments
      return $ EFunc name args
    string = do
      char '"'
      s <- many $ escapedChar <|> notChar '"'
      char '"'
      return s
    escapedChar = char '\\' >> anyChar

notIdent :: Parser a -> Parser a
notIdent parser = do
    x <- parser
    next <- peekChar
    guard $ maybe True (not . identChar) next
    return x

keyword :: LBS.ByteString -> Parser ()
keyword word = void $ notIdent (string word)

postfixOp :: Expr -> Parser Expr
postfixOp e =
    choice [ EIndex e <$> between (char '[') (char ']') expression >>= postfixOp
           , return e
           ]

-- | Parse an expression
expression :: Parser Expr
expression = do
    skipSpace
    e1 <- term
    skipSpace
    e1' <- postfixOp e1
    skipSpace
      
    let op c f = do
          string c
          e2 <- expression
          return $ f e1' e2

        ternary = do
          char '?'
          e2 <- expression
          withSpaces $ char ':'
          e3 <- expression
          return $ ETernary e1' e2 e3
    choice [ ternary
           , op "+"  EPlus
           , op "-"  EMinus
           , op "*"  EMult
           , op "/"  EDiv
           , op "%"  EMod
           , op "==" EEquals
           , op "!=" ENotEquals
           , op ">"  EGT
           , op ">=" EGE
           , op "<"  ELT
           , op "<=" ELE
           , op "||" EOr
           , op "&&" EAnd
           , return e1'
           ]

-- | Parse a comment
comment :: Parser String
comment = (singleLine <|> multiLine) <?> "comment"
  where
    singleLine = skipSpace *> string "//" *> manyTill' anyChar (char '\n')
    multiLine  = skipSpace *> string "/*" *> manyTill' anyChar (string "*/")

-- | Parse the given parser bracketed by opening and closing parsers
between :: Parser open -> Parser close -> Parser a -> Parser a
between start end parser = do
    start
    p <- parser
    end
    return p

-- | Parse a block of OpenSCAD statements
block :: Parser a -> Parser [a]
block parser = do
    xs <- between (char '{' >> skipSpace) (char '}') (many parser)
    skipSpace
    optional (char ';')
    return xs

-- | Parse an OpenSCAD object
object :: Parser Object
object = withSpaces $ choice
    [ forLoop     <?> "for loop"
    , conditional <?> "if statement"
    , moduleRef   <?> "module reference"
    , Objects <$> block object
    , mod '%' BackgroundMod
    , mod '#' DebugMod
    , mod '!' RootMod
    , mod '*' DisableMod
    ]
  where
    moduleRef = do
      name <- withSpaces ident
      args <- arguments
      skipSpace
      block <- (char ';' >> return Nothing) <|> Just <$> object
      return $ Module name args block

    forLoop = do
      withSpaces $ string "for"
      char '('
      var <- ident
      withSpaces $ char '='
      range <- expression 
      char ')'
      body <- object
      return $ ForLoop var range body

    conditional = do
      withSpaces $ string "if"
      e <- between (char '(') (char ')') expression
      _then <- object
      _else <- optional $ do
        withSpaces $ string "else"
        object
      return $ If e _then _else
      
    mod :: Char -> (Object -> Object) -> Parser Object
    mod c f = do
      withSpaces (char c)
      f <$> object

singleton :: a -> [a]
singleton x = [x]

-- | Parse an OpenSCAD scope
scad :: Parser Scad
scad = skipSpace >> scad
  where
    scad = choice [ moduleDef                <?> "module definition"
                  , varDef                   <?> "variable definition"
                  , funcDef                  <?> "function definition"
                  , (Object <$> object)      <?> "object"
                  ]
    moduleDef = do
      withSpaces $ string "module"
      name <- ident
      args <- withSpaces arguments
      body <- choice [ singleton <$> scad
                     , between (char '{') (char '}') $ many scad
                     ]
      return $ ModuleDef name args body

    arguments = betweenSepBy (char ',') (char '(') (char ')') $ withSpaces $ do
      name <- withSpaces ident
      value <- optional $ char '=' >> skipSpace >> expression
      return (name, value)

    varDef = do
      name <- skipSpace *> ident
      withSpaces $ char '='
      value <- expression
      skipSpace >> char ';'
      return $ VarDef name value

    funcDef = do
      withSpaces $ string "function"
      name <- ident <* skipSpace
      args <- betweenSepBy (char ',') (char '(') (char ')') (withSpaces ident)
      withSpaces $ char '='
      body <- expression
      skipSpace >> char ';'
      return $ FuncDef name args body

withSpaces :: Parser a -> Parser a
withSpaces parser = skipSpace *> parser <* skipSpace

-- | Things which can appear at the top level of an OpenSCAD source file           
data TopLevel = TopLevelScope Scad
              | UseDirective String
              | IncludeDirective String
              deriving (Show)

-- | Parse the top-level definitions of an OpenSCAD source file
topLevel :: Parser TopLevel
topLevel =
    choice [ TopLevelScope <$> scad
           , UseDirective <$> fileDirective "use"
           , IncludeDirective <$> fileDirective "include"
           ]
  where
    fileDirective keyword = do
      withSpaces $ string keyword
      char '<'
      path <- many1 (notChar '>')
      char '>'
      skipSpace >> optional (char ';')
      return path

-- | Parse an OpenSCAD source file
parseFile :: LBS.ByteString -> Either String [TopLevel]
parseFile src = 
    go $ parse (many1 topLevel) (stripComments src)
  where
    go (Fail rem ctxs err) = Left $ err ++ ": " ++ show ctxs
    go (Partial feed)      = go $ feed LBS.empty
    go (Done rem r)
      | LBS.null (strip rem) = Right r
      | otherwise            = Left $ "Remaining: " ++ show rem
    strip = LBS.filter (not . isSpace)

-- | Strip the comments from and OpenSCAD source file
stripComments :: LBS.ByteString -> LBS.ByteString
stripComments = go LBS.empty
  where
    go accum b | LBS.null b = accum
    go accum b =
      let (before, after) = LBS.span (/= '/') b
          (before', after') = case after of
                c | LBS.null c              -> (before, LBS.empty)
                c | "/*" `LBS.isPrefixOf` c -> let (_, d) = LBS.breakSubstring "*/" c
                                               in (before, LBS.drop 2 d)
                c | "//" `LBS.isPrefixOf` c -> (before, LBS.dropWhile (/= '\n') c)
                c                           -> (before<>"/", LBS.drop 1 after)
      in go (accum <> before') after'
