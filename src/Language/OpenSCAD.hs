{-# LANGUAGE OverloadedStrings #-}

module Language.OpenSCAD
    ( Ident(..)
    , Argument(..)
    , Expr(..)
    , ident
    , parseObject
    , Scad(..)
    , parseScad
    , TopLevel(..) 
    , parseTopLevel
    , parseFile
    , stripComments
    ) where

import Data.Attoparsec.Char8
import Control.Applicative
import Control.Monad (guard)
import Data.List (foldl')
import Data.Char (ord)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as LBS

-- | An identifier
newtype Ident = Ident String
              deriving (Show, Eq, Ord)

identChar :: Char -> Bool
identChar = inClass "a-zA-Z0-9_"

ident :: Parser Ident
ident = do
    c <- satisfy $ inClass "$a-zA-Z_"
    rest <- many $ satisfy identChar
    return $ Ident (c:rest)

data Argument a = Argument a | NamedArgument Ident a
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
    
data Range a = Range a          -- ^ start
                     a          -- ^ end
                     (Maybe a)  -- ^ step
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
double' = do
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

term :: Parser Expr
term = withSpaces $ choice
    [ funcRef
    , ENum <$> signed double'
    , ENegate <$> (char '-' *> term)
    , ENot <$> (char '!' *> term)
    , ERange <$> range
    , EVec <$> betweenSepBy (char ',') (char '[') (char ']') (withSpaces expression)
    , EString <$> between (char '"') (char '"') (many $ notChar '"')
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

keyword :: LBS.ByteString -> Parser ()
keyword word = do
  string word
  next <- peekChar
  guard $ maybe True (not . identChar) next

expression :: Parser Expr
expression = do
    skipSpace
    e1 <- term
    skipSpace
    e1' <- option e1 $ 
      EIndex e1 <$> between (char '[') (char ']') expression
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

comment :: Parser String
comment = (singleLine <|> multiLine) <?> "comment"
  where
    singleLine = skipSpace *> string "//" *> manyTill' anyChar (char '\n')
    multiLine  = skipSpace *> string "/*" *> manyTill' anyChar (string "*/")

between :: Parser open -> Parser close -> Parser a -> Parser a
between start end parser = do
    start
    p <- parser
    end
    return p

block :: Parser a -> Parser [a]
block parser = do
    xs <- between (char '{' >> skipSpace) (char '}') (many parser)
    skipSpace
    optional (char ';')
    return xs

parseObject :: Parser Object
parseObject = skipSpace *> object <* skipSpace
  where
    object =
      choice [ forLoop     <?> "for loop"
             , conditional <?> "if statement"
             , moduleRef   <?> "module reference"
             , Objects <$> block parseObject
             , mod '%' BackgroundMod
             , mod '#' DebugMod
             , mod '!' RootMod
             , mod '*' DisableMod
             ]

    moduleRef = do
      name <- withSpaces ident
      args <- arguments
      skipSpace
      block <- (char ';' >> return Nothing) <|> Just <$> parseObject
      return $ Module name args block

    forLoop = do
      withSpaces $ string "for"
      char '('
      var <- ident
      withSpaces $ char '='
      range <- expression 
      char ')'
      body <- parseObject
      return $ ForLoop var range body

    conditional = do
      withSpaces $ string "if"
      e <- between (char '(') (char ')') expression
      _then <- parseObject
      _else <- optional $ do
        withSpaces $ string "else"
        parseObject
      return $ If e _then _else
      
    mod :: Char -> (Object -> Object) -> Parser Object
    mod c f = do
      withSpaces (char c)
      f <$> parseObject

singleton :: a -> [a]
singleton x = [x]

parseScad :: Parser Scad
parseScad = skipSpace >> scad
  where
    scad = choice [ moduleDef                <?> "module definition"
                  , varDef                   <?> "variable definition"
                  , funcDef                  <?> "function definition"
                  , (Object <$> parseObject) <?> "object"
                  ]
    moduleDef = do
      withSpaces $ string "module"
      name <- ident
      args <- withSpaces arguments
      body <- choice [ singleton <$> parseScad
                     , between (char '{') (char '}') $ many parseScad
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

parseTopLevel :: Parser TopLevel
parseTopLevel =
    choice [ TopLevelScope <$> parseScad
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

parseFile :: LBS.ByteString -> Either String [TopLevel]
parseFile src = 
    go $ parse (many1 parseTopLevel) (stripComments src)
  where
    go (Fail rem ctxs err) = Left $ err ++ ": " ++ show ctxs
    go (Partial feed)      = go $ feed LBS.empty
    go (Done rem r)
      | LBS.null (strip rem) = Right r
      | otherwise            = Left $ "Remaining: " ++ show rem
    strip = LBS.filter (not . isSpace)

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
