{-# LANGUAGE OverloadedStrings #-}                

module Language.OpenSCAD
    ( Ident(..)
    , Argument(..)
    , Expr(..)
    , Scad(..)
    , ident
    , parseObject
    , parseScad
    , arguments
    ) where

import Data.Attoparsec.Char8
import Control.Applicative
                
-- | An identifier                
newtype Ident = Ident String
              deriving (Show, Eq, Ord)
                
ident :: Parser Ident
ident = do
    c <- satisfy $ inClass "$a-zA-Z_"
    rest <- many $ satisfy $ inClass "a-zA-Z0-9_"
    return $ Ident (c:rest)

data Argument a = Argument a | NamedArgument Ident a
                deriving (Show)
     
data Object
    = Module Ident [Argument Expr] (Maybe Object)
    | Objects [Object]  -- ^ Implicit union
    | BackgroundMod Object
    | DebugMod Object
    | RootMod Object
    | DisableMod Object
    deriving (Show)

data Expr
    = EVar Ident
    | ENum Double
    | EVec [Expr]
    | EString String
    | EBool Bool
    | EFunc Ident [Argument Expr]
    | ENegate Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
    | EMult Expr Expr
    | EDiv Expr Expr
    | EParen Expr
    deriving (Show)
    
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

term :: Parser Expr
term = choice
    [ funcRef
    , ENum <$> signed double
    , ENegate <$> (char '-' *> term)
    , EVec <$> betweenSepBy (char ',') (char '[') (char ']') (withSpaces expression)
    , EString <$> between (char '"') (char '"') (many $ notChar '"')
    , EBool <$> choice [ string "true" >> return True
                       , string "false" >> return False
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
    
expression :: Parser Expr
expression = do
    skipSpace            
    e1 <- term
    skipSpace
    let op c f = do
          char c
          e2 <- expression
          return $ f e1 e2
    choice [ op '+' EPlus
           , op '-' EMinus
           , op '*' EMult
           , op '/' EDiv
           , return e1
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

parseObject :: Parser Object
parseObject = skipSpace *> object <* skipSpace
  where
    object = 
          (moduleRef <?> "module reference")
      <|> (Objects <$> between (char '{') (char '}') (many parseObject))
      <|> mod '%' BackgroundMod
      <|> mod '#' DebugMod
      <|> mod '!' RootMod
      <|> mod '*' DisableMod
    moduleRef = do
      name <- withSpaces ident
      args <- arguments
      skipSpace
      block <- (char ';' >> return Nothing)
        <|> Just <$> parseObject
        <|> (Just <$> between (char '{') (char '}') parseObject)
      return $ Module name args block
      
    mod :: Char -> (Object -> Object) -> Parser Object
    mod c f = do
      withSpaces (char c)
      f <$> parseObject

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
      body <- between (char '{') (char '}') $ many parseScad
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
      args <- betweenSepBy (char ',') (char ')') (char ')') (withSpaces ident)
      withSpaces $ char '='
      body <- expression
      skipSpace >> char ';'
      return $ FuncDef name args body

withSpaces :: Parser a -> Parser a
withSpaces parser = skipSpace *> parser <* skipSpace
