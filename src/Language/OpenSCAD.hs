{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.OpenSCAD
    ( -- * Basic parsing
      parse
      -- * Primitives
    , Ident(..)
    , ident
    , TopLevel(..)
    , Object(..)
      -- * Expressions
    , Expr(..)
    , Argument(..)
    , Range(..)
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (ord, digitToInt)
import Data.List (foldl')
import Data.Maybe
import qualified Data.Scientific as Sci
import qualified Data.CharSet as CS
import qualified Data.CharSet.Unicode as CS
import Data.Monoid ((<>))
import Text.Trifecta hiding (ident)
import Text.Parser.Expression
import qualified Data.ByteString.Char8 as BS
import Text.Parser.Token.Style (emptyOps)

-- | An identifier
newtype Ident = Ident String
              deriving (Show, Eq, Ord)

identChars :: CS.CharSet
identChars = CS.letter <> CS.decimalNumber <> CS.fromList "_"

-- | Parse an identifier
ident :: Parser Ident
ident = token $ do
    c <- oneOfSet $ CS.fromList "$_" <> CS.letter
    rest <- many $ oneOfSet identChars
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
    | ModuleDef { moduleName :: Ident
                , moduleArgs :: [(Ident, Maybe Expr)]
                , moduleBody :: [Object]
                }
    | VarDef { varName       :: Ident
             , varValue      :: Expr
             }
    | FuncDef { funcName     :: Ident
              , funcArgs     :: [Ident]
              , funcBody     :: Expr
              }
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


sepByTill :: Parser delim -> Parser end -> Parser a -> Parser [a]
sepByTill delim end parser = (end *> return []) <|> go []
  where
    go xs = do x <- parser
               let xs' = x:xs
               (end *> return (reverse xs')) <|> (delim >> go xs')

betweenSepBy :: Parser delim -> Parser start -> Parser end -> Parser a -> Parser [a]
betweenSepBy delim start end parser = start >> sepByTill delim end parser

equals :: Parser Char
equals = symbolic '='

-- | Parse a module reference argument list
refArguments :: Parser [Argument Expr]
refArguments = list <?> "argument list"
  where
    list = parens $ commaSep $ namedArg <|> arg

    namedArg = do
      name <- try $ ident <* equals
      value <- expression
      return $ NamedArgument name value
    arg = spaces >> Argument <$> expression

-- | Parse a range
range :: Parser (Range Expr)
range = brackets $ do
    start <- expression
    colon
    stop <- expression
    step <- option Nothing $ do
        colon
        Just <$> expression
    return $ Range start stop step

-- | Accept decimals, including fractional values lacking a zero to the left of
-- the decimal point.
double' :: Parser Double
double' = notIdent $
    choice [ try $ do
                 s <- sign
                 n <- decimal
                 f <- try fractExponent <|> (fromInteger <$ char '.') <|> pure fromInteger
                 return $ realToFrac $ s $ f n
           , do s <- sign
                realToFrac . s . ($ 0) <$> fractExponent
           ]

fractExponent :: forall m. TokenParsing m => m (Integer -> Sci.Scientific)
fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1 exponent'
            <|> (\expo n -> fromInteger n * expo) <$> exponent'
 where
  fraction :: m Sci.Scientific
  fraction = foldl' op 0 <$> (char '.' *> (some digit <?> "fraction"))

  op f d = f + Sci.scientific (fromIntegral (digitToInt d)) (Sci.base10Exponent f - 1)

  exponent' :: m Sci.Scientific
  exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"

  power = Sci.scientific 1 . fromInteger

sign :: (Num a, TokenParsing m) => m (a -> a)
sign =
      negate <$ char '-'
  <|> id <$ char '+'
  <|> pure id

-- | Parse a term of an expression
term :: Parser Expr
term = do
  e <- choice
    [ try funcRef
    , ERange <$> try range
    , EVec <$> brackets (sepEndBy expression (some comma))
    , EString <$> stringLit
    , EBool <$> choice [ keyword "true" >> return True
                       , keyword "false" >> return False
                       ]
    , EVar <$> ident
    , ENum <$> double'
    , EParen <$> parens expression
    ]
  idx <- optional $ brackets expression <?> "index expression"
  spaces
  return $ maybe e (e `EIndex`) idx
  where
    funcRef = do
      name <- ident
      args <- refArguments
      return $ EFunc name args
    stringLit = between (char '"') (char '"') $
      many $ escapedChar <|> notChar '"'
    escapedChar = char '\\' >> anyChar

notIdent :: Parser a -> Parser a
notIdent parser = do
    x <- parser
    notFollowedBy $ oneOfSet identChars
    return x

keyword :: String -> Parser ()
keyword word = void $ notIdent (symbol word)

-- | Parse an expression
expression :: Parser Expr
expression =
    choice [ try ternary
           , buildExpressionParser opTable term
           ]
    <?> "expression"
  where
    ternary = do
        e1 <- term
        symbolic '?'
        e2 <- expression
        colon
        e3 <- expression
        return $ ETernary e1 e2 e3

opTable :: [[Operator Parser Expr]]
opTable =
    [ [ prefix "-" ENegate, prefix "+" id, prefix "!" ENot ]
    , [ binary "*" EMult AssocLeft, binary "/" EDiv AssocLeft, binary "%" EMod AssocLeft ]
    , [ binary "+" EPlus AssocLeft, binary "-" EMinus AssocLeft ]
    , [ binary "==" EEquals AssocLeft
      , binary "!=" ENotEquals AssocLeft
      , binary ">"  EGT AssocLeft
      , binary ">=" EGE AssocLeft
      , binary "<"  ELT AssocLeft
      , binary "<=" ELE AssocLeft
      ]
    , [ binary "||" EOr AssocLeft, binary "&&" EAnd AssocLeft ]
    ]
  where
    binary  name fun assoc = Infix (fun <$ reservedOp name) assoc
    prefix  name fun       = Prefix (fun <$ reservedOp name)
    reservedOp name = reserve emptyOps name

-- | Parse a comment
comment :: Parser String
comment = do
    spaces
    (singleLine <|> multiLine) <?> "comment"
  where
    singleLine = string "//" *> manyTill anyChar (char '\n')
    multiLine  = string "/*" *> manyTill anyChar (string "*/")

-- | Parse a block of OpenSCAD statements
block :: Parser a -> Parser [a]
block parser = do
    xs <- between (char '{' >> spaces) (char '}') (many parser)
    spaces
    optional someSemis
    return xs

-- | Parse an OpenSCAD object
object :: Parser Object
object = spaces >> choice
    [ forLoop     <?> "for loop"
    , conditional <?> "if statement"
    , moduleDef   <?> "module definition"
    , funcDef     <?> "function definition"
    , try varDef  <?> "variable definition"
    , moduleRef   <?> "module reference"
    , Objects <$> block object
    , mod '%' BackgroundMod
    , mod '#' DebugMod
    , mod '!' RootMod
    , mod '*' DisableMod
    ]
  where
    moduleRef = do
      name <- ident
      args <- refArguments
      spaces
      block <- (someSemis >> return Nothing) <|> fmap Just object
      return $ Module name args block

    forLoop = do
      symbol "for"
      (var, range) <- parens $
          ((,) <$> ident <* equals <*> expression)
      body <- object
      return $ ForLoop var range body

    conditional = do
      symbol "if"
      e <- parens expression
      _then <- object
      _else <- optional $ do
        symbol "else"
        object
      return $ If e _then _else

    mod :: Char -> (Object -> Object) -> Parser Object
    mod c f = do
      symbolic c
      f <$> object

    moduleDef = do
      symbol "module"
      name <- ident
      args <- arguments
      body <- choice [ braces $ many object
                     , singleton <$> object
                     ]
      return $ ModuleDef name args body
      where
        arguments = parens $ commaSep $ do
          name <- ident
          value <- optional $ equals >> expression
          return (name, value)

    varDef = do
      name <- ident
      equals
      value <- expression
      someSemis
      return $ VarDef name value

    funcDef = do
      symbol "function"
      name <- ident <* spaces
      args <- parens $ commaSep ident
      equals
      body <- expression
      someSemis
      return $ FuncDef name args body

-- | Things which can appear at the top level of an OpenSCAD source file
data TopLevel = TopLevelScope Object
              | UseDirective String
              | IncludeDirective String
              deriving (Show)

-- | Parse the top-level definitions of an OpenSCAD source file
topLevel :: Parser TopLevel
topLevel = do
    spaces
    optional someSemis
    tl <- choice [ UseDirective <$> fileDirective "use"
                 , IncludeDirective <$> fileDirective "include"
                 , TopLevelScope <$> object
                 ]
    optional someSemis
    return tl
  where
    fileDirective keyword = try $ do
      symbol keyword
      path <- runUnspaced $ angles $ many (notChar '>')
      optional someSemis
      return path

-- not currently safe due to need to strip comments
parseFile :: FilePath -> IO (Either String [TopLevel])
parseFile = fmap resultToEither . parseFromFileEx (some topLevel)

-- | Parse OpenSCAD source
parse :: BS.ByteString -> Either String [TopLevel]
parse =
    resultToEither . parseByteString (some topLevel) mempty . stripComments

resultToEither :: Result a -> Either String a
resultToEither (Failure err) = Left $ show err
resultToEither (Success r)   = Right r

-- | Strip the comments from an OpenSCAD source file.
stripComments :: BS.ByteString -> BS.ByteString
stripComments = go BS.empty
  where
    -- TODO: This is terribly inefficient
    go accum b | BS.null b = accum
    go accum b =
      let (before, after) = BS.span (/= '/') b
          (before', after') = case after of
                c | BS.null c              -> (before, BS.empty)
                  | "/*" `BS.isPrefixOf` c -> let (_, d) = BS.breakSubstring "*/" c
                                              in (before, BS.drop 2 d)
                  | "//" `BS.isPrefixOf` c -> (before, BS.dropWhile (/= '\n') c)
                  | otherwise              -> (before<>"/", BS.drop 1 after)
      in go (accum <> before') after'

singleton :: a -> [a]
singleton x = [x]

someSemis :: Parser ()
someSemis = sepEndBy1 semi spaces >> return ()
