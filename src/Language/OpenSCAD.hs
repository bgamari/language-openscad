{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.OpenSCAD
    ( -- * Basic parsing
      parse
      -- * Primitives
    , Ident(..)
    , ident
    , TopLevel(..)
    , topLevel
    , Object(..)
    , object
      -- * Expressions
    , Expr(..)
    , expression
    , Argument(..)
    , Range(..)
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Char (ord, digitToInt)
import Data.Foldable (foldr')
import Data.Function (fix)
import Data.Functor.Identity (Identity(..))
import Data.List (foldl')
import Data.Maybe
import qualified Data.Scientific as Sci
import qualified Data.CharSet as CS
import qualified Data.CharSet.Unicode as CS
import Data.Monoid ((<>))
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc ((<+>))
import GHC.Generics (Generic(..), Generic1(..))
import qualified Test.QuickCheck as QC
import Text.Trifecta hiding (ident)
import Text.Parser.Expression
import qualified Data.ByteString.Char8 as BS
import Text.Parser.Token.Style (emptyOps)

-- | An identifier
newtype Ident = Ident String
              deriving (Show, Eq, Ord)

instance PP.Pretty Ident where
  pretty (Ident i) = PP.pretty i

instance QC.Arbitrary Ident where
  arbitrary = do
    c <- QC.elements $ CS.toList ident1Chars
    l <- QC.getSize
    rest <- QC.vectorOf l . QC.elements $ CS.toList identChars
    pure $ Ident (c:rest)
  shrink (Ident s) = case s of
    c1 : c2 : cs ->
      fmap Ident $ [c1:cs] <> [c2:cs | c2 `CS.member` ident1Chars]
    _ -> mempty

identChars :: CS.CharSet
identChars = CS.letter <> CS.decimalNumber <> CS.fromList "_"

ident1Chars :: CS.CharSet
ident1Chars = CS.fromList "$_" <> CS.letter

-- | Parse an identifier
ident :: Parser Ident
ident = token $ do
    c <- oneOfSet ident1Chars
    rest <- many $ oneOfSet identChars
    return $ Ident (c:rest)

-- | An item in an argument list
data Argument a = Argument a            -- ^ Just a plain value
                | NamedArgument Ident a -- ^ A named argument
                deriving (Show, Eq, Generic)

instance QC.Arbitrary a => QC.Arbitrary (Argument a) where
  arbitrary = QC.oneof
    [ Argument <$> QC.arbitrary
    , NamedArgument <$> QC.arbitrary <*> QC.arbitrary
    ]
  shrink = QC.genericShrink

instance PP.Pretty a => PP.Pretty (Argument a) where
  pretty v = case v of
    Argument a -> PP.pretty a
    NamedArgument i a -> PP.pretty i <+> "=" <+> PP.pretty a

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
    deriving (Show, Generic, Eq)

instance QC.Arbitrary Object where
  arbitrary = QC.sized $ fix $ \rec n -> QC.oneof $
    [ do
        l <- QC.choose (0, n)
        let n' = n `div` (2 * max l 1)
        Module
          <$> QC.arbitrary
          <*> QC.vectorOf l (QC.resize n' QC.arbitrary)
          <*> QC.resize n' QC.arbitrary
    ] <> mconcat [
    [ ForLoop <$> QC.arbitrary <*> QC.resize (n `div` 2) QC.arbitrary <*> rec (n `div` 2)
    , do
        l <- QC.choose (0, n)
        let n' = n `div` max l 1
        Objects <$> QC.vectorOf l (rec n')
    -- FIXME: nested if-else gives issues
    -- , let n' = n `div` 3
    --   in If <$> QC.resize n' QC.arbitrary <*> rec n' <*> QC.resize n' QC.arbitrary
    , BackgroundMod <$> rec (n-1)
    , DebugMod <$> rec (n-1)
    , RootMod <$> rec (n-1)
    , DisableMod <$> rec (n-1)
    , do
        l <- QC.choose (0, n)
        l' <- QC.choose (0, n)
        let n' = n `div` (max l 1 * max l' 1)
        ModuleDef
          <$> QC.arbitrary
          <*> QC.vectorOf l ((,) <$> QC.arbitrary <*> QC.resize n' QC.arbitrary)
          <*> QC.vectorOf l' (rec n')
    , VarDef <$> QC.arbitrary <*> QC.resize (n-1) QC.arbitrary
    , FuncDef <$> QC.arbitrary <*> QC.listOf QC.arbitrary <*> QC.resize (n-1) QC.arbitrary
    ] | n > 0
    ]
  shrink = QC.genericShrink

instance PP.Pretty Object where
  pretty v = case v of
    Module i args mBody ->
      PP.pretty i
      <> (if null args
           then PP.lparen <> PP.rparen
           else PP.align (PP.tupled (PP.pretty <$> args)))
      <> maybe PP.semi
               (PP.nest 2 . mappend PP.softline . PP.pretty)
               mBody 
    ForLoop i e o ->
      "for"
      <> PP.parens (PP.pretty i <+> "=" <+> PP.pretty e)
      <> PP.nest 2 (PP.softline <> PP.pretty o)
    Objects os -> 
      PP.braces . PP.vsep $ PP.pretty <$> os
    If c t me ->
      "if"
      <+> PP.parens (PP.pretty c)
      <+> PP.pretty t
      <+> maybe mempty (("else" <+>) . PP.pretty) me
    BackgroundMod o ->
      "%" <> PP.pretty o
    DebugMod o ->
      "#" <> PP.pretty o
    RootMod o ->
      "!" <> PP.pretty o
    DisableMod o ->
      "*" <> PP.pretty o
    ModuleDef { moduleName, moduleArgs, moduleBody } -> 
      "module"
      <+> PP.pretty moduleName
      <> PP.tupled ((\(i,mV) -> PP.pretty i <> maybe mempty (\v -> PP.equals <> PP.pretty v) mV)<$> moduleArgs)
      <+> PP.braces (PP.vsep $ PP.pretty <$> moduleBody)
    VarDef { varName, varValue } -> 
      PP.pretty varName
      <> PP.equals
      <> PP.pretty varValue
      <> PP.semi
    FuncDef { funcName, funcArgs, funcBody } -> 
      "function"
      <> PP.pretty funcName
      <> PP.tupled (PP.pretty <$> funcArgs)
      <> PP.equals
      <> PP.pretty funcBody
      <> PP.semi

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
    deriving (Show, Eq, Generic)

instance QC.Arbitrary Expr where
  arbitrary = 
    let
      arbitraryExpr' :: Int -> Maybe Assoc -> Int -> QC.Gen Expr
      arbitraryExpr' p mAssoc n
        | n <= 0 = QC.oneof
            [ EBool <$> QC.arbitrary
            , EString <$> QC.arbitrary
            , EVar <$> QC.arbitrary
            , ENum <$> (QC.arbitrary `QC.suchThat` (>= 0))
            ]
        | otherwise = QC.oneof $ 
            [ EParen <$> QC.resize (n-1) QC.arbitrary
            , do
                l <- QC.choose (0,n)
                EVec <$> QC.vectorOf l (QC.resize (n `div` l) QC.arbitrary)
            , ERange <$> QC.resize (n - 1) QC.arbitrary
            , do
                l <- QC.choose (0,n)
                EFunc <$> QC.arbitrary <*> QC.vectorOf l (QC.resize (n `div` l) QC.arbitrary)
            ] ++ catMaybes
            [ genOp p' op
            | (p',ops) <- zip (reverse [1 .. length opTable']) opTable'
            , (_,op) <- ops
            ]
        where
          genOp p' op = case (op) of
            Prefix (Identity c)
              | p' > p -> Just $ c <$> subExpr Nothing (n - 1)
            Postfix (Identity c)
              | p' > p -> Just $ c <$> subExpr Nothing (n - 1)
            Infix (Identity c) assoc'
              | p' > p || p' == p && maybe True (== assoc') mAssoc ->
              Just $ c <$> subExpr (Just assoc') (n `div` 2) <*> subExpr (Just assoc') (n `div` 2) 
            _ -> Nothing
           where subExpr = arbitraryExpr' (p' + 1)
    in QC.sized (arbitraryExpr' 0 Nothing)
  shrink = QC.genericShrink

instance PP.Pretty Expr where
  pretty e = case e of
    EVar i -> PP.pretty i
    EIndex e1 idx -> PP.pretty e1 <> PP.brackets (PP.pretty idx)
    ENum d -> PP.pretty d
    EVec es -> PP.list (PP.pretty <$> es)
    ERange r -> PP.pretty r
    EString s ->
      let escape c acc =
            if c `elem` ("\"\n\t\\\r" :: [Char])
              then '\\' : c : acc
              else c : acc
      in PP.dquotes $ PP.pretty (foldr' escape "" s)
    EBool b -> case b of
      True -> "true"
      False -> "false"
    EFunc name args -> PP.pretty name <> PP.tupled (PP.pretty <$> args)
    ENegate e1 -> prefix "-" e1
    EPlus e1 e2 -> binary "+" e1 e2
    EMinus e1 e2 -> binary "-" e1 e2
    EMult e1 e2 -> binary "*" e1 e2
    EDiv e1 e2 -> binary "/" e1 e2
    EMod e1 e2 -> binary "%" e1 e2
    EEquals e1 e2 -> binary "==" e1 e2
    ENotEquals e1 e2 -> binary "!=" e1 e2
    EGT e1 e2 -> binary ">" e1 e2
    EGE e1 e2 -> binary ">=" e1 e2
    ELT e1 e2 -> binary "<" e1 e2
    ELE e1 e2 -> binary "<=" e1 e2
    ENot e1 -> prefix "!"  e1
    EOr e1 e2 -> binary "||" e1 e2
    EAnd e1 e2 -> binary "&&" e1 e2
    ETernary e1 e2 e3 -> ternary "?" ":" e1 e2 e3
    EParen e1 -> PP.parens $ PP.pretty e1
   where
    prefix op e1 = op <+> PP.pretty e1 -- FIXME: use `<>` instead, fails atm
    binary op e1 e2 = PP.pretty e1 <+> op <+> PP.pretty e2
    ternary op1 op2 e1 e2 e3 = PP.pretty e1 <+> op1 <+> PP.pretty e2 <+> op2 <+> PP.pretty e3

-- | @Range start end step@ denotes a list starting at @start@ and
-- stopping at @end@ with increments of @step@.
data Range a = Range a a (Maybe a)
             deriving (Show, Eq, Generic1)

instance QC.Arbitrary a => QC.Arbitrary (Range a) where
  arbitrary = Range <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance PP.Pretty a => PP.Pretty (Range a) where
  pretty (Range start stop step) =
    PP.encloseSep PP.lbracket PP.rbracket PP.colon
    . fmap PP.pretty
    $ [start, stop] <> maybe [] pure step

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
    list = parens $ commaSep $ try namedArg <|> arg

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
  let mkParser (name, op) = case op of
        Infix (Identity fun) assoc -> Infix (fun <$ reservedOp name) assoc
        Prefix (Identity fun) -> Prefix (fun <$ reservedOp name) 
        Postfix (Identity fun) -> Postfix (fun <$ reservedOp name) 
  in fmap mkParser <$> opTable'
  where
    reservedOp name = reserve emptyOps name

opTable' :: [[(String,Operator Identity Expr)]]
opTable' =
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
    binary  name fun assoc = (name, Infix (Identity fun) assoc)
    prefix  name fun       = (name, Prefix (Identity fun))

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
              deriving (Show, Eq, Generic)

instance QC.Arbitrary TopLevel where
  arbitrary = QC.oneof
    [ TopLevelScope <$> QC.arbitrary
    , UseDirective <$> (QC.arbitrary `QC.suchThat` all (/= '>'))
    , IncludeDirective <$> (QC.arbitrary `QC.suchThat` all (/= '>'))
    ]
  shrink = QC.genericShrink

instance PP.Pretty TopLevel where
  pretty v = case v of
    TopLevelScope o -> PP.pretty o
    IncludeDirective s -> "include" <> PP.angles (PP.pretty s)
    UseDirective s -> "use" <> PP.angles (PP.pretty s)

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
