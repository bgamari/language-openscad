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
import Data.Functor ((<&>), ($>))
import Data.Functor.Identity (Identity(..))
import Data.List (foldl', find)
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
        -- use quickcheck `sized` to keep sizes manageable
        -- expressions are also resized with objects
        [ do  -- amount of arguments
              l <- QC.choose (0, n)
              let n' = n `div` (2 * max l 1)
              Module
                  <$> QC.arbitrary
                  <*> QC.vectorOf l (QC.resize n' QC.arbitrary)
                  <*> QC.resize n' QC.arbitrary
        ] <>
        if n > 0
        then
            [ let n' = n `div` 2
              in ForLoop <$> QC.arbitrary <*> QC.resize n' QC.arbitrary <*> rec n'
            , do  -- amount of inner objects
                  l <- QC.choose (0, n)
                  let n' = n `div` max l 1
                  Objects <$> QC.vectorOf l (rec n')
            , -- see `isAmbiguousIfElse`, nested if-else objects often must have an
              -- `Object` in between
              let n' = n `div` 3
              in (If
                      <$> QC.resize n' QC.arbitrary
                      <*> rec n'
                      <*> QC.resize n' QC.arbitrary
                ) `QC.suchThat` (not . isAmbiguousIfElse)
            , BackgroundMod <$> rec (n-1)
            , DebugMod <$> rec (n-1)
            , RootMod <$> rec (n-1)
            , DisableMod <$> rec (n-1)
            , do  -- amount of arguments
                  l <- QC.choose (0, n)
                  -- amount of inner objects
                  l' <- QC.choose (0, n)
                  let n' = n `div` (max l 1 * max l' 1)
                  ModuleDef
                      <$> QC.arbitrary
                      <*> QC.vectorOf l ((,) <$> QC.arbitrary <*> QC.resize n' QC.arbitrary)
                      <*> QC.vectorOf l' (rec n')
            , VarDef <$> QC.arbitrary <*> QC.resize (n-1) QC.arbitrary
            , FuncDef
                  <$> QC.arbitrary
                  <*> QC.listOf QC.arbitrary
                  <*> QC.resize (n-1) QC.arbitrary
            ]
        else []
    shrink =
        -- make sure the shrinks also satisfy the predicate for `If`-expressions
        filter (not . isAmbiguousIfElse) . QC.genericShrink

instance PP.Pretty Object where
    pretty v = PP.group $ case v of
        Module i args mBody ->
            PP.pretty i
            <> (if null args
                then PP.lparen <> PP.rparen
                else PP.align (PP.tupled (PP.pretty <$> args)))
            <> case mBody of
                Nothing -> PP.semi
                -- for `Objects`, the `PP.nest` call is already done
                Just os@(Objects _) -> PP.space <> PP.pretty os
                Just o -> PP.nest 2 $ PP.line <> PP.pretty o
        ForLoop i e o ->
            "for"
            <> PP.parens (PP.pretty i <+> "=" <+> PP.pretty e)
            <> case o of
                -- for `Objects`, the `PP.nest` call is already done
                Objects _ -> PP.space <> PP.pretty o
                _ -> PP.nest 2 $ PP.line <> PP.pretty o
        Objects os -> 
            PP.enclose PP.lbrace (PP.line <> PP.rbrace)
            . PP.nest 2
            $ PP.line <> PP.vsep (PP.pretty <$> os)
        If c t me ->
            "if"
            <+> PP.parens (PP.pretty c)
            <> (case t of
                  -- for `Objects`, the `PP.nest` call is already done
                  Objects _ -> PP.space <> PP.pretty t
                  _ -> PP.nest 2 (PP.line <> PP.pretty t)
                  )
            <> maybe
                mempty
                (\e -> PP.line <> "else" <> PP.line <> PP.pretty e)
                me
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
            <> (if null moduleArgs
                then PP.lparen <> PP.rparen
                else PP.align . PP.tupled $ moduleArgs <&> \(i,mV) ->
                  PP.pretty i <> maybe mempty (\v -> PP.space <> PP.equals <+> PP.pretty v) mV
              )
            <> (case moduleBody of
                  [] -> PP.space <> PP.lbrace <> PP.rbrace
                  _ -> PP.space
                    <> PP.enclose PP.lbrace (PP.line <> PP.rbrace)
                        (PP.nest 2 $ PP.line <> PP.vsep (PP.pretty <$> moduleBody))
               )
        VarDef { varName, varValue } -> 
            PP.pretty varName
            <> PP.nest 2
                ( PP.line
                <> PP.equals
                <+> PP.group (PP.pretty varValue <> PP.semi))
        FuncDef { funcName, funcArgs, funcBody } -> 
            "function"
            <+> PP.pretty funcName
            <> (if null funcArgs
                then PP.lparen <> PP.rparen
                else PP.align . PP.tupled $ PP.pretty <$> funcArgs
               )
            <> PP.nest 2
                (PP.line
                <> PP.equals
                <+> PP.pretty funcBody
                <> PP.semi)

-- | Is this `Object` an ambiguous if-else expression?
-- This occurs if an if-else contains a plain if without any braces between it.
-- Then the else is parsed as belonging to the inner if.
isAmbiguousIfElse :: Object -> Bool
isAmbiguousIfElse v = case v of
    If _ t (Just _) -> 
        let f e =
                case e of
                    If _ _ Nothing -> True
                    If _ _ (Just e') -> f e'
                    ForLoop _ _ o -> f o
                    BackgroundMod o -> f o
                    DebugMod o -> f o
                    RootMod o -> f o
                    DisableMod o -> f o
                    Module _ _ (Just o) -> f o
                    _ -> False
        in f t
    _ -> False

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
        -- use quickcheck `sized` to keep sizes manageable
        -- guide expression generation to efficiently get operators with correct
        -- precedence/associativity
        --
        -- p = current precedence
        -- mAssoc = current associativity
        -- n = size
        QC.sized $ fix (\rec p mAssoc n ->
            let
                -- simple non-recursive terms (parsed by `term`)
                simpleTerms =
                    [ EBool <$> QC.arbitrary
                    , EString <$> QC.arbitrary
                    , EVar <$> QC.arbitrary
                    , ENum <$> QC.arbitrary
                    ]
                -- recursive terms (parsed by `term`)
                -- recursion resets precedence/assoc, this by simply calling `arbitrary`
                -- instead of `rec`
                recursiveTerms =
                    [ EParen <$> QC.resize (n-1) QC.arbitrary
                    , do  l <- QC.choose (0,n)
                          let n' = n `div` max 1 l
                          EVec <$> QC.vectorOf l (QC.resize n' QC.arbitrary)
                    , ERange <$> QC.resize (n - 1) QC.arbitrary
                    , do  l <- QC.choose (0,n)
                          let n' = n `div` max 1 l
                          EFunc <$> QC.arbitrary <*> QC.vectorOf l (QC.resize n' QC.arbitrary)
                    , let n' = n `div` 2
                      in EIndex <$> QC.resize n' (QC.oneof $ simpleTerms <> recursiveTerms)
                                <*> QC.resize n' QC.arbitrary
                    ]
                -- operators (parsed by `expression`)
                ops =
                    [ let n' = n `div` 3
                      in ETernary <$> QC.resize n' (QC.oneof simpleTerms)
                                  <*> QC.resize n' QC.arbitrary
                                  <*> QC.resize n' QC.arbitrary
                    | p == 0 -- don't mix with other operators
                    ] ++ catMaybes -- generate operators with higher precedence
                    [ genOp p' op
                    | (p',ops) <- zip (reverse [1 .. length opTable]) opTable
                    , op <- ops
                    ]
                  where
                    isNegatedNum e = case e of
                        ENegate e' ->
                            let f e'' = case e'' of
                                    ENum _ -> True
                                    EIndex e''' _ -> f e'''
                                    _ -> False
                            in f e'
                        _ -> False
                    isNegativeNum e = case e of
                        ENum d -> d < 0
                        EIndex e' _ -> isNegativeNum e'
                        _ -> False
                    genOp p' op = case op of
                        -- NOTE: this generation mechanism relies on the
                        -- assumption that unary operators have a higher
                        -- precedence than binary operators
                        Prefix (OperatorParser c _)
                            | p' > p ->
                                -- inner operators should have higher precedence
                                Just $ (c <$> rec (p'+1) Nothing (n - 1)
                                        -- prefix + sign is not supported
                                        `QC.suchThat` (not . isNegativeNum)
                                      -- negation of num is parsed as sign
                                      ) `QC.suchThat` (not . isNegatedNum)
                        Postfix (OperatorParser c _)
                            | p' > p ->
                                -- inner operators should have higher precedence
                                Just $ c <$> rec (p'+1) Nothing (n - 1)
                        Infix (OperatorParser c _) assoc'
                            | p' > p || p' == p && maybe True (== assoc') mAssoc ->
                                -- inner operators should have higher precedence
                                -- or equal precedence and same associativity
                                let n' = n `div` 2
                                in Just $ c <$> rec (p'+1) (Just assoc') n'
                                            <*> rec (p'+1) (Just assoc') n' 
                        _ -> Nothing
            in if n <= 0
                then QC.oneof simpleTerms
                else QC.oneof $ recursiveTerms <> ops   
        ) 0 Nothing
    shrink = QC.genericShrink

instance PP.Pretty Expr where
    pretty e = case e of
        EVar i -> PP.pretty i
        EIndex e1 idx -> PP.pretty e1 <> PP.brackets (PP.pretty idx)
        ENum d -> PP.pretty d
        EVec es ->
            if null es
            then PP.lbracket <> PP.rbracket
            else PP.align . PP.list $ PP.pretty <$> es
        ERange r -> PP.pretty r
        EString s ->
            let escape c acc =
                    case lookup c escapedChars of
                        Just c' -> '\\' : c' : acc
                        Nothing -> c : acc
            in PP.dquotes $ PP.pretty (foldr' escape "" s)
        EBool b -> case b of
            True -> "true"
            False -> "false"
        EFunc name args ->
            PP.pretty name
            <> (if null args
                then PP.lparen <> PP.rparen
                else PP.align . PP.tupled $ (PP.pretty <$> args)
               )
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
        EParen e1 ->
            PP.group
            . PP.align
            . PP.enclose (PP.flatAlt "( " "(") (PP.flatAlt " )" ")")
            $ PP.pretty e1
      where
        prefix op e1 = op <> PP.pretty e1
        binary op e1 e2 = PP.align $ PP.pretty e1 <> PP.line <> op <+> PP.pretty e2
        ternary op1 op2 e1 e2 e3 =
            let f s = PP.pretty e1 <> s <> op1 <+> PP.pretty e2 <> s <> op2 <+> PP.pretty e3
            in PP.group $ PP.align (f PP.line) `PP.flatAlt` f PP.space

-- | @Range start end step@ denotes a list starting at @start@ and
-- stopping at @end@ with increments of @step@.
data Range a = Range a a (Maybe a)
             deriving (Show, Eq, Generic1)

instance QC.Arbitrary a => QC.Arbitrary (Range a) where
    arbitrary = Range <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

instance PP.Pretty a => PP.Pretty (Range a) where
    pretty (Range start stop step) =
        PP.align
        . PP.group
        . PP.encloseSep (PP.flatAlt "[ " "[")
                        (PP.flatAlt " ]" "]")
                        (PP.flatAlt ": " ":")
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

-- | Association list of escaped chars.
-- Key is the escaped char, value is what gets parsed/printed with a '\' in front.
-- E.g. the quote '"' character gets parsed/printed as `\"`.
escapedChars :: [(Char, Char)]
escapedChars =
    [ ('\\' , '\\')
    , ('"' , '"')
    , ('\t' , 't')
    , ('\n' , 'n')
    , ('\r' , 'r')
    ]

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
    -- build all `EIndex` expressions here
    idxs <- many $ brackets expression <?> "index expression"
    spaces
    return $ foldl' EIndex e idxs
  where
    funcRef = do
      name <- ident
      args <- refArguments
      return $ EFunc name args
    stringLit = between (char '"') (char '"') $
      many $ escapedChar <|> notChar '"'
    escapedChar = char '\\' >> choice ((\(c, escapeChar) -> char escapeChar $> c) <$> escapedChars)

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
           , buildExpressionParser opTable' term
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
    opTable' :: [[Operator Parser Expr]]
    opTable' =
      let mkParser op = case op of
            Infix (OperatorParser fun p) assoc -> Infix (fun <$ p) assoc
            Prefix (OperatorParser fun p) -> Prefix (fun <$ p) 
            Postfix (OperatorParser fun p) -> Postfix (fun <$ p) 
      in fmap mkParser <$> opTable

-- | Internal data structure to keep both the operator and its parser 
data OperatorParser a = OperatorParser
    { opFun :: a
    , opParser :: Parser ()
    }

opTable :: [[Operator OperatorParser Expr]]
opTable =
    [ [ Prefix (OperatorParser ENegate (try $ reservedOp "-" >> notFollowedBy double'))
      , Prefix (OperatorParser id (try $ reservedOp "+" >> notFollowedBy double'))
      , prefix "!" ENot ]
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
    binary  name fun assoc = Infix (OperatorParser fun (reservedOp name)) assoc
    prefix  name fun       = Prefix (OperatorParser fun (reservedOp name))
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
              deriving (Show, Eq, Generic)

instance QC.Arbitrary TopLevel where
    arbitrary = QC.oneof
        [ TopLevelScope <$> QC.arbitrary
        , UseDirective <$> (QC.arbitrary `QC.suchThat` notElem '>')
        , IncludeDirective <$> (QC.arbitrary `QC.suchThat` notElem '>')
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
