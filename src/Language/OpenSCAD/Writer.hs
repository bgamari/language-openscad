{-# LANGUAGE OverloadedStrings #-}

module Language.OpenSCAD.Writer where

import           Data.Maybe                (fromMaybe, maybeToList)
import           Data.Text.Lazy            (Text, pack)
import           Data.Text.Prettyprint.Doc (Doc, align, concatWith, fillSep,
                                            group, hang, hardline, indent, line,
                                            nest, sep, softline, vsep)
import qualified Data.Text.Prettyprint.Doc as P
import           Language.OpenSCAD

pretty :: [TopLevel] -> Doc Text
pretty = concatWith (<>) . map (\x -> prettyTopLevel x <> hardline)

prettyTopLevel :: TopLevel -> Doc Text
prettyTopLevel x =
  case x of
    TopLevelScope obj    -> group $ prettyObject obj
    UseDirective str     -> "use" </> "<" <> t str <> ">"
    IncludeDirective str -> "include" </> "<" <> t str <> ">"

prettyObject :: Object -> Doc Text
prettyObject obj =
  case obj of
    Module (Ident ident) args maybeObj ->
      t ident <>
      "(" </> prettyArguments args </> ")" <>
      fromMaybe ";" (fmap (nest 2 . mappend line . prettyObject) maybeObj)
    ForLoop ident expr obj -> undefined
    Objects objs -> undefined
    If expr obj maybeObj -> undefined
    BackgroundMod obj -> undefined
    DebugMod obj -> undefined
    RootMod obj -> undefined
    DisableMod obj -> undefined
    ModuleDef name args body -> undefined
    VarDef name value -> undefined
    FuncDef name args body -> undefined

prettyArguments :: [Argument Expr] -> Doc Text
prettyArguments args =
  align $ concatWithComma $ map (\(Argument expr) -> prettyExpr expr) args

prettyExpr :: Expr -> Doc Text
prettyExpr expr =
  case expr of
    EVar (Ident ident) -> undefined
    EIndex expr1 expr2 -> undefined
    ENum double -> P.pretty double
    EVec exprs -> "[" </> align (concatWithComma (map prettyExpr exprs)) </> "]"
     --ERange (Range Expr)
     --EString String
     --EBool Bool
     --EFunc Ident [Argument Expr]
     --ENegate Expr
     --EPlus Expr Expr
     --EMinus Expr Expr
     --EMult Expr Expr
     --EDiv Expr Expr
     --EMod Expr Expr
     --EEquals Expr Expr
     --ENotEquals Expr Expr
     --EGT Expr Expr
     --EGE Expr Expr
     --ELT Expr Expr
     --ELE Expr Expr
     --ENot Expr
     --EOr Expr Expr
     --EAnd Expr Expr
     --ETernary Expr Expr Expr
     --EParen Expr

t :: String -> Doc Text
t = P.pretty . pack

x <$$> y = x <> line <> y

x </> y = x <> softline <> y

concatWithComma = concatWith (\x y -> x <> "," </> y)
