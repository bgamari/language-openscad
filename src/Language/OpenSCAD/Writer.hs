{-# LANGUAGE OverloadedStrings #-}

module Language.OpenSCAD.Writer where

import           Data.Maybe                (fromMaybe, maybeToList)
import           Data.Text.Lazy            (Text, pack)
import           Data.Text.Prettyprint.Doc (Doc, group, hardline, line,
                                            softline, vsep)
import qualified Data.Text.Prettyprint.Doc as P
import           Language.OpenSCAD

pretty :: [TopLevel] -> Doc Text
pretty xs = vsep (map prettyTopLevel xs) <> hardline

prettyTopLevel :: TopLevel -> Doc Text
prettyTopLevel x =
  case x of
    TopLevelScope obj    -> prettyObject obj
    UseDirective str     -> "use" </> "<" <> t str <> ">"
    IncludeDirective str -> "include" </> "<" <> t str <> ">"

prettyObject :: Object -> Doc Text
prettyObject obj =
  case obj of
    Module (Ident ident) args maybeObj ->
      group $
      t ident <>
      "()" <> fromMaybe ";" (fmap (mappend line . prettyObject) maybeObj)
    ForLoop ident expr obj   -> undefined
    Objects objs             -> undefined
    If expr obj maybeObj     -> undefined
    BackgroundMod obj        -> undefined
    DebugMod obj             -> undefined
    RootMod obj              -> undefined
    DisableMod obj           -> undefined
    ModuleDef name args body -> undefined
    VarDef name value        -> undefined
    FuncDef name args body   -> undefined

t :: String -> Doc Text
t = P.pretty . pack

x <$> y = x <> line <> y

x </> y = x <> softline <> y
