{-# LANGUAGE OverloadedStrings #-}
module Language.OpenSCAD.Writer where
import Language.OpenSCAD
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Data.Text.Prettyprint.Doc (vsep, group, Doc)
import qualified Data.Text.Prettyprint.Doc as P

pretty :: [TopLevel] -> Doc Text
pretty = vsep . map prettyTopLevel


prettyTopLevel :: TopLevel -> Doc Text
prettyTopLevel x = case x of
    TopLevelScope obj    -> prettyObject True obj
    UseDirective str     -> group $ vsep $ map t ["use", "<" <> (fromString str) <> ">"]
    IncludeDirective str -> group $ vsep $ map t ["include", "<" <> (fromString str) <> ">"]


prettyObject :: Bool -> Object -> Doc Text
prettyObject isTopLevel obj = case obj of
     Module (Ident ident) args maybeObj ->
        group $ vsep $
            [ t $ (fromString ident) <> "()" ]
            ++ maybeToList (fmap (prettyObject False) maybeObj)
            ++ if isTopLevel then [t ";"] else []
     ForLoop ident expr obj     -> undefined
     Objects objs               -> undefined
     If expr obj maybeObj       -> undefined
     BackgroundMod obj          -> undefined
     DebugMod obj               -> undefined
     RootMod obj                -> undefined
     DisableMod obj             -> undefined
     ModuleDef name args body   -> undefined
     VarDef name value          -> undefined
     FuncDef name args body     -> undefined

t :: Builder -> Doc Text
t = P.pretty . toLazyText
