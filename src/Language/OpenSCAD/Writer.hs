module Language.OpenSCAD.Writer where
import Language.OpenSCAD
import qualified Data.ByteString.Char8 as BS

write :: [TopLevel] -> [BS.ByteString]
write = map writeTopLevel

writeTopLevel :: TopLevel -> BS.ByteString
writeTopLevel x = case x of
    TopLevelScope obj    -> writeObject obj
    UseDirective str     -> BS.pack ("use <" ++ str ++ ">")
    IncludeDirective str -> BS.pack ("include <" ++ str ++ ">")

writeObject :: Object -> BS.ByteString
writeObject obj = BS.pack ""
