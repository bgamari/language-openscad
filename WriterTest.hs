module Main where
import qualified Data.ByteString.Char8 as BS

import Language.OpenSCAD
import Language.OpenSCAD.Writer

main :: IO ()
main = do
    scad <- BS.readFile "test.scad"
    mapM BS.putStrLn $ case parse scad of
        Left err -> [BS.pack err]
        Right tl -> write tl
    return ()
