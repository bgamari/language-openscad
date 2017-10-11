{-# LANGUAGE OverloadedStrings #-}

import Language.OpenSCAD
import qualified Data.ByteString as BS
import System.Environment
import System.Exit

main :: IO ()
main = do
    file <- head `fmap` getArgs
    result <- parse <$> BS.readFile file
    case result of
      Left err
        | length err > 50 -> putStrLn err >> exitWith (ExitFailure 1)
        | otherwise       -> putStrLn $ "warning: "++err
      Right a    -> print a
