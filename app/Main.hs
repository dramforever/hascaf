module Main where

import qualified Data.Text.IO as T
import           Hascaf.Utils.Compile
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.FilePath (replaceExtension)
import           System.IO (hPutStr, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [src] -> work src Nothing
        [src, out] -> work src (Just out)
        _ -> do
            prog <- getProgName
            hPutStr stderr $ unlines
                [ "Usage:"
                , "   " ++ prog ++ " <source.c>"
                , "   " ++ prog ++ " <source.c> <out.S>"
                ]
            exitWith (ExitFailure 1)

work :: FilePath -> Maybe FilePath -> IO ()
work src out = do
    contents <- T.readFile src
    case compile src contents of
        Left err -> do
            T.hPutStr stderr $ err
            exitWith (ExitFailure 1)
        Right output ->
            case out of
                Just outFile -> T.writeFile outFile output
                Nothing -> T.putStr output
