module Main where

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import CSharpStatic
import SSM
import CSharpCode
import Prelude hiding ((<$), (<*), (*>))

import qualified Data.Map as M


main :: IO ()
main = do
    -- get command line arguments
    args  <- getArgs
    files <- case args of
              []  ->  do
                putStrLn "no argument given; assuming example.cs"
                --return ["test-cases/01-test-task01.cs"]
                return ["example.cs"]
              xs  ->  return xs
    -- translate each of the files
    processFiles files

processFiles :: [FilePath] -> IO ()
processFiles = mapM_ $ processFile
        . \f -> (f, addExtension (dropExtension f) "ssm")


-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process xs = formatCode
                   $ flip (foldCSharp codeAlgebra) (M.fromList (static xs), M.empty)
                   $ parsed xs
        static  = foldCSharp staticAlgebra
                . parsed
        parsed  = run (pClass <* eof)
                . run lexicalScanner

run :: Parser s a -> [s] -> a
run p = fst . head . filter (null . snd) . parse p
