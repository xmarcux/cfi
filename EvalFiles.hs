{-| 
Module      : EvalFiles
Description : Evaluate and compare functions for two files.
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Exprimental
Portability : Portable
Version     : Alpha

Functions for evaluation and comparing of two files.
Handles the report on equality.
-}

module EvalFiles where

import System.Directory

{-|
  Type that represent file contents.
-}
type FileContent = String

{-|
  Prints error message to terminal if any of files does not exist.
  If both files exists evaluation is continued.
-}
argFilesExist :: String -> String -> Bool -> IO ()
argFilesExist f1 f2 toFile = do fstF <- doesFileExist f1
                                sndF <- doesFileExist f2
                                bothExists (existNo fstF sndF)
    where existNo file1 file2 | not file1 && not file2 = 0
                              | file1 && file2         = 1
                              | not file1 && file2     = -1
                              | file1 && not file2     = -2
          bothExists n | n ==  1 = putStrLn "Files exist."
                       | n ==  0 = do putStrLn ("Neither of file: " ++ f1 ++ " nor: " ++ f2 ++ " exists.")
                                      endErrTxt
                       | n == -1 = do putStrLn ("First file: " ++ f1 ++ " does not exist.")
                                      endErrTxt
                       | n == -2 = do putStrLn ("Second file: " ++ f2 ++ " does not exist.")
                                      endErrTxt
          endErrTxt = do putStrLn ""
                         putStrLn "Can not parse files as they do not exist."
                         putStrLn "Correct filenames and try again."
                         putStrLn ""

{-|
  Function takes contents for two files(splited in lines) to be compared,
  the default array of Strings that is the formated
  Strings stating the difference, usualy an empty array,
  and an integer counting up the number of rows.
  Usualy zero.
  Returning the formated report String from the parsing
  of the content.
-}
parseFiles :: [FileContent] -> [FileContent] -> [String] -> Int -> String
parseFiles (c:cs) (f:fs) report count
    | c == f    = parseFiles cs fs report (count + 1)
    | otherwise = parseFiles cs fs (writeReport (count + 1) report) (count +1)
    where writeReport rowNo rep = ("Row number: " ++ show rowNo ++ "does not match."):
                                  ("File1 row: " ++ c):("File2 row: " ++ f):rep

                               