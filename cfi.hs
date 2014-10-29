{-
Module      : Start of program
Description : Compares two files.
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Exprimental
Portability : Portable
Version     : v0.1-alpha

Compares two files and gives a report if
files are equal or what lines that differes.
Start profram with -f flag to write a report
to a file named YYYYMMDD_HHMMSS_filename1_filename2.txt.
-}

module Main where

import EvalFiles
import System.Environment

{-|
  Main function.
  Start of program.
-}
main :: IO ()
main = do args <- getArgs
          evalArgs args


{-|
  Prints a smalln copyright notice
  on terminal.
-}
copyrightPrint :: IO ()
copyrightPrint = do putStrLn ""
                    putStrLn "cfi  Copyright (C) 2014  Marcus Pedersén."
                    putStrLn "This program comes with ABSOLUTELY NO WARRANTY;"
                    putStrLn "for details use flag -c at program start."
                    putStrLn "This is free software, and you are welcome to redistribute it"
                    putStrLn "under certain conditions; use flag -c at program start for details."
                    putStrLn ""

{-|
  Evaluates argumens given on program start.

  Filters actions depending on arguments.
-}
evalArgs :: [String] -> IO ()
evalArgs (a:as) 
    | a == "-c" = longCopyrightPrint
    | a == "-h" = do copyrightPrint
                     helpPrint
    | a == "-f" = do copyrightPrint
                     evalFileArgs (a:as)
    | notDashFst a && length as == 1 = do copyrightPrint
                                          evalFileArgs (a:as)
    | otherwise = do copyrightPrint
                     wrongInput (a:as)
    where notDashFst (a:as) = a /= '-'

evalArgs [] = do copyrightPrint
                 wrongInput []

{-|
  Prints copyright notice on terminal.
-}
longCopyrightPrint :: IO ()
longCopyrightPrint = do copy <- readFile "copyright"
                        putStrLn copy

{-|
  Prints message about wrong arguments to terminal.
-}
wrongInput :: [String] -> IO ()
wrongInput args = do putStrLn "cfi called with wrong arguments."
                     argPrint args
                     putStrLn "Use -h flag for help on arguments: cfi -h"
                     putStrLn ""
                  where argPrint ("-f":[]) = do 
                            putStrLn "Argument '-f' must be given with two file arguments." 
                            putStrLn "As in: cfi -f filename1 filemane2"
                        argPrint ("-f":f1:[]) = argPrint ("-f":[])
                        argPrint ("-f":f1:f2:f3:_) = argPrint ("-f":[])
                        argPrint (a:as) = do
                            putStrLn ("Argument " ++ "'" ++ a ++ "'" ++ "is not valid.")
                            argPrint as
                        argPrint [] = return ()


{-|
  Prints help text to terminal.
-}
helpPrint :: IO ()
helpPrint = do putStrLn "Use: cfi [FLAG]"
               putStrLn " or: cfi [FLAG]...[FILE1]...[FILE2]"
               putStrLn "Compares two files and prints report to terminal."
               putStrLn "If flag '-f' is used report will be written to file."
               putStrLn ""
               putStrLn "Single argument flags without other arguments (cfi [FLAG])."
               putStrLn "  -c          A copyright notice will be written to terminal."
               putStrLn "  -h          Shows this help text."
               putStrLn ""
               putStrLn "Multi arguments with flag."
               putStrLn "  -f          Prints file compare report to file instead to terminal."
               putStrLn "              File name on report:"
               putStrLn "              YYYYMMDD_HHMMSS_Filename1_Filename2.txt"
               putStrLn "              Use: cfi -f Filename1 Filename2"
               putStrLn ""

{-|
  Evaluates arguments when '-f' flag is given
  or two arguments is given.
-}
evalFileArgs :: [String]-> IO ()
evalFileArgs (_:b:c:[]) = argFilesExist b c True
evalFileArgs (a:b:[])   = argFilesExist a b False
evalFileArgs as = wrongInput as