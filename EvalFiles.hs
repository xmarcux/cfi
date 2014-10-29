{-| 
Module      : EvalFiles
Description : Evaluate and compare functions for two files.
Copyright   : (C) Marcus Pedersén, 2014

License     : GPLv3
Maintainer  : marcux@marcux.org
Stability   : Exprimental
Portability : Portable
Version     : v0.1-alpha

Functions for evaluation and comparing of two files.
Handles the report on equality.
-}

module EvalFiles where

import System.Directory
import System.Time

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
          bothExists n | n ==  1 = readFiles f1 f2 toFile
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
  Read files from disc and calls the compare function
  and prints the report to terminal or
  file depending on argument toFile.
-}
readFiles :: String -> String -> Bool -> IO ()
readFiles file1 file2 toFile = do
    f1 <- readFile file1
    f2 <- readFile file2

    c <- getClockTime
    ct <- toCalendarTime c
    putStrLn $ unlines (repStr f1 f2 ct) ++ parseFiles (lines f1) (lines f2) [] 0 0
    where repStr f1 f2 ct  =  "Difference report":
                              "===================":
                              dateStr ct:"":
                              ("File1: " ++ file1): 
                              ("Total number of rows: " ++ show (length $ lines f1)):
                              ("File2: " ++ file2):
                              ("Total number of rows: " ++ show (length $ lines f2)):
                              "":[]


{-|
  Returns a formated date and time string in format: YYYY-MM-DD HH:MM:SS
-}
dateStr :: CalendarTime -> String
dateStr ct = show (ctYear ct) ++ "-" ++ monthNo (ctMonth ct) ++ "-" ++ padStr (ctDay ct) ++
             " " ++ padStr (ctHour ct) ++ ":" ++ padStr (ctMin ct) ++ ":" ++ padStr (ctSec ct)
             where padStr  d     | d < 10             = "0" ++ show d
                                 | otherwise          = show d
	     	   monthNo month | month == January   = "01"
                                 | month == February  = "02"
                                 | month == March     = "03"
                                 | month == April     = "04"
                                 | month == May       = "05"
                                 | month == June      = "06"
                                 | month == July      = "07"
                                 | month == August    = "08"
                                 | month == September = "09"
                                 | month == October   = "10"
                                 | month == November  = "11"
                                 | month == December  = "12"

{-|
  Function takes contents for two files (splited in lines) to be compared,
  the default array of Strings that is the formated
  Strings stating the difference, usualy an empty array,
  and an integer counting up the number of rows parsed
  and an integer counting up the number of different rows.
  Both usualy zero.
  Returning the formated report String from the parsing
  of the content.
-}
parseFiles :: [FileContent] -> [FileContent] -> [String] -> Int -> Int -> String
parseFiles (c:cs) (f:fs) report count diffCount
    | c == f    = parseFiles cs fs report (count + 1) diffCount
    | otherwise = parseFiles cs fs (writeReport (count + 1) report) (count +1) (diffCount + 1)
    where writeReport rowNo rep = rep ++ 
                                  ("Row number: " ++ show rowNo ++ " does not match."):
                                  ("File1 row " ++ (show rowNo) ++ ": " ++ c):
                                  ("File2 row " ++ (show rowNo) ++ ": " ++ f):"":[]
parseFiles [] _ report count diffCount = sumParse report count diffCount
parseFiles _ [] report count diffCount = sumParse report count diffCount

{-|
  The final text from the parsing of the files.
  Number of rows searched and number of rows that differs.
-}
sumParse :: [String] -> Int -> Int -> String
sumParse report count diffCount =
    unlines $ ("Number of rows searched: " ++ show count):diffStr:"":"":
               "Differences:":"============":"":report
    where diffStr | diffCount == 0 = "FILES ARE EQUAL!!!"
                  | otherwise      = "Number of rows that are different: " ++ (show diffCount)
