------------------------------------
-- brainfuck to C                 --
--                    jaburns.net --
------------------------------------

import System.Environment( getArgs )
import System.IO

type BrainfuckProgram = String        -- A string of this type will only contain legal Brainfuck characters.
type ReducedBrainfuck = [(Int,Char)]  -- Represents a list of brainfuck instructions along with how many times they appear in a row.
type CProgram = String                -- String alias to represent a C program.

-- Given some input string, this function will return a new string containing only the valid BrainFuck instructions.
bfClean :: String -> BrainfuckProgram
bfClean "" = ""
bfClean (x:xs) = if elem x "><+-,.[]"
               then x:(bfClean xs)
               else    bfClean xs
               
-- Groups strings of the same brainfuck characters together and returns the character/count pairs.
bfReduce :: BrainfuckProgram -> ReducedBrainfuck
bfReduce "" = []
bfReduce (x:xs) = go 1 x xs
  where
    go acc char   []   = [(acc,char)]
    go acc char (x:xs) = if x == char
                       then go (acc+1) char xs
                       else (acc,char):( go 1 x xs )

-- Generates a valid, but not necessarily run-time safe, C program from reduced brainfuck code.
reducedBrainfuckToC :: ReducedBrainfuck -> CProgram
reducedBrainfuckToC [] = ""
reducedBrainfuckToC rbf = "main(){char tape[32767];char *ptr=tape;" ++ code rbf ++ "return 0;}"
  where
    code [] = ""
    code ((n,cmd):xs) =
      case cmd of
        '>' -> "ptr+="  ++ goNextNumerical
        '<' -> "ptr-="  ++ goNextNumerical
        '+' -> "*ptr+=" ++ goNextNumerical
        '-' -> "*ptr-=" ++ goNextNumerical
        '.' -> "putchar(*ptr);"  ++ consumedOne
        ',' -> "*ptr=getchar();" ++ consumedOne
        '[' -> "while(*ptr){"    ++ consumedOne
        ']' -> "}"               ++ consumedOne
        _   -> consumedOne
      where
        goNextNumerical = show n ++ ";" ++ code xs
        consumedOne = if n == 1
                    then code xs
                    else code ((n-1,cmd):xs)
                
-- Main: process the file provided as a parameter and output to stdout, or tell the user no parameter was passed.
main = do
  args <- getArgs
  if args /= [] then do
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStrLn . reducedBrainfuckToC . bfReduce . bfClean $ contents
  else do
    putStrLn "Please supply an input file name."

    