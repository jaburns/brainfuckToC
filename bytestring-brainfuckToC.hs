------------------------------------
-- brainfuck to C                 --
--                    jaburns.net --
--
-- modified for bytestring by     --
--                      latermuse --
------------------------------------

import System.Environment (getArgs)
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B hiding (uncons)
import Data.Maybe
import Data.ByteString.Char8 (putStrLn)

main = do
    args <- getArgs
    if args /= [] 
        then do
            contents <- (B.readFile . head) args
            (putStrLn . reducedBrainfuckToC . bfReduce . bfClean) contents
        else 
            putStrLn $ B.fromString "Please supply an input file name."


-- Generates a valid, but not necessarily run-time safe, C program from
-- reduced brainfuck code.
reducedBrainfuckToC x = cHeader `B.append` code x `B.append` cFooter
  where
    code x = if x == [] 
                then B.empty
                else conditions (head x) (tail x)
    conditions x y
        | snd x == hf ">" = B.fromString "p+=" `B.append` goNext (fst x) y
        | snd x == hf "<" = B.fromString "p-=" `B.append` goNext (fst x) y
        | snd x == hf "+" = B.fromString "*p+=" `B.append` goNext (fst x) y
        | snd x == hf "-" = B.fromString "*p-=" `B.append` goNext (fst x) y
        | snd x == hf "." = B.fromString "putchar(*p);" `B.append` uncurry consumedOne x y
        | snd x == hf "," = B.fromString "*p=getchar();" `B.append` uncurry consumedOne x y
        | snd x == hf "[" = B.fromString "while(*p){" `B.append` uncurry consumedOne x y
        | snd x == hf "]" = B.fromString "}" `B.append` uncurry consumedOne x y
        | otherwise = B.empty
      where
        goNext x y = (B.fromString . show $ x) `B.append` B.fromString ";" `B.append` code y
        consumedOne x y z = if x == 1
                                then code z
                                else code ((x - 1,y):z)
        hf = B.head . B.fromString

-- Pure C code for generating output
cHeader = B.fromString "main(){char a[32767];char*p=a;" :: B.ByteString
cFooter = B.fromString "return 0;}" :: B.ByteString

-- Given an input bytestring, this function returns a new bytestring containing only valid brainfuck instructions.
bfClean :: B.ByteString -> B.ByteString
bfClean x 
    | B.null x = B.empty
    | B.head x `B.elem` validElements = B.head x `B.cons` bfClean (B.tail x)
    | otherwise = bfClean $ B.tail x

-- Elements of bf code
validElements :: B.ByteString
validElements = B.fromString "><+-,.[]"

-- Groups bytestrings of the same brainfuck characters together
-- Returns character/count pairs
bfReduce x 
    | B.null x = []
    | otherwise = go (1 :: Int) (B.head x) (B.tail x)
  where
    --go :: Int -> Word8 -> B.ByteString -> [(Int, Word8)]
    go i x xs
        | B.null xs = [(i, x)]
        | otherwise = 
            if B.head xs == x
                then go (i + 1) x (B.tail xs)
                else (i,x) : go 1 (B.head xs) (B.tail xs)

