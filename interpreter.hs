import Data.Char
import System.IO
import Data.Text (isInfixOf, pack)    
import System.Environment

------------------------------------------------------------------------------
-- Types

-- An instruction in BF is one character of source code
data Instruction = IncrPtr
    | DecrPtr
    | IncrData
    | DecrData
    | Print
    | Input
    | Loop [Instruction]
    | Comment Char

-- A program is the whole source code, represented as a list of Instructions
-- we create a new data type instead of typedefing so we can have a show instance for it
data Program = BrainfuckProgram [Instruction]

-- BF programs operate on a list of memory cells, where each cell holds one character
-- memory is inf tape represented by zipper
data Memory = Memory [MemoryCell] MemoryCell [MemoryCell] 

type MemoryCell = Int
------------------------------------------------------------------------------
-- Parser
parser :: [Char] -> Program
parser sourceCode = BrainfuckProgram $ parseInstr sourceCode []

parseInstr :: String -> [Instruction] -> [Instruction]
parseInstr (c:cs) instr = case c of
    '>' -> parseInstr cs (instr ++ [IncrPtr])
    '<' -> parseInstr cs (instr ++ [DecrPtr])
    '+' -> parseInstr cs (instr ++ [IncrData])
    '-' -> parseInstr cs (instr ++ [DecrData])
    '.' -> parseInstr cs (instr ++ [Print])
    ',' -> parseInstr cs (instr ++ [Input])
    '[' -> parseInstr (after) (instr ++ [Loop loopInstr])
        where
            loopInstr = parseInstr (loopContents) []
            (loopContents, after) = superBracket (c:cs)
    ']' -> parseInstr cs instr -- do nothing
    _ -> parseInstr cs instr

parseInstr "" instr = instr

superBracket :: String -> (String, String)
superBracket "" = ("","")
superBracket " " = (" ","")
superBracket string = ((tail . init) parsed, drop ((length parsed)) string)
    where 
        parsed = if length (brackets string) == 0 then "  " else (brackets string)

brackets :: String -> String
brackets string = go string 0 False
  where go (s:ss) 0 False | s /= '[' = go ss 0 False
        go ('[':ss) 0 False = '[' : go ss 1 True
        go (']':_) 1 True = "]"
        go (']':ss) n True = ']' : go ss (n-1) True
        go ('[':ss) n True = '[' : go ss (n+1) True
        go (s:ss) n flag = s : go ss n flag 
        go "" _ _ = ""

-- Checks for the only possible syntax error, which are mismatched parenthesis
checkSyntax :: String -> Bool
checkSyntax s = go s 0
    where
        go ('[':cs) cnt = go cs (cnt + 1)
        go (']':cs) cnt = 
            if cnt == 0 then False -- e.g. "[] ]" or "]["
            else go cs (cnt - 1)
        go (_:cs) cnt = go cs cnt
        go "" cnt = (cnt == 0)
------------------------------------------------------------------------------
-- Evaluator
evaluator :: Program -> Memory -> Bool -> IO Memory
evaluator (BrainfuckProgram (x:xs)) mem debug = case x of 
    DecrPtr -> 
                if (not debug) then evaluator (BrainfuckProgram xs) (moveLeft mem) debug
                else
                    (putStrLn . show) mem >> evaluator (BrainfuckProgram xs) (moveLeft mem) debug
    IncrPtr ->
            if (not debug) then evaluator (BrainfuckProgram xs) (moveRight mem) debug
                else
                    (putStrLn . show) mem >> evaluator (BrainfuckProgram xs) (moveRight mem) debug
    DecrData -> 
            if (not debug) then evaluator (BrainfuckProgram xs) (decr mem) debug
                else
                    (putStrLn . show) mem >> evaluator (BrainfuckProgram xs) (decr mem) debug
    IncrData -> 
            if (not debug) then evaluator (BrainfuckProgram xs) (incr mem) debug
                else
                    (putStrLn . show) mem >> evaluator (BrainfuckProgram xs) (incr mem) debug
    loop@(Loop l) ->              
                    if (not debug) then evalLoop
                        else  
                            (putStrLn . show) mem >> evalLoop
                        where
                            evalLoop = 
                                if (getCurrentCell mem == 0) 
                                    then evaluator (BrainfuckProgram xs) mem debug
                                else 
                                    evaluator (BrainfuckProgram (l ++ [loop] ++ xs)) mem debug -- evaluate inside of loop and then append loop to it again
    Input -> 
        do 
            c <- getChar
            evaluator (BrainfuckProgram xs) (modifyMemory (const (ord c)) mem) debug
    Print -> 
        do
            (putChar . chr . getCurrentCell) mem -- for printing chars
            hFlush stdout
            evaluator (BrainfuckProgram xs) mem debug

evaluator (BrainfuckProgram []) mem _ = return mem
 
-- corresponding to DecrPtr
moveLeft :: Memory -> Memory
moveLeft (Memory left center right) = Memory newLeft newCenter newRight
    where
        newLeft = tail left -- drop the first element
        newCenter = head left
        newRight = (center : right)

-- corresponding to IncrPtr
moveRight :: Memory -> Memory
moveRight (Memory left center right) = Memory newLeft newCenter newRight
    where
        newLeft = (center : left)
        newCenter = head right
        newRight = tail right

-- corresponding to IncrData
-- wraps from 0 to 255
incr :: Memory -> Memory
incr (Memory left val right) = Memory left (mod (val + 1) 255) right

-- corresponding to DecrData
-- wraps from 0 to 255
decr :: Memory -> Memory
decr (Memory left val right) = Memory left (mod (val + (-1)) 255) right

modifyMemory :: (MemoryCell -> MemoryCell) -> Memory -> Memory
modifyMemory func (Memory left center right) = Memory left (func center) right 

getCurrentCell :: Memory -> MemoryCell
getCurrentCell (Memory _ m _) = m

-- initialize memory with blanks on both sides, and current value set to 0
blankMemory :: Memory
-- blankMemory = Memory (repeat 0) 0 (repeat 0)
blankMemory = Memory (repeat 0) 0 (repeat 0)

------------------------------------------------------------------------------
-- REPL
main :: IO ()
main = do
    args <- getArgs
    case args of
        [prgm] -> interpret prgm False
        [prgm, "-d"] -> interpret prgm True
        ["-f", file] -> do
                contents <- readFile file
                interpret contents False
        ["-f", file, "-d"] -> do                 
                contents <- readFile file
                interpret contents True

        _ -> putStrLn "Usage: ./interpreter <program> [-d] or ./interpreter -f <src.bf> [-d], -d for debug mode"

interpret :: String -> Bool -> IO ()
interpret prgm debug = 
    do 
        if (checkSyntax prgm) then
            do
                mem <- evaluator (parser prgm) blankMemory debug
                putStrLn (show mem)
        else
            putStrLn "Mismatched brackets!"

------------------------------------------------------------------------------
-- Printing

instance Show Instruction where
    show IncrPtr = "IncrPtr "
    show DecrPtr = "DecrPtr "
    show IncrData = "IncrData "
    show DecrData = "DecrData "
    show Print = "Print "
    show Input = "Input "
    show (Loop l) = "Loop [" ++ (foldr (++) "" (map show l)) ++ "] "
    show (Comment c) = ""

instance Show (Program) where
    show (BrainfuckProgram a) = foldr (++) "" (map show a)

instance Show Memory where
    show (Memory left pointer right) = "\n\n..." ++ (showLeft left 10) ++ (showCenterCell pointer) ++ (showRight right 10) ++ "..."
        where
            showRight list numToPrint = (foldr (++) "" (take numToPrint (map showMemoryCell list))) 
            showLeft list numToPrint = (foldr (++) "" (reverse $ take numToPrint (map showMemoryCell list)))

-- for the cell currently pointed to
showCenterCell :: MemoryCell -> String
showCenterCell m = " [*" ++ (show m) ++ "*] "

-- for all other cells
showMemoryCell :: MemoryCell -> String
showMemoryCell m = "[" ++ (show m) ++ "]"

