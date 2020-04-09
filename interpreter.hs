import Data.Char
import System.IO

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
    | SyntaxError -- todo better error handling
    | Comment Char

-- A program is the whole source code, represented as a list of Instructions
-- we create a new data type instead of typedefing so we can have a show instance for it
data Program = BrainfuckProgram [Instruction]

-- BF programs operate on a list of memory cells, where each cell holds one character
-- A pointer points to the current memory cell the program is looking at
-- Left memory is in reverse order. 
-- e.g. the memory tape (...-4, -3, -2, -1, *0*, 1, 2, 3, 4...) would be represented by
-- Memory [-1, -2, -3, -4, ...] 0 [1, 2, 3, 4...]
data Memory = Memory [MemoryCell] MemoryCell [MemoryCell] 

type MemoryCell = Int

------------------------------------------------------------------------------
-- Parser
parser :: [Char] -> Program
-- parser sourceCode = BrainfuckProgram $ map parseSingleInstruction sourceCode
parser sourceCode = BrainfuckProgram $ parseInstr sourceCode []

-- assumes syntax errors have already been checked for
parseInstr :: String -> [Instruction] -> [Instruction]
parseInstr (c:cs) instr = case c of
    '>' -> parseInstr cs (instr ++ [IncrPtr])
    '<' -> parseInstr cs (instr ++ [DecrPtr])
    '+' -> parseInstr cs (instr ++ [IncrData])
    '-' -> parseInstr cs (instr ++ [DecrData])
    '.' -> parseInstr cs (instr ++ [Print])
    ',' -> parseInstr cs (instr ++ [Input])
    '[' -> parseInstr (drop (length (brackets cs)) cs) (instr ++ [Loop loopInstr])
        where
            loopInstr = parseInstr ((tail . init) (brackets cs)) []
            -- str = stringAfterMatchingBracket cs
    ']' -> parseInstr cs instr -- do nothing
    _ -> parseInstr cs instr

parseInstr "" instr = instr

brackets :: String -> String
brackets string = go string 0 False
  where go (s:ss) 0 False | s /= '[' = go ss 0 False
        go ('[':ss) 0 False = '[' : go ss 1 True
        go (']':_) 1 True = "]"
        go (']':ss) n True = ']' : go ss (n-1) True
        go ('[':ss) n True = '[' : go ss (n+1) True
        go (s:ss) n flag = s : go ss n flag 
        go "" _ _ = ""


-- -- returns string inside matching brackets assuming first char is open bracket
-- -- e.g. for "[abcdef]ghijkl", returns "abcdef"
-- stringInsideMatchingBracket :: String -> String -> Int -> String
-- stringInsideMatchingBracket (c:cs) currString parenCounter = 
--     if c == ']' 
--         then 
--             if parenCounter - 1 == 0 
--                 then currString
--             else stringInsideMatchingBracket cs (currString ++ [c]) (parenCounter - 1)
--     else if c == '[' then stringInsideMatchingBracket cs (currString ++ [c]) (parenCounter++)
--     else (stringInsideMatchingBracket cs (currString ++ [c]))
-- stringInsideMatchingBracket "" currString _ = currString 
    
-- -- returns string after matching brackets
-- -- e.g. for "[abcdef]ghijkl", returns "ghijkl"
-- stringAfterMatchingBracket :: String -> String 
-- stringAfterMatchingBracket (c:cs) = 
--     if c == ']' then cs
--     else stringAfterMatchingBracket cs
-- stringAfterMatchingBracket "" = ""


-- Checks for the only possible syntax error, which are mismatched parenthesis
checkSyntax :: Program -> Maybe (Program)
checkSyntax = undefined

------------------------------------------------------------------------------
-- Evaluator
evaluator :: Program -> Memory -> IO Memory
evaluator (BrainfuckProgram (x:xs)) mem = case x of 
    DecrPtr -> evaluator (BrainfuckProgram xs) (moveLeft mem)
    IncrPtr -> evaluator (BrainfuckProgram xs) (moveRight mem)
    DecrData -> evaluator (BrainfuckProgram xs) (decr mem)
    IncrData -> evaluator (BrainfuckProgram xs) (incr mem)
    loop@(Loop l) -> 
                do 
                    putStrLn ("loop" ++ show l ++ show mem)    
                    if (getCurrentCell mem == 0) 
                        then evaluator (BrainfuckProgram xs) mem
                    else 
                        evaluator (BrainfuckProgram (l ++ [loop])) mem -- evaluate inside of loop and then append loop to it again
    Input -> 
        do 
            -- putStrLn("Input: ")
            c <- getChar
            evaluator (BrainfuckProgram xs) (modifyMemory (const (ord c)) mem)
    Print -> 
        do
            -- putStrLn "Print: "
            putStrLn (show (getCurrentCell mem)) -- for printing decimal values, debugging only
            -- (putChar . chr . getCurrentCell) mem -- for printing chars
            -- hFlush stdout

            evaluator (BrainfuckProgram xs) mem

evaluator (BrainfuckProgram []) mem = return mem
 
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
showMemoryCell m = " [" ++ (show m) ++ "] "

