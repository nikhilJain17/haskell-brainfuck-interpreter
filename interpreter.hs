import Data.Char

------------------------------------------------------------------------------
-- Types

-- An instruction in BF is one character of source code
data Instruction = IncrPtr
    | DecrPtr
    | IncrData
    | DecrData
    | Print
    | Input
    | LoopBegin
    | LoopEnd
    | Comment Char

-- A program is the whole source code, represented as a list of Instructions
data Program = BrainfuckProgram [Instruction]

-- BF programs operate on a list of memory cells, where each cell holds one character
-- A pointer points to the current memory cell the program is looking at
data Memory = Memory [MemoryCell] MemoryCell [MemoryCell] 

type MemoryCell = Int

------------------------------------------------------------------------------
-- Parser
parser :: [Char] -> Program
parser sourceCode = BrainfuckProgram $ map parseSingleInstruction sourceCode

parseSingleInstruction :: Char -> Instruction
parseSingleInstruction '>' = IncrPtr 
parseSingleInstruction '<' =  DecrPtr
parseSingleInstruction '+' =  IncrData
parseSingleInstruction '-' =  DecrData
parseSingleInstruction '.' =  Print
parseSingleInstruction ',' =  Input
parseSingleInstruction '[' =  LoopBegin
parseSingleInstruction ']' =  LoopEnd
parseSingleInstruction c = Comment c

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
    Input -> 
        do 
            c <- getChar
            evaluator (BrainfuckProgram xs) (modifyMemory (const (ord c)) mem)
            -- where
            --     newMem = modifyMemory (const (ord c)) mem
    Print -> 
        do
            putChar (chr (getCurrentCell mem))
            evaluator (BrainfuckProgram xs) mem

evaluator (BrainfuckProgram []) mem = return mem



-- corresponding to DecrPtr
moveLeft :: Memory -> Memory
moveLeft (Memory left center right) = Memory newLeft newCenter newRight
    where
        newLeft = init left
        newCenter = last left
        newRight = (center : right)

-- corresponding to IncrPtr
moveRight :: Memory -> Memory
moveRight (Memory left center right) = Memory newLeft newCenter newRight
    where
        newLeft = (left ++ [center])
        newCenter = head right
        newRight = tail right

-- corresponding to IncrData
incr :: Memory -> Memory
incr = modifyMemory (+1)

-- corresponding to DecrData
decr :: Memory -> Memory
decr = modifyMemory (+(-1))

modifyMemory :: (MemoryCell -> MemoryCell) -> Memory -> Memory
modifyMemory func (Memory left center right) = Memory left (func center) right 

getCurrentCell :: Memory -> MemoryCell
getCurrentCell (Memory _ m _) = m

-- initialize memory with blanks on both sides, and current value set to 0
blankMemory :: Memory
-- blankMemory = Memory (repeat 0) 0 (repeat 0)
blankMemory = Memory [1..4] 0 [1..4]

------------------------------------------------------------------------------
-- REPL
------------------------------------------------------------------------------
-- Printing

instance Show Instruction where
    show IncrPtr = ">"
    show DecrPtr = "<"
    show IncrData = "+"
    show DecrData = "-"
    show Print = "."
    show Input = ","
    show LoopBegin = "["
    show LoopEnd = "]"  
    show (Comment c) = show c

instance Show (Program) where
    show (BrainfuckProgram a) = foldr (++) "" (map show a)

instance Show Memory where
    show (Memory left pointer right) = "..." ++ (showList left 10) ++ (showCenterCell pointer) ++ (showList right 10) ++ "..."
        where
            showList list numToPrint = (foldr (++) "" (take numToPrint (map showMemoryCell list))) 

-- for the cell currently pointed to
showCenterCell :: MemoryCell -> String
showCenterCell m = " [*" ++ (show m) ++ "*] "

-- for all other cells
showMemoryCell :: MemoryCell -> String
showMemoryCell m = " [" ++ (show m) ++ "] "
