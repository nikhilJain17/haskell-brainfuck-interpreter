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

-- A program is the whole source code, represented as a list of Instructions
data Program = BrainfuckProgram [Instruction]

instance Show (Program) where
    show (BrainfuckProgram a) = foldr (++) "" (map show a)

-- BF programs operate on a list of memory cells, where each cell holds one character
-- A pointer points to the current memory cell the program is looking at
data Memory = Memory [MemoryCell] MemoryCell [MemoryCell] 

instance Show Memory where
    show (Memory left pointer right) = (showList left 5) ++ (showCenterCell pointer) ++ (showList right 5)
        where
            showList list numToPrint = foldr (++) "" (take numToPrint (map showMemoryCell list))

-- for the cell currently pointed to
showCenterCell :: MemoryCell -> String
showCenterCell m = "[*" ++ (show m) ++ "*]"

-- for all other cells
showMemoryCell :: MemoryCell -> String
showMemoryCell m = "[" ++ (show m) ++ "]"

type MemoryCell = Int

-- initialize memory with blanks on both sides, and current value set to 0
blankMemory :: Memory
blankMemory = Memory (repeat 0) 0 (repeat 0)
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

-- -- corresponding to DecrPtr
-- moveLeft :: Memory -> Memory
-- moveLeft (Memory left center right) = Memory newLeft newCenter newRight
--     where
--         newLeft = init left
--         newCenter = le
--         newRight = (center : right)

-- corresponding to IncrPtr