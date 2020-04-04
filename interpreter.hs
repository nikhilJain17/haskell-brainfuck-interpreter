
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