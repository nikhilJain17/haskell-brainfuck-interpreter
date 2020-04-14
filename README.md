# haskell-brainfuck-interpreter
A small interpreter for the Brainfuck language written in Haskell.

## About 
Brainfuck is an esoteric programming language invented by Urban Müller in 1992. Although it is Turing-complete, Brainfuck's terse syntax makes it very challenging (and fun) to do anything meaningful with it.

| Command       | Action           
| ------ |:-------------:|
| >      | increment pointer |
| <      | decrement pointer |
| +      | increment data |
| -      | decrement data |
| ,      | stdin (at current cell)  |
| .      | stdout (at current cell) |
| [      | if the current cell is 0, jump to the matching ] |
| ]      | if the current cell is not 0, jump to the matching [ |

Brainfuck's memory model is an infinite tape of memory cells that each hold an integer in [0, 255]. Since Haskell's lazy evaluation makes infinite lists possible, the memory tape was implemented as an infinite zipper data structure. The interpreter dumps the state of memory at the end of execution, and in debug mode, the interpreter will dump memory after each instruction execution.

## Usage

### Building
Make sure you have GHC installed, and run ```ghc -o brainfuck interpreter.hs``` to build.

### Running a program from source file
```./brainfuck -f <filename.bf>```

<img src="https://raw.githubusercontent.com/nikhilJain17/haskell-brainfuck-interpreter/master/assets/bf_file.gif" width="4000"/>


### Running a program from command line
```./brainfuck <program>```

<img src="https://raw.githubusercontent.com/nikhilJain17/haskell-brainfuck-interpreter/master/assets/bf_inputted.gif" width="4000"/>


### Running in debug mode
Debug mode dumps the state of memory after each instruction execution.
```./brainfuck <program> -d"``` or ```./brainfuck -f <filename.bf> -d```


<img src="https://raw.githubusercontent.com/nikhilJain17/haskell-brainfuck-interpreter/master/assets/debugmode.gif" width="4000"/>


