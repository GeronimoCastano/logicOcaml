# OCaml-Based Logical Expression Compiler

## Project Overview
This project is a deep dive into the core components of compiler design through functional programming in OCaml. It's structured around creating a compiler that includes a lexer, parser, evaluator, and a suite of logical functions, all entirely coded in the Functional Programming style of OCaml. It is aimed to handle logical expressions.

### Core Components
- **lexer.ml**: Contains the lexer, which tokenizes input strings of logical expressions into a structured series of tokens.
- **parser.ml**: Contains the parser, that uses tokens to build an abstract syntax tree (AST), reflecting the grammar and precedence of the logical language.
- **expression.ml**: Contains the grammar for the language, and also functions like evaluation, which recursively navigates the AST to determine the truth value of expressions.
- **logicfunctions.ml**: Contains all the functions that actually handle the logical expressions. It contains also a truth table generator.

### Functional Programming in Focus
Leveraging OCaml's functional programming paradigms, this project emphasizes immutability, recursion, and higher-order functions, offering a clear and concise approach to compiler construction.

## Key Features in `Logicfunctions.ml`
The `Logicfunctions.ml` module introduces several powerful functions for analyzing logical expressions:

- **`truth_table`**: Takes a string representation of a logical expression and outputs the value of the expression in all possible environments for its variables. 
- **`print_truth_table`**: A custom function designed to print the output of `truthTable` in a readable format.
- **`is_tautology`**, **`is_satisfiable`**: These functions also take string inputs, analyzing expressions for tautology and satisfiability, respectively.

### Logical Operators and Syntax
The language supports the following operators, with variables being any length of capital letters, except for reserved constants "F" (false) and "T" (true):

- **And**: Represented as `&`
- **Or**: Represented as `|`
- **Xor**: Represented as `^`
- **Implies**: Represented as `->`
- **Iff (If and only if)**: Represented as `<->`
- **Not**: Represented as `~`

## Getting Started

### Prerequisites
- OCaml
- OPAM

### Installation
1. Clone the repository:

`git clone https://github.com/GeronimoCastano/logicOcaml.git`

2. Build the project with Dune:
   `dune build`


## Usage

To run the project and evaluate logical expressions along with generating truth tables: write the logical expression in a string format inside of the main file using the logical functions, and then run the file in the terminal with:

`dune exec bin/main.exe`




## Contributing
Your contributions are welcome! Feel free to submit pull requests, open issues, or suggest improvements.

