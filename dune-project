(lang dune 3.11)

(name logic)

(generate_opam_files true)

(source
 (github GeronimoCastano/logicOcaml))

(authors "Geronimo Castano")

(maintainers "Geronimo Castano")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name logic)
 (synopsis "This project is a hands-on journey into compiler design using OCaml's functional programming strengths. It breaks down the process into creating a lexer, parser, and evaluator from scratch, all aimed at handling logical expressions. The focus here is on using pure functional programming techniques, which means we're all about immutability, recursion, and leveraging higher-order functions to get the job done.")
 (description "### Project Description: OCaml-Based Logical Expression Compiler

This project represents a foray into the intersection of functional programming and compiler design, implemented entirely in OCaml. It's an exploration of how to build a compiler's core components—the lexer, parser, and evaluator—using the principles of functional programming to handle logical expressions. The goal is to create a system that not only processes and evaluates logical expressions but does so with the precision and elegance afforded by OCaml's functional paradigm.

**Core Components:**

- **Lexer**: The lexer's role is to take raw input strings representing logical expressions and break them down into a sequence of tokens. These tokens are the basic building blocks for the parser, representing logical operators, variables, and constants in a structured form that's easy to manipulate programmatically.

- **Parser**: With the tokens ready, the parser steps in to construct an abstract syntax tree (AST). This process adheres strictly to the defined grammar and precedence rules of the logical expressions, ensuring that the structure of the AST accurately reflects the intentions behind the input expression. The parser uses recursive descent, a method well-suited to functional programming, to navigate through tokens and build the tree.

- **Evaluator**: Once we have an AST, the evaluator takes over. It recursively traverses the tree, evaluating the logical expressions it represents. This step calculates the truth value of the entire expression based on the values assigned to variables and the definitions of the logical operators. The evaluator exemplifies the power of recursion and immutability in functional programming, providing clear and concise logic for computation.

- **Truth Table Generator**: As a practical application of the evaluator, this component generates truth tables for given expressions. It systematically varies the truth values of variables, evaluates the expression for each combination, and outputs a list of results showing how the expression's truth value changes with the variables. This feature is particularly useful for visualizing the behavior of logical expressions and for educational purposes.

**Functional Programming in Focus**:

The project is a testament to the strengths of functional programming, particularly its emphasis on immutability and pure functions. By avoiding side effects and using recursion and higher-order functions, it achieves a level of clarity and simplicity in code that's both powerful and elegant. This approach not only makes the code more understandable but also facilitates reasoning about the program's behavior and correctness.

**For the Community**:

Designed with clarity in mind, this project aims to serve as an educational tool for those interested in compiler construction, functional programming, or the OCaml language. It demonstrates practical compiler design concepts and functional programming techniques in a real-world context, making it a valuable resource for learners and enthusiasts alike.")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
