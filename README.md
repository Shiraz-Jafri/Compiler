# Compiler

COMPILER
HOW TO USE:

  Compiler.rkt is a file that's been built by smaller passes that includes a small description of each of the passes.
  To get a better understanding each pass, it is recommended to call each pass function to differentiate their behavior.
  my_compiler function takes all the passes function and applies them all together resulting in an X86 AST form.
  To compare whether my_compiler outputs the same evaluated expression as Racket, use interp-x86 function from included interp.rkt file.
  
Coupler Disclaimers about Base Programming Language:
  This compiler will not be taking Racket programming language and converting to x86, it takes a variation of it called Lvar which is built using Racket
  
Abstract Syntax of Lvar:
```
  type ::= Integer
  exp ::= (Int int) | (Prim 'read ()) | (Prim '- (exp)) | (Prim '+ (exp exp)) | (Prim '- (exp exp))
  exp ::= (Var var) | (Let var exp exp)
  LVar ::= (Program ’() exp)
  (You can find more information on how Lvar works in the included Utilities.rkt file).
  ````

Although the given Abstract Sytanx of Lvar, because my_compiler applies parser-lvar, you can give an expression in Racket, parser-lvar function will convert given
Racket program into the Abstract Syntax of Lvar, make sure that it doesn't include a feature that's not included in syntax of Lvar.

Reference: Textbook - Essentials of Compilation: An Incremental Approach in Racket/Python by Jeremy Siek


  
