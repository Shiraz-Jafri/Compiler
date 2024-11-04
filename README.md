# Compiler

For many years, we have been surrounded by numerous programming languages that it's gotten harder to choose from. Whether you want to use Java, or Python, or some other langauge like C++, there is always a certainty when programming, which is that the computer is always able to understand the language. How does it do that? How is it able to always understand the programming language? Well, we have compilers to thank for that.
Imagine that you have a person who's job is to take any language in the world, and convert it to your native language. Because of this person, you are able to understand all the languages in the world, neat right? Sort of like a real time translator who can immediately translate a phrase in your native language within seconds that you don't even have to think about the orginal language spoken to you. 
That's how a compiler acts in the programming world, a tool that will take a program in a language and convert it so that the cpu can understand it.

## My Custom Compiler
I am an undergraduate at Indiana University Bloomington and with the knowledge I have obtained here, I was able to create my own compiler that translates a subset of programming language called Racket into x86 Assembly. 
A compiler is made up of many steps, each step is responsible for a valuable operation that transforms the source code more and more into target machine language. 

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


  
