# Compiler

For many years, we have been surrounded by numerous programming languages that it's gotten harder to choose from. Whether you want to use Java, or Python, or some other langauge like C++, there is always a certainty when programming, which is that the computer is always able to understand the language. How does it do that? Whenever you start with a new programming language, you download the programming language and voila! You can write a program in that language and the computer is able to understand it. What's interesting about that is that when you download the programming language, it comes with a tool called a compiler. This tool takes program written in source language and translates it to the target machine language. 
Imagine you are trying to talk to someone in Spanish but you don't know any Spanish. Most likely, you will get a translator who will first hear the speaker talk in Spanish, and then translate it to you in your native language. Due to the translator, you are able to speak spanish in a way, which is neat, but now imagine if that translator is so fast that it translates anything in Spanish within seconds, to the point where you don't even have to think about it, it's natural. Sort of like speaking Spanish, when you are fluent in a language, you don't have to translate word by word, you just hear the phrase and immediately understand it. That's what a compiler does for a computer. 

## My Custom Compiler
I am an undergraduate at Indiana University Bloomington and with the knowledge I have obtained here in programming languages, I was able to create my own compiler that translates a subset of programming language called Racket into x86 Assembly. 
A compiler is made up of many steps, each step is responsible for a valuable operation that transforms the source code more and more into the target machine language. 

### Step 1: Uniquify
Uniquify is a step that is very crucial in identifying each variable with its proper value. In some programming languages, there is a concept called variable shadowing which allows different variables to have the same name but different values. Let's look at an example with let expressions. To understand let expressions, think of an expression with a restricted scope. ```(Let x 5 (+ x x)) ``` In this example, we created an expression that returns 10, with a variable x that's bound to 5, we cannot use this x anywhere outside of this let expression. So let's look at an example of how uniquify works.
``` (Let x 10 (Let x 5 (+ x 5))) ``` In this example, we have two variables with the same name, the inner x is mapped to 5, but the outer x is mapped to 10
With the help of Uniquify, we can transform the program so that it has unique variables.
``` (Let x 10 (Let x 5 (+ x 5))) ``` transforms to ``` (Let x1 10 (Let x2 5 (+ x2 5)) ```

### Step 2: Remove Complex Operation
Following Uniquify, we have remove complex operations which takes complex operation and maps it to a variable. This is an important step that allows our source language to be restricted to only have atomic arguments to operations. Let's consider an example, 
``` (+ 5 2) ``` In this + operation, we have two atomic values which is fine, but ``` (+ 5 (- 2)) ``` in this expression, we have a complex operation as an argument which is problematic, so to tackle this problem, we map the operation to a variable making it atomic. ``` (Let 'tmp.1 (- 2) (+ 5 tmp.1)) ``` Now if you notice, + expression no longer contains an operation as an argument rather it's a variable and a value, which makes it atomic. Let's consider a more complex example,
``` (Let x 5 (Let y 10 (+ (+ x y) (- 2)))) ``` transforms into ``` (Let x 5 (Let y 10 (Let tmp.1 (+ x y) (Let tmp.2 (- 2) (+ tmp.1 tmp.2))))) ``` In this resulting expression, we can tell that the expression (+ (+ x y) (- 2)) has two operations as arguments which is problematic. So once again, to tackle this, the arguments get mapped to variables. tmp.1 = (+ x y), tmp.2 = (- 2) --> (+ tmp.1 tmp.2)
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


  
