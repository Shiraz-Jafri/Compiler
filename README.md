# Compiler

For many years, we have been surrounded by numerous programming languages that it's gotten harder to choose from. Whether you want to use Java, or Python, or some other langauge like C++, there is always a certainty when programming, which is that the computer is always able to understand the language. How does it do that? Whenever you start with a new programming language, you download the programming language and voila! You can write a program in that language and the computer is able to understand it. What's interesting about that is that when you download the programming language, it comes with a tool called a compiler. This tool takes program written in source language and translates it to the target machine language. 

Imagine you are trying to talk to someone in Spanish but you don't know any Spanish. Most likely, you will get a translator who will first hear the speaker talk in Spanish, and then translate it to you in your native language. Due to the translator, you are able to speak spanish in a way, which is neat, but now imagine if that translator is so fast that it translates anything in Spanish within seconds, to the point where you don't even have to think about it, it's natural. Sort of like speaking Spanish, when you are fluent in a language, you don't have to translate word by word, you just hear the phrase and immediately understand it. That's what a compiler does for a computer. 

## My Custom Compiler
I am an undergraduate at Indiana University Bloomington and with the knowledge I have obtained here in programming languages, I was able to create my own compiler that translates a subset of programming language called Racket into x86 Assembly. 
A compiler is made up of many steps, each step is responsible for a valuable operation that transforms the source code more and more into the target machine language. 

### Step 1: Uniquify
Uniquify is a step that is very crucial in identifying each variable with its proper value. In some programming languages, there is a concept called variable shadowing which allows different variables to have the same name but different values. Let's look at an example with let expressions. To understand let expressions, think of an expression with a restricted scope. 

```(Let x 5 (+ x x)) ``` In this example, we created an expression that returns 10, with a variable x that's bound to 5, we cannot use this x anywhere outside of this let expression. So let's look at an example of how uniquify works.

``` (Let x 10 (Let x 5 (+ x 5))) ``` In this example, we have two variables with the same name, the inner x is mapped to 5, but the outer x is mapped to 10 but it's confusing to read this and the more the complex the program gets, the harder it will become to differentiate all the 'x' variables.

With the help of Uniquify, we can transform the program so that it has unique variables.

``` (Let x 10 (Let x 5 (+ x 5))) ``` --> ``` (Let x1 10 (Let x2 5 (+ x2 5)) ```. Resulting in unique variables with no shadowing. 

### Step 2: Remove Complex Operation
Following Uniquify, we have remove complex operations which takes complex operation and maps it to a variable. This is an important step that allows our source language to be restricted to only have atomic arguments to operations. Let's consider an example, 

``` (+ 5 2) ``` In this + operation, we have two atomic values which is fine, but ``` (+ 5 (- 2)) ``` in this expression, we have a complex operation ``` (- 2) ``` as an argument which is problematic, so to tackle this problem, we map the operation to a variable making it atomic. ``` (Let 'tmp.1 (- 2) (+ 5 tmp.1)) ``` Now if you notice, + expression no longer contains an operation as an argument rather it's a variable and a value, which makes it atomic. Let's consider a more complex example,

``` (Let x1 5 (Let y1 10 (+ (+ x1 y1) (- 2)))) ``` transforms into ``` (Let x1 5 (Let y1 10 (Let tmp.1 (+ x1 y1) (Let tmp.2 (- 2) (+ tmp.1 tmp.2))))) ```
In this resulting expression, we can tell that the expression ``` (+ (+ x1 y1) (- 2)) ``` has two operations as arguments which is problematic. So once again, to tackle this, the arguments get mapped to variables. ``` tmp.1 = (+ x1 y1), tmp.2 = (- 2) --> (+ tmp.1 tmp.2) ```.

### Step 3: Explicate Control
This Step is essential to Racket because unlike most popular languages, Racket isn't explicit with its syntax, that is, it's not easy to tell in what order are the operations executing. To tackle this problem and get one step closer to our target language, we transform Lvar language to Cvar language. 

Let's consider two basic examples, ``` (Let x1 5 (+ x1 5)) --> start: x1 = 5; return x1 + 5; ``` We first map the value 5 to variable x1 and then return the expression. The order of operation here is easy to understand. But one thing to note is that, the right handside of let expression is executed before the body, that is, 5 is evaluated then the body ``` (+ x1 5) ``` is evaluated. 

Consider a more complex example, ``` (Let x1 (Let y1 5 (Var y1)) (Prim '+ (list (Var 'x1) (Int 10)))) ```.

In this example, ``` (Let y1 5 (Var y1)) ``` would be executed first then the body will be executed. So the program above translates into Cvar which results in

``` (cons 'start (Seq (Assign (Var 'y1) (Int 5)) (Seq (Assign (Var 'x1) (Var 'y1)) (Return (Prim '- (list (Var 'x))))))) ```.

In simpler terms to understand better: ``` start: y1 = 5; x1 = y1; return -x1  ``` 

Now due to this step, we can very clearly tell what is executing first and so on. Racket can make it harder to tell which operation is executing first, which is why this step is a crucial step which translates Lvar into an intermediate language Cvar. 

### Step 4: Select Instructions


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


  
