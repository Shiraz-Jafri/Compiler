# Compiler

For many years, we have been surrounded by numerous programming languages that it's gotten harder to choose from. Whether you want to use Java, or Python, or some other langauge like C++, there is always a certainty when programming, which is that the computer is always able to understand the language. How does it do that? Whenever you start with a new programming language, you download the programming language and voila! You can write a program in that language and the computer is able to understand it. What's interesting about that is that when you download the programming language, it comes with a tool called a compiler. This tool takes program written in source language and translates it to the target machine language. 

Imagine you are trying to talk to someone in Spanish but you don't know any Spanish. Most likely, you will get a translator who will first hear the speaker talk in Spanish, and then translate it to you in your native language. Due to the translator, you are able to speak spanish in a way, which is neat, but now imagine if that translator is so fast that it translates anything in Spanish within seconds, to the point where you don't even have to think about it, it's natural. Sort of like speaking Spanish, when you are fluent in a language, you don't have to translate word by word, you just hear the phrase and immediately understand it. That's what a compiler does for a computer. 

## My Custom Compiler
I am an undergraduate at Indiana University Bloomington and with the knowledge I have obtained here in programming languages, I was able to create my own compiler that translates a subset of programming language called Racket into x86 Assembly. 
A compiler is made up of many steps, each step is responsible for a valuable operation that transforms the source code more and more into the target machine language. 
Our base language in this repo is Lvar, which is a very small language consisting or arithmetic operations +, -, negation, read (input), and variables/integers. 
The abstract syntax that will be used is listed below. 

#### Base Language (Lvar): 
Abstract Syntax:
```
	type ::= Integer
	exp ::= (Int int) | (Prim 'read ()) | (Prim '- (exp)) | (Prim '+ (exp exp)) | (Prim '- (exp exp))
	exp ::= (Var var) | (Let var exp exp)
	LVar ::= (Program ’() exp)
  ```

#### Intermediate Language (Cvar):
Abstract Syntax:
```
	atm ::= (Int int) | (Var var)
	exp ::= atm | (Prim 'read ()) | (Prim '- (atm))
	| (Prim '+ (atm atm)) | (Prim '- (atm atm))
	stmt ::= (Assign (Var var) exp)
	tail ::= (Return exp) | (Seq stmt tail)
	CVar ::= (CProgram info ((label . tail) …))
```

#### Source Language (X86Var):
Abstract Syntax:
```
	reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi |
	r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
	arg ::= (Imm int) | (Reg reg) | (Deref reg int)
	instr ::= (Instr addq (arg arg)) | (Instr subq (arg arg))
	| (Instr negq (arg)) | (Instr movq (arg arg))
	| (Instr pushq (arg)) | (Instr popq (arg))
	| (Callq label int) | (Retq) | (Jmp label)
	block ::= (Block info (instr …))
	x86Int ::= (X86Program info ((label . block)…))

```

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
Select Instructions is a step that translates Cvar into x86Var. In Lvar, we have total of three operations to translate, 

```(Prim '+ (list (Int n) (Int m))), (Prim '- (list (Int n) (Int m))), (Prim '- (list (Int n))), and (Prim 'read '()). ```

Considering we have an add operation, we can do it two ways, first is we store the first value in a register and add the second value to that same register. But as of this step, we don't have to worry about replacing variables, so the syntax can be simpler. 

Suppose we have the operation ``` (Prim '+ (list (Int 1) (Var 'x))) --> addq 1, x ``` Since we don't have to worry about variables, our translated programs are shorter. 

Suppose we have ``` (Prim '+ (list (Int 1) (Int 2))) --> movq 1, x then addq 2, x ``` This same syntax follows with every operation with the exception of 

``` (Let x1 (Prim 'read '()) (Var 'x1)) --> callq read_int then movq %rax, x1 ``` here read_int function gets called and the return value is stored in rax register which then is moved to the x1 variable. And lastly if an expression is an int value it gets converted to an immediate integer, consider ``` (Int 10) --> (Imm 10) ```.

### Step 5: Assign Homes
This step takes the result of select instructions and replaces all the variables with stack locations. The key thing about stack locations is that they have to be multiples of 8 as the stack gets more information, the variables convert to the according location in the stack. Consider a simple example,

``` movq $42, a. movq a, b. movq b, %rax. --> movq $42, -8(%rbp). movq -8(%rbp), -16(%rbp). movq -16(%rbp), %rax. ```

### Step 6: Patch Instructions
Similar to the previous step, this step takes care of a minor issue that may or may not occur after assign homes. Having two stack locations as arguments is not allowed, to tackle this problem, we take the first stack location, store it inside a rax register, and then move the second stack location to the rax register. Eliminating the need of two stack locations in the same operation. Consider the example,

``` movq $42, -8(%rbp). movq -8(%rbp), -16(%rbp). movq -16(%rbp), %rax. --> movq -8(%rbp), %rax. movq %rax, -16(%rbp). ```

### Step 7: Generate Prelude and Conclusion
The main function in x86 ``` pushq %rbp. movq %rsp, %rbp. subq $16, %rsp. jmp start ``` first starts off by subtracting 8 from the stack, then saves the base pointer at rsp which then replaces the base pointer with rsp (current pointer). And then finally the stack pointer moves down to make enough room for the variables. 

The conclusion function in x86 ``` addq $16, %rsp. popq %rbp. retq ``` The first instruction adds 16 to rsp which essentially moves the stack pointer back to the old base pointer. Then popq line restores the old base pointer to rbp and adds 8 to the stack pointer. And finally retq, jumps back to the procedure that called this one and adds 8 to the stack pointer. 

Through the use of these passes, we take the transformation of Lvar -> x86Int step by step. Most programming languages have their own unique functionality that has to be transformed in such a way to make up for the syntax of x86Int. Now, it's time to test the function in Racket and see it in action.

### HOW TO USE:

Compiler.rkt file is the main file to use. Other files are neccessary files that make sure that the programs follows the restriction of the different variations of the languages. Suppose we have a program (Prim '+ (list (Int 10) (Int 10.5)), now we know that you can add an integer and a float value, but for our compiler function to know this, we need to provide the proper type checkers to make sure that it follows our source langauge restrictions. 
  
Other file to mention is Utilities.rkt and runtime-config.rkt, this file is possibly the most important file because it provides all the structures that we use for our source language. Along with that, it also includes more structures that are basically upgrades to our source language. These necessary files are written by my Professor Jermey Siek. With his written structures and helpful functions, we don't have to worry about creating basic functions and instead, we can focus on the main funcionality of the compiler, in another words, we can focus on the main steps mentioned.

Lastly, there is a parser-lvar function to notice at the beginning of compiler.rkt, this function allows us to write our operations like 
``` (Prim '+ (list (Int 10) (Int 20))) ``` in basic Racket syntax. The purpose of this is purley to save time, so you can write a program faster, for example

``` (Prim '+ (list (Int 10) (Int 20))) --> '(+ 10 20) ```

Because the compiler functions automatically calls parser-lvar, you can just give an expression in racket syntax like so (make sure to include ' before the expression)
``` (compiler_x86 '(+ 2 3)) ```

#### AST form:
``` 
(X86Program
 '()
 (list
  (cons
   'start
   (Block
    '()
    (list
     (Instr
      'movq
      (list (Imm 1) (Reg 'rax)))
     (Instr
      'addq
      (list (Imm 2) (Reg 'rax)))
     (Jmp 'conclusion))))
  (cons
   'main
   (Block
    '()
    (list
     (Instr 'pushq (list (Reg 'rbp)))
     (Instr
      'movq
      (list (Reg 'rsp) (Reg 'rbp)))
     (Instr
      'subq
      (list (Imm 0) (Reg 'rsp)))
     (Jmp 'start))))
  (cons
   'conclusion
   (Block
    '()
    (list
     (Instr
      'addq
      (list (Imm 0) (Reg 'rsp)))
     (Instr 'popq (list (Reg 'rbp)))
     (Retq))))))
```

#### x86 Syntax:

```
.align 8
start:
	movq	$1, %rax
	addq	$2, %rax
	jmp conclusion

	.globl main
	.align 8
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$0, %rsp
	jmp start

	.align 8
conclusion:
	addq	$0, %rsp
	popq	%rbp
	retq
```

Reference: Textbook - Essentials of Compilation: An Incremental Approach in Racket/Python by Jeremy Siek
