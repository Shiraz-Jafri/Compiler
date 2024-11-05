#lang racket

(require racket/set racket/stream)
(require racket/fixnum)
(require "type-check-Lvar.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))
(require racket/dict)
(require racket/trace) 

(define (parser-lvar e)
  (match e
    [`,n #:when (number? n)
         (Int n)]
    [`,x #:when (symbol? x)
         (Var x)]
    [`(let ((,x ,e)) ,body)
     (Let x (parser-lvar e)
          (parser-lvar body))] 
    [`(+ ,exp1 ,exp2)
     (define val1 (parser-lvar exp1))
     (define val2 (parser-lvar exp2))
     (Prim '+ (list val1 val2))]
    [`(- ,exp1 ,exp2)
     (define val1 (parser-lvar exp1))
     (define val2 (parser-lvar exp2))
     (Prim '- (list val1 val2))]
    [`(- ,exp)
     (Prim '- (list (parser-lvar exp)))]
    [`(read)
     (Prim 'read '())])) 

(define (test-uniquify expE actualE)
  (when (not (equal? expE actualE))
    (error "Bad")))   

(define test1 (Let 'y (Int 3) (Let 'y (Int 3) (Var 'y))))
(define test2 (Let 'x (Let 'x (Int 4) (Var 'x)) (Var 'x)))
(define test3 (Let 'x (Let 'x (Int 4) (Var 'x)) (Let 'y (Let 'z (Int 4) (Int 4)) (Var 'x))))
;; Step 1
; Responsible for transforming programs such that duplicate variables don't exist.
; No Variable Shadowing
;; uniquify : Lvar -> Lvar

(define (append-s-n sym num)
  (string->symbol (string-append (symbol->string sym) (number->string num))))

(define generate-unique-number-uni
  (let ([counter 0])
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

(define (uniquify p)
  (begin
    (match p
      [(Program info e) (Program info ((uniquify-exp '()) e))]
      )))

(define (uniquify-exp env)
  (lambda (e)
    (begin
      (match e
        [(Var x)
         (Var (append-s-n x (dict-ref env x)))]
        [(Int n) (Int n)]
        [(Let x e body)
         (let* ([new-num (generate-unique-number-uni)]
                [new-name (append-s-n x new-num)]
                [new-env (dict-set env x new-num)]
                [un-e ((uniquify-exp new-env) e)]
                [un-body ((uniquify-exp new-env) body)])
           (Let new-name
                un-e
                un-body))]
        [(Prim op es)
         (Prim op (for/list ([e es])
                    ((uniquify-exp env) e)))]))))

;; Step 2
;; remove-complex-opera* : Lvar -> Lvar
; Responsible for minimizing complex operation

(define (remove-complex-opera* p)
  (match p
    [(Program info e)
     (Program info
              (rco-exp e (make-hash)))]
    ))


(define (reverse-ls ls)
  (cond [(empty? ls) empty]
        [(empty? (cdr ls)) (car ls)]
        [else (append (reverse (cdr ls))
                      (cons (car ls)
                            empty))]))

(define (keyNum key)
  (let ([str (symbol->string key)])
    (string->number (substring
                     str
                     4))))

(define (numKey num)
  (string->symbol
   (string-append "tmp." (number->string num))))

(define (getEnv ls)
  (map numKey (sort (map keyNum ls)
                    >)))

(define (getKey env key count)
  (cond [(dict-has-key? env key)
         (string->symbol(string-append "tmp" "."
                                       (number->string
                                        (add1
                                         count))))] 
        [else (string->symbol
               (string-append "tmp" "."
                              (number->string
                               1)))]))

; rco-atm : Expression Alist -> Expression Alist


(define (rco-atm e env count)
  (match e
    [(Prim '+ (list exp1 exp2))
     (values (let-values ([(res env^)
                           (rco-atm exp1 env (dict-count env))])
               (let-values
                   ([(res^ env^^)
                     (rco-atm exp2 env^
                              (dict-count env^))])
                 (let ([val (Prim '+ (list
                                      res
                                      res^))])
                   (let ([ke (getKey env^^ 'tmp.1
                                     (dict-count env^^))])
                     (let ([val (Prim '+
                                      (list
                                       res
                                       res^))]) 
                       (dict-set! env^^ ke val))
                     (Var ke)) 
                   )))  
             env)] 
    [(Prim '- (list exp1 exp2))
     (values (let-values ([(res env^)
                           (rco-atm exp1 env (dict-count env))])
               (let-values
                   ([(res^ env^^)
                     (rco-atm exp2 env^
                              (dict-count env^))])
                 (let ([val (Prim '- (list
                                      res
                                      res^))])
                   (let ([ke (getKey env^^ 'tmp.1
                                     (dict-count env^^))])
                     (let ([val (Prim '-
                                      (list
                                       res
                                       res^))])
                       
                       (dict-set! env^^ ke val))
                     (Var ke)) 
                   )))  
             env)]
    [(Prim '- (list exp))
     (values (let-values ([(res e)
                           (rco-atm exp env (dict-count env))])
               (let ([ke (getKey env 'tmp.1
                                 (dict-count env))])
                 (let ([val (Prim '- (list
                                      res))])
                   (dict-set! env ke val)
                   (Var ke)))) env)]
    [(Prim 'read '())
     (values (let ([ke (getKey env 'tmp.1 (dict-count env))])
               (dict-set! env ke e)
               (Var ke))
             env)] 
    [(Let x e body)
     (values (let ([ke (getKey env 'tmp.1 (dict-count env))])
               (let ([val (Let x (rco-exp e env)
                               (rco-exp body env))])
                 (dict-set! env ke val)
                 (Var ke))) env)]
    [else (values (rco-exp e env)
                  env)])) 

(define (rco-exp e env)
  (match e
    [(Int n)
     (Int n)]
    [(Var x)
     (Var x)]
    [(Let x e body)
     (Let x (rco-exp e (make-hash)) (rco-exp body env))]
    [(Prim 'read '())
     (let-values ([(v env^)
                   (rco-atm e env (dict-count env))])
       (Let (Var-name v) (dict-ref env^ (Var-name v))
            v))] 
    [(Prim op expr)
     (let* ([res (Prim op (for/list ([e expr])
                            (let-values ([(s e)
                                          (rco-atm e env
                                                   (dict-count env))])
                              s)))]
            [env^ (make-hash)])
       (list-ref
        (for/list ([e expr])
          (let-values ([(r env^^)
                        (rco-atm e env^
                                 (dict-count env^))])
            (for/fold ([last res])
                      ([bind (dict-count env^^)]
                       [key (getEnv
                             (dict-keys env^^))])
              (if (dict-has-key? env^^ key)
                  (Let key (dict-ref 
                            env^^
                            key)
                       last)
                  r)))) 
        (sub1 (length expr))))]))

(define ft2 (Program '() (Prim 'read '())))
(define ft3 (Program '() (Prim '+ (list (Int 47) (Prim '- (list (Int 5)))))))
(define ft4 (Program '() (Prim '+ (list (Prim '+ (list (Int 10) (Int 11))) (Prim '+ (list (Int 25) (Prim '- (list (Int 4)))))))))
(define ft7 (Program '() (Let 'x (Int 20) (Prim '+ (list (Let 'x (Int 22) (Var 'x)) (Var 'x))))))

(define test4 (Let 'x (Int 3) (Prim '+ (list (Var 'x) (Int 10)))))

; Step 3
;; explicate-control : Lvar -> Cvar
; responsible for making sure the order of operations is clear.

(define (explicate-control p)
  (match p
    [(Program info body)
     (CProgram info (list (list* 'start (explicate_tail body))))]))

(define req-ex
  (lambda (proc1)
    (match proc1
      [(Seq stmt tail) (req-ex tail)]
      [(Return exp) exp])))

(define cont-concat
  (lambda (proc1 proc2)
    (match proc1
      [(Seq stmt tail) (Seq stmt (cont-concat tail proc2))]
      [(Return exp) proc2])))

(define (explicate_tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body) (explicate_assign rhs x (explicate_tail body))]
    [(Prim op es) (Return (Prim op es))]
    [else (error "explicate_tial unhandled case" e)]))

(define (explicate_assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Let y rhs body) (let*
                          ([r-r (explicate_tail (Let y rhs body))]
                           [r-r-ret (Seq (Assign (Var x) (req-ex r-r)) cont)])
                        (cont-concat r-r r-r-ret))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
    [else (error "explicate_assign unhandled case" e)]))

(define c1 (Seq (Assign (Var 'x) (Int 3)) (Seq (Assign (Var 'y) (Int 4)) (Return (Var 'x)))))
(define c2 (Seq (Assign (Var 'z) (Int 4)) (Return (Var 'z))))

(define s1 (Instr 'addq (list 'Rax 'Rax)))

(define test7 (Program '() (Let 'x (Int 3) (Let 'y (Int 4) (Prim '+ (list (Var 'x) (Var 'y)))))))
(define test8 (Program '() (Let 'x (Let 'y (Int 6) (Var 'y)) (Int 5))))
(define test9 (Program '() (Int 42)))
(define test10 (CProgram '() (list (cons 'start (Return (Int 42))))))
(define test11 (explicate-control test8))

; Step 4
;; select-instructions : Cvar -> x86Var
; Translates Cvar into x86Var.
; Takes each operation and converts it into x86Var syntax.

(define (select-instructions p)
  (match p
    [(CProgram info labels)
     (X86Program info (list (lab-t (car labels))))]))

(define lab-t
  (lambda (lab)
    (match lab
      [`(,lab . ,tail)
       (list* lab (Block '() (selins-tail tail)))
       ])))

(define (selins-atm e)
  (match e
    [(Int n)
     (Imm n)]
    [(Var x)
     (Var x)]
    [else (selins-stmt e)]))

(define (selins-stmt e)
  (match e
    [(Prim '+ (list (Int x) (Int y)))
     (list (Instr 'movq (list (Imm x) (Reg 'rax)))
           (Instr 'addq (list (Imm y) (Reg 'rax))))]
    [(Prim '+ (list (Int n) (Var x)))
     (list (Instr 'addq (list (Imm n) (Var x))))]
    [(Prim '+ (list (Var x) (Int n)))
     (list (Instr 'addq (list (Imm n) (Var x))))]
    [(Prim '+ (list (Var x) (Var y)))
     (list (Instr 'addq (list (Var x) (Var y))))]
    [(Prim '+ expr)
     (list (Instr 'addq (for/list ([e expr])
                          (selins-atm e))))]
    [(Prim '- (list (Int n) (Int m)))
     (list (Instr 'movq (list (Imm n) (Reg 'rax)))
           (Instr 'subq (list (Imm m) (Reg 'rax))))]
    [(Prim '- (list (Int n) (Var x)))
     (list (Instr 'subq (list (Imm n) (Var x))))]
    [(Prim '- (list (Var x) (Int n)))
     (list (Instr 'subq (list (Imm n) (Var x))))]
    [(Prim '- (list (Var x) (Var y)))
     (list (Instr 'subq (list (Var x) (Var y))))] 
    [(Prim '- (list (Int n)))
     (list (Instr 'movq (list (Imm n) (Reg 'rax)))
           (Instr 'negq (list (Reg 'rax))))]
    [(Prim '- (list (Var x)))
     (list (Instr 'negq (list (Var x))))]
    [(Prim 'read '())
     (list (Callq 'read_int 0)
           (Instr 'movq (list (Reg 'rax)
                              (Var 'x))))]   
    [(Assign v e)
     (match e
       [(Int num) (list (Instr 'movq (list (Imm num) v)))]
       [(Var x) (list (Instr 'movq (list (Var x) v)))]
       [else (surgery2 (selins-atm e) v)])]
    [else (list (selins-tail e))])) 

(define (selins-tail e)
  (match e
    [(Seq s t)
     (append (selins-stmt s)
             (selins-tail t))]
    [(Return exp)
     (let ([res (selins-atm exp)])
       (cond
         [(list? res) (surgery res)]
         [else (list (Instr 'movq (list res (Reg 'rax))))]))])) 

(define (surgery lst)
  (cond [(null? lst) (error "The list is empty.")]
        [(null? (cdr lst))
         (match (car lst)
           [(Instr sth1 (list var (Reg 'rax)))
            (list (Instr sth1 (list var (Reg 'rax))))]
           [(Instr sth (list var1 var2))
            (list
             (Instr sth (list var1 var2))
             (Instr 'movq (list var2 (Reg 'rax))))]
           [(Instr sth (list var1))
            (list
             (Instr sth (list var1))
             (Instr 'movq (list var1 (Reg 'rax))))]
           )]
        [else (cons (car lst) (surgery (cdr lst)))]))

(define (surgery2 lst sthx)
  (cond [(null? lst) (error "The list is empty.")]
        [(null? (cdr lst))
         (match (car lst)               
           [(Instr sth1 (list var (Reg 'rax)))
            (list (Instr sth1 (list var (Reg 'rax)))
                  (Instr 'movq (list (Reg 'rax) sthx)))]
           [(Instr sth (list var1 var2))
            (list
             (Instr sth (list var1 var2))
             (Instr 'movq (list var2 sthx)))]
           [(Instr sth (list var1))
            (list
             (Instr sth (list var1))
             (Instr 'movq (list var1 sthx)))])]
        [else (cons (car lst) (surgery2 (cdr lst) sthx))])) 
 
(define tt2 (CProgram '((locals-types)) (list (cons 'start (Return (Int 42))))))
(define tt3 (CProgram '((locals-types)) (list (cons 'start (Return (Prim '+ (list (Int 20) (Int 22))))))))
(define tt4 (CProgram '((locals-types (x1 . Integer))) (list (cons 'start (Seq (Assign (Var 'x1) (Int 41)) (Return (Prim '+ (list (Var 'x1) (Int 1)))))))))



;; merge 2 dict
(define (merge-dict dict1 dict2)
  (cond
    [(empty? dict1) dict2]
    [(dict-has-key? dict2 (caar dict1))
     (merge-dict (cdr dict1) dict2)]
    [else (merge-dict (cdr dict1) (cons (car dict1) dict2))]))

;; List of instruction -> alist of types

(define (var-ent arg)
  (match arg
    [(Var x) (dict-set '() (Var x) 'Integer)]
    [else '()]))

(define (alot loi)
  (cond
    [(empty? loi) '()]
    [else (let*
              ([first-dict (match (car loi)
                             [(Instr strq args)
                              (match args
                                [(list arg1) (var-ent arg1)]
                                [(list arg1 arg2)
                                 (merge-dict (var-ent arg1) (var-ent arg2))])]
                             [(Callq 'read_int 0)
                              '()])])
            (merge-dict first-dict (alot (cdr loi))))]))

;; labels to info
(define (lti labels)
  (cond
    [(empty? labels) '()]
    [else (merge-dict (alot (match (cdar labels)
                              [(Block info instrs) instrs])) (lti (cdr labels)))]))

;Info to stack locations, use -8

(define (itos info accu)
  (cond
    [(empty? info) '()]
    [else (cons (cons (caar info) accu) (itos (cdr info) (- accu 8)))]))

(define sta-lo-hold '())
; Step 5
;; assign-homes X86Var -> X86Int
; Responsible for converting x86Var to x86Int
; Makes sure that there are no more variables, instead there are stack locations.

(define (assign-homes p)
  (begin
    (match p
      [(X86Program info labels)
       (X86Program info
                   (map (begin
                          (set! sta-lo-hold
                                (* -8
                                   (length
                                    (lti labels))))
                          (ah-label (itos
                                     (lti labels) -8)))
                        labels))])))

(define ttt1
  (X86Program
   (list (cons (Var 'd) 'Integer) (cons (Var 'c) 'Integer))
   (list
    (cons 'start
          (Block '()
                 (list (Instr 'movq (list (Imm 42) (Var 'c)))
                       (Instr 'movq (list (Var 'c) (Var 'd)))
                       (Instr 'movq (list (Var 'd) (Reg 'rax)))))))))
(define ttt2 (X86Program '((locals-types)) (list (cons 'start (Block '() (list (Instr 'movq (list (Imm 42) (Reg 'rax)))))))))
(define ttt3 (X86Program '((locals-types)) (list (cons 'start (Block '() (list (Instr 'movq (list (Imm 20) (Reg 'rax))) (Instr 'addq (list (Imm 22) (Reg 'rax)))))))))
(define ttt4 (X86Program '((locals-types (x1 . Integer))) (list (cons 'start (Block '() (list (Instr 'movq (list (Imm 41) (Var 'x1))) (Instr 'addq (list (Imm 1) (Var 'x1))) (Instr 'movq (list (Var 'x1) (Reg 'rax)))))))))

(define (ah-label sta-lo)
  (lambda (lab)
    (match lab
      [`(,lab . ,block)
       (list* lab ((ah-block sta-lo)block))])))

(define (ah-block sta-lo)
  (lambda (expr)
    (match expr
      [(Block info instrs) (Block info (map (ah-instr sta-lo) instrs))])))

(define (ah-instr sta-lo)
  (lambda (expr)
    (match expr
      [(Instr strq args)
       (Instr strq (match args
                     [(list arg1) (list ((ah-arg sta-lo) arg1))]
                     [(list arg1 arg2)
                      (let* ([arg1c ((ah-arg sta-lo) arg1)]
                             [arg2c ((ah-arg sta-lo) arg2)])
                        (list arg1c arg2c))]))]
      [(Callq label int) (Callq label int)])))  

(define (ah-arg sta-lo)
  (lambda (expr)
    (match expr
      [(Imm int) (Imm int)]
      [(Reg 'rax) (Reg 'rax)]
      [(Var x) (Deref 'rbp (dict-ref sta-lo (Var x)))])))

(define b1
  (list
   (list* 'start
          (Block '()
                 (list
                  (Instr 'movq (list (Imm 42) (Var 'c)))
                  (Instr 'movq (list (Var 'c) (Var 'd)))
                  (Instr 'movq (list (Var 'd) (Reg 'rax))))))))



(define p1
  (assign-homes (X86Program (lti b1) b1)))

; Step 6
;; patch-instructions : x86Int -> x86Int
; Responsible for making sure there are at most one stack location in one instruction

(define (patch-inst p)
  (match p
    [(X86Program info labels) (X86Program info
                                          (map patch-label labels))]))
(define (patch-label lab)
  (match lab
    [`(,lab . ,block)
     (list* lab (patch-block block))]))

(define (patch-block block)
  (match block
    [(Block info instrs) (Block info (p-instrs instrs '()))]))

(define (cont-ret env element)
  (cond
    [(dict-has-key? env element) (dict-ref env element)]
    [else element]))

(define pair-of-mem?
  (lambda (x)
    (match x
      [(list arg1 arg2)
       (match arg1
         [(Deref reg num)
          (match arg2
            [(Deref reg num) #t]
            [else #f])]
         [else #f])]
      [else #f])))

(define check-valid
  (lambda (arg)
    (match arg
      [(Imm num) (if (>= num 65536)
                     (error "Invalid Input")
                     #t)]
      [else #t])))

(define p-instrs
  (lambda (exprs env)
    (cond
      [(empty? exprs) '()]
      [else
       (match (car exprs)
         [(Callq label int)
          (cons (Callq label int)
                (p-instrs (cdr exprs) env))]
         [(Instr sth (list arg1))
          (begin
            (check-valid arg1)
            (cons (Instr sth (list (cont-ret env arg1)))
                  (p-instrs (cdr exprs) env)))]
         [(Instr sth (list arg1 arg2))
          (begin
            (check-valid arg1)
            (check-valid arg2)
            (let ([new-Instr (Instr sth (list (cont-ret env arg1)
                                              (cont-ret env arg2)))])
              (match new-Instr
                [(Instr sth (list arg1 arg2))
                 (if (pair-of-mem? (list arg1 arg2))
                     (cons (Instr sth (list arg1 (Reg 'rax)))
                           (p-instrs (cdr exprs)
                                     (cons (list* arg2 (Reg 'rax)) env)))
                     (cons (Instr sth (list arg1 arg2))
                           (p-instrs (cdr exprs) env)))])))])]))) 
 
; This function scan through instrs and find duplicates
(define scan-dup
  (lambda (exprs)
    (cond
      [(empty? exprs) '()]
      [else (append
             (match (car exprs)
               [(Instr sth (list arg1))
                (match arg1
                  [(Imm int) (if (> int 65536)
                                 (error "very big integer")
                                 '())]
                  [else '()])]
               [(Instr sth (list arg1 arg2))
                (match arg1
                  [(Deref reg num)
                   (match arg2
                     [(Deref reg num) (list (list* arg2 (Reg 'rax)))]
                     [else '()])]
                  [else '()])])
             (scan-dup (cdr exprs)))])))

(define tttt1
  (list (Instr 'movq (list (Imm 42) (Deref 'rbp -16))) (Instr 'movq (list (Deref 'rbp -16) (Deref 'rbp -8))) (Instr 'movq (list (Deref 'rbp -8) (Reg 'rax)))))
; Step 7
;; prelude-and-conclusion : x86int -> x86int
; Last step, responsible for wrapping the translated program with main and conclusion block

(define main-generator
  (lambda (sta-lo)
    (list* 'main (Block '() (list
                             (Instr 'pushq (list (Reg 'rbp)))
                             (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                             (Instr 'subq (list (Imm sta-lo) (Reg 'rsp)))
                             (Jmp 'start))))))

(define conclusion-generator
  (lambda (sta-lo)
    (list* 'conclusion
           (Block '() (list
                       (Instr 'addq (list (Imm sta-lo) (Reg 'rsp)))
                       (Instr 'popq (list (Reg 'rbp)))
                       (Retq)
                       )))))
(define two-min
  (lambda (a b)
    (if (> a b)
        b
        a)))

(define big-sta-lo
  (lambda (instrs bst)
    (cond
      [(empty? instrs) bst]
      [else
       (match (car instrs)
         [(Jmp somewhere) (big-sta-lo (cdr instrs) bst)]
         [(Callq label int) (big-sta-lo (cdr instrs) bst)]
         [(Instr sth (list arg1))
          (match arg1
            [(Deref reg sta-num)
             (big-sta-lo (cdr instrs) (two-min sta-num bst))]
            [else (big-sta-lo (cdr instrs) bst)])]
         [(Instr sth (list arg1 arg2))
          (match arg1
            [(Deref reg sta-num1)
             (match arg2
               [(Deref reg sta-num2)
                (big-sta-lo instrs
                            (two-min (two-min sta-num2 bst) sta-num1))]
               [else (big-sta-lo (cdr instrs) (two-min sta-num1 bst))])]
            [else (big-sta-lo (cdr instrs) bst)])])])))

(define (prelude-and-conclusion p)
  (match p
    [(X86Program info labels)
     (X86Program
      info
      (let*([start
             (match (car labels)
               [`(,lab . ,block)
                (list* 'start (match block
                                [(Block info instrs)
                                 (Block
                                  info
                                  (append instrs
                                          (list (Jmp 'conclusion))))]))])]
            [num
             (match (car labels)
               [`(,lab . ,block)
                (match block
                  [(Block info instrs) (abs (- (big-sta-lo instrs 0)
                                               (modulo (big-sta-lo instrs 0)
                                                       16)))])])])
        (begin
          (list
           start
           (main-generator num)
           (conclusion-generator num)))))]))

; compiler_x86 : Lvar (Racket Sub Expression) -> String (x86 syntax)
(define (compiler_x86 e)
  (printf (print-x86 (prelude-and-conclusion
   (patch-inst (assign-homes
                (select-instructions
                 (explicate-control
                  (remove-complex-opera*
                   (uniquify
                    (type-check-Lvar (Program '()
                                              (parser-lvar e)))))))))))))

; compiler_ast : Lvar (Racket Sub Expression) -> x86Int
(define (compiler_ast e)
  (prelude-and-conclusion
   (patch-inst (assign-homes
                (select-instructions
                 (explicate-control
                  (remove-complex-opera*
                   (uniquify
                    (type-check-Lvar (Program '()
                                              (parser-lvar e)))))))))))

