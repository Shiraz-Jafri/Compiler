#lang racket

(require racket/contract)
(provide
 (contract-out
  [rootstack-size (parameter/c exact-nonnegative-integer?)]
  [heap-size      (parameter/c exact-nonnegative-integer?)]))

;; Parameter that determines what the initial rootstack size of the program is.
;; in order to get this value use                (rootstack-size)
;; in order to set this value to (expt 2 8) use  (rootstack-size (expt 2 8))
(define rootstack-size
  (make-parameter (expt 2 16)))

;; Parameter that determines what the initial heap size of the program is.
(define heap-size 
  (make-parameter (expt 2 16)))
