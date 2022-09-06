;; What is this??
;; It is a bare minimum way to get input and output in the repl and essentially can only eval strings and numbers.
;; Basically a pretty printer but I wanted to see how to get I/O working in a bare minimum system.

#lang racket

;;EVAL
;; mini eval
(define (eval exp)
  (cond
  ((self-eval? exp) exp)
  ((variable? exp) lookup-variable-value exp env)
  ((quoted? exp) text-of-quotation exp)
  ((assignment? exp)
   (eval-assignment exp env))
  ((definition? exp)
   (eval-definition exp env))
  ((if? exp)
   (eval-if exp env))
  ((lambda? exp)
   (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))
  ((begin? exp)
   (eval-sequence
    (begin-actions exp)
    env))
  ;;cond?
  ((application? exp)
   (apply(eval (operator exp) env)
         (list-of-values
          (operands exp)
          env)))
  (else
   error "Uknown expression type: EVAL" exp)))

;;APPLY
(define (apply-self procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arugments))
  (else
   (error "Uknown procedure type: APPLY"
          procedure))))


;; Constructor
(define (make-procedure) )

;; Selector self-eval?
(define (self-eval? exp)
  (cond
    ((number? exp) #t)
    ((string? exp) #t)
    (else #f)))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (quoted? exp)
  (tagged-list? exp `quote))
(define (text-of-quotation exp)
  (cadr exp))

(define (variable? exp)
  (symbol? exp))

(define (assignment? ) )

(define (definition? ) )

(define (if? ) )

(define (lambda? ) )

(define (begin? ) )

(define (primitive-procedure? ) )

;; cond?

(define (application? ) )

;; eval-helpers
(define (eval-assignment) )

(define (eval-definition ) )

(define (eval-if) )

(define (eval-sequence) )

;;apply-helpers
(define (apply-primitive-procedure ) )

;; I/O stuff....

(define input-prompt ";;; M-Eval input:")

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define output-prompt ";;; M-Eval value:")

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
      (display object))

(define (driver-loop)
   (prompt-for-input input-prompt)
   (let ([input (read)])
     (let ([output (eval input)])
       (announce-output output-prompt)
       (user-print output)))
   (driver-loop))

;;init interpreter
(driver-loop)
