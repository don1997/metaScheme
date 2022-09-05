;; What is this??
;; It is a bare minimum way to get input and output in the repl and essentially can only eval strings and numbers.
;; Basically a pretty printer but I wanted to see how to get I/O working in a bare minimum system.

#lang racket

;; mini eval
(define (eval exp)
  (cond
  ((self-eval? exp) exp)
  ((quoted? exp) text-of-quotation exp)
  (else
   error "UKNOWN")))


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
