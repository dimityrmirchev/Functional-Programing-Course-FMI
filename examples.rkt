#lang racket

(define (length l)
  (if (equal? l '()) 0
      (+ 1 (length (cdr l)))))

(define (1+ x) (+ 1 x))

(define (++ x) (cons x '(1)))

(define (map f l)
  (if (equal? l '()) '()
      (cons (f (car l)) (map f (cdr l)))))

(define (filter c? l)
  (cond ((equal? l '()) '())
      ( (c? (car l))
        (cons (car l) (filter c? (cdr l))))
         (else (filter c? (cdr l)))))

(define (my-filter c? l)
  (cond ((null? l) null)
        ((c? (car l))
         (cons (car l) (my-filter c? (cdr l))))
        (else (my-filter c? (cdr l)))))

(define (g5? x)
  (if (> x 5) #t
      #f))

(define (acumulatel op nv l term)
  (if (equal? l '()) nv
      (op (term (car l)) (acumulatel op nv (cdr l) term))))


(define f (lambda (x) (+ x 1)))

(define fn
  (lambda (op1 op2)
    (lambda (x y)
      (op1 (op1 x y) x))))