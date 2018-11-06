#lang racket

(define (filter p? list)
  (if (null? list) '()
      (if (p? (car list)) (cons (car list) (filter p? (cdr list))) (filter p? (cdr list)))))
 
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))
 
(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))
 
(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op (op nv (car l)) (cdr l))))

;------------------------------------------

(define (filterm p? l)
  (if (null? l) '()
      (if (p? (car l)) (cons (car l) (filterm p? (cdr l)))
          (filterm p? (cdr l)))))

(define (accumulatem op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulatem op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op (term a) nv) (next a) b term next)))

(define (foldrm op nv l)
  (if (null? l) nv
      (op (car l) (foldrm op nv (cdr l)))))

(define (foldlm op nv l)
  (if (null? l) nv
      (foldlm op (op nv (car l)) (cdr l))))


;(define (length n)
;  (if (and (< n 10) (> n -10)) 1
;      (+ 1 (length (quotient n 10)))))

(define (length-l l)
  (if (null? l) 0
      (+ 1 (length-l (cdr l)))))

;COMPOSE FUNCTION
(define (compose f g) (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (1+ x) (+ 1 x))

;REPEATED FUNCTION
(define (repeated f n)
  (lambda (x) (if (= n 0) x (f ((repeated f (- n 1)) x)))))

;DERIVE FUNCTIONS
(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  (if (= n 0) f (derive (derive-n f (- n 1) dx) dx)))


;MAP FUNCTION LISTS
(define (map-l f l)
  (if (null? l) '()
      (cons (f (car l)) (map-l f (cdr l)))))

;APPEND FUNCTION
(define (append-l l1 l2)
  (if (null? l1) l2
    (cons (car l1) (append-l (cdr l1) l2))))

;REVERSE FUNCTION
(define (reverse-helper l acc)
  (if (null? l) acc
      (reverse-helper (cdr l) (cons (car l) acc))))

(define (reverse-l l)
  (reverse-helper l '()))

(define (is-member? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
         (else (is-member? x (cdr l)))))

