#lang racket

; Да се напише функция (meetTwice? f g a b), която проверява дали в целочисления интервал [a; b]
; съществуват две различни цели числа x и y такива, че f(x) = g(x) и f(y) = g(y).
; Пример:(meetTwice? (lambda(x)x) (lambda(x) (- x)) -3 1) → #f
; Пример:(meetTwice? (lambda(x)x) sqrt 0 5) → #t

(define (filter-acc p? op nv a b term next)
  (cond [(> a b) nv]
        [(p? a) (op (term a) (filter-acc p? op nv (next a) b term next))]
        [else (filter-acc p? op nv (next a) b term next)]))

(define (meetTwice? f g a b)
  ( if (> (filter-acc (lambda (x) (= (f x) (g x))) + 0 a b
              (lambda (x) 1)
              (lambda (x) (+ 1 x))) 1) #t #f))