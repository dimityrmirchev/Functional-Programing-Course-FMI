;Да се напише функция middle-digit, която намира средната цифра от записа на подадено естествено число n.
;Ако n е с четен брой цифри, функцията връща -1.
;Пример: (middle-digit 452) → 5
;Пример: (middle-digit 4712) → -1

#lang racket

(define (1+ x) (+ x 1))

(define (length n)
  (cond ((and(< n 10) (> n -10)) 1)
        (else (+ (length (quotient n 10)) 1))))

(define (nth-digit n i)
  (define (helper n i j)
    (cond((= i j) (remainder n 10))
         ((< j i) (helper (quotient n 10) i (1+ j)))
         (else -1)))
  (helper n i 1))

(define (middle-digit n)
  (cond ((even? (length n)) -1)
      (else (nth-digit n (/ (1+ (length n)) 2)))))
