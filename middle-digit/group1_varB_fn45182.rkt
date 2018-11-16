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


(define (square x) (* x x))

; Казваме, че дадено n-цифрено число е нарцистично, ако то съвпада със сумата от n-тите
; степени на цифрите си. Да се напише функция narcissistic?, която проверява дали дадено число е нарцистично.
; Пример: (narcissistic? 153) -> #t
;-----NARCISSISTIC------

(define (length n)
  (if (and (< n 10) (> n -10)) 1
      (+ 1 (length (quotient n 10)))))

(define (nth-pow n num nv)
  (if(= n 0) nv
     (nth-pow (- n 1) num (* nv num))))

(define (narcissistic-helper n current length)
  (if (< n 10) (+ current (nth-pow length n 1))
      (narcissistic-helper (quotient n 10) (+ current (nth-pow length (modulo n 10) 1)) length)))

(define (narcissistic? n)
  (if (= n (narcissistic-helper n 0 (length n))) #t #f))

; С d(x) означаваме сумата на делителите на естественото число x, които са различни от самото x.
; Две естествени числа a и b наричаме приятелски, ако d(a) = b и d(b) = a. Да се напише функция
; friendly?, която проверява дали две дадени числа са приятелски.
; Пример: (friendly? 220 284) -> #t
;------FRIENDLY------

(define (divisor-sum n)
  (define (helper a b sum)
  (cond [(>= a b) sum]
        [(= (modulo b a) 0) (helper (+ 1 a) b (+ a sum))]
        [else (helper (+ 1 a) b sum)]))
  (helper 1 n 0))

(define (friendly? a b)
  (if (and (= (divisor-sum a) b) (= (divisor-sum b) a)) #t #f))


; Отворен числов интервал (a; b) се описва с наредената двойка (a . b).
; Да се напише функция shortest-interval-subsets, която по даден списък от интервали il връща нов списък, който съдържа
; всички интервали от il, на които най-късият интервал в списъка е подинтервал.
;----------SHORTEST-INTERVAL-SUPERSETS------

(define (filter-int p? list shortest)
  (if (null? list) '()
      (if (p? shortest (car list)) (cons (car list) (filter-int p? (cdr list) shortest)) (filter-int p? (cdr list) shortest))))

(define (interval-length p)
  (if (and (pair? p) (< (car p) (cdr p))) (* (- (car p) (cdr p) ) -1) 0))

(define (shortest-interval li shortest int)
   (cond [(null? li) int]
         [(< (interval-length (car li)) shortest) (shortest-interval (cdr li) (interval-length (car li)) (car li))]
         [else (shortest-interval (cdr li) shortest int)]))

(define (sub-int? int1 int2)
  (if (and (pair? int1) (pair? int2) (>= (car int1) (car int2)) (<= (cdr int1) (cdr int2))) #t #f))

(define (shortest-interval-supersets il)
  (filter-int sub-int? il (shortest-interval il (interval-length (car il)) (car il))))

;----за сортировка---

(define (min-interval l min)
  (cond [(null? l) min]
        [(< (cdr (car l)) (cdr min)) (min-interval (cdr l) (car l))]
        [else (min-interval (cdr l) min)]))
