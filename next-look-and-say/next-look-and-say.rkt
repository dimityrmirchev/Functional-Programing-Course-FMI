#lang racket
;next-look-and-say
;Казваме, че списъкът x = (x1 x2 ... x2n) от цели числа се получава от прочитането
;(look-and-say) на списъка y, ако y се състои от последователно срещане на x1 пъти x2, последвано от x3 пъти x4,
;и така нататък до x2n-1 пъти x2n. Да се дефинира функция next-look-and-say,
;която по даден списък y намира списъка x, получен от прочитането y.

;Пример: (next-look-and-say ‘(1 1 2 3 3)) → ‘(2 1 1 2 2 3)

(define (help-look-n-say l current count)
  (cond ( (and (null? l) (not (= current 0))) (list count current) )
        ( (null? l) '() )
        ( (equal? (car l) current) (help-look-n-say (cdr l) current (+ 1 count)))
        (else (append (list count current) (help-look-n-say l (car l) 0))) ))

(define (next-look-and-say l)
  (help-look-n-say l (car l) 0))