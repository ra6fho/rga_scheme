(define MaxInt 4294967087)

(define (ch-length ch acc)
  (cond ((null? ch) acc)
        (else (ch-length (cdr ch) (+ acc 1)))))

(define (random-double range-min range-max)
  (+ range-min (* (- range-max range-min) (/ (random MaxInt) MaxInt))))

(define (float-crossover ch1 ch2 out)
  (cond ((not (equal? (ch-length ch1 0) (ch-length ch2 0))) (display "Wrong chromosome size!!!"))
        ((or (null? ch1) (null? ch2)) out)
        (else (float-crossover (cdr ch1) (cdr ch2) (append out (list (random-double (car ch1) (car ch2))))))))

(define (arithmetical-crossover ch1 ch2 N out1 out2)
  (cond ((not (equal? (ch-length ch1 0) (ch-length ch2 0))) (display "Wrong chromosome size!!!"))
        ((or (< N 0) (> N 1)) (display "Wrong coefficient N!!!"))
        ((or (null? ch1) (null? ch2)) (list out1 out2))
        (else (arithmetical-crossover (cdr ch1) (cdr ch2) N (append out1 (list (+ (* N (car ch1)) (* (- 1 N) (car ch2))))) (append out2 (list (+ (* N (car ch2)) (* (- 1 N) (car ch1)))))))))