#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs)  (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([t (s)])
        (cons (car t) (stream-for-n-steps (cdr t) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons (if x "dan.jpg" "dog.jpg") (lambda () (f (not x)))))])
    (lambda () (f #t))))

(define (stream-add-zero s)
  (lambda ()
    (let ([v (s)])
      (cons (cons 0 (car v)) (stream-add-zero (cdr v))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (i j)
                (let ([x (list-nth-mod xs i)]
                      [y (list-nth-mod ys j)])
                  (cons (cons x y) (lambda () (f (+ i 1) (+ j 1))))))])
    (lambda () (f 0 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (pos) (if (= pos len)
                              #f
                              (let ([e (vector-ref vec pos)])
                                (if (and (cons? e) (equal? (car e) v))
                                    e
                                    (f (+ pos 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [pos 0])
    (lambda (v) (let ([cached-value (vector-assoc v cache)])
                  (if cached-value
                      cached-value
                      (let ([value (assoc v xs)])
                        (if value
                            (begin (vector-set! cache pos value)
                                   (set! pos (if (= pos (- n 1)) 0 (+ pos 1)))
                                   value)
                            #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([r1 e1]
              [f (lambda () (if (>= e2 r1) #t (f)))])
       (f))]))
