;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recursion-abstract) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ?? Lazy-function/Y-combinator ??

(define collatz
  ((lambda (f) (f f))    
   (lambda (r)
     (lambda (n)
       (cond
         [(even? n) (cons n ((r r) (/ n 2)))]
         [else (cons n ((r r) (add1 (* n 3))))])))))

(define my-reverse
  (lambda (list)
    (((lambda (f) (f f))
      (lambda (r)
        (lambda (l acc)
          (cond [(empty? l) acc]
                [else ((r r) (rest l) (cons (first l) acc))])))) list empty)))

(define my-build-list
  (lambda (len f)
    (((lambda (f) (f f))
      (lambda (r)
        (lambda (i)
          (cond [(= i len) empty]
                [else (cons (f i) ((r r) (add1 i)))])))) 0)))

(define my-foldr
  (lambda (f base list)
    (((lambda (f) (f f))
      (lambda (r)
        (lambda (l)
          (cond [(empty? l) base]
                [else (f (first l) ((r r) (rest l)))])))) list)))

(define my-foldl
  (lambda (f base list)
    (((lambda (f) (f f))
      (lambda (r)
        (lambda (l rr)
          (cond [(empty? l) rr]
                [else ((r r) (rest l) (f (first l) rr))])))) list base)))

(define my-map
  (lambda (f list)
    (((lambda (f) (f f))
      (lambda (r)
        (lambda (l)
          (cond [(empty? l) empty]
                [else (cons (f (first l)) ((r r) (rest l)))])))) list)))

(define fibonacci
  (lambda (n)
    (cond [(<= n 1) n]
          [else
           (((lambda (f) (f f))
             (lambda (r)
               (lambda (i p1 p2)
                 (cond [(= i n) (+ p1 p2)]
                       [else ((r r) (add1 i) (+ p1 p2) p1)])))) 2 1 0)])))


;; !! The Lazy Implementation !!
;; it crushes...

;; (define Y
;;   (λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))))
;; (define Fact
;;   (Y (λ (fact) (λ (n) (if (zero? n) 1 (* n (fact (- n 1))))))))

;; (define Fib
;;   (Y (λ (fib) (λ (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))))


       
       
   
