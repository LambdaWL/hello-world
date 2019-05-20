;; Recursions defined without explicitly calling itself

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
