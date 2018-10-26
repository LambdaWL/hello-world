;; To return all the possible arrangements of the elements in the given list

(define (permutations lst)
  (cond
    [(empty? lst) (list empty)]
    [else
     (arrange-one-to-many (first lst)
                          (permutations (rest lst)))]))

(define (arrange-one-to-many n lst-of-lst)
  (cond
    [(empty? lst-of-lst) empty]
    [else
     (append (arrange-one-to-one n (first lst-of-lst))
             (arrange-one-to-many n (rest lst-of-lst)))]))

(define (arrange-one-to-one n lst)
  (cond
    [(empty? lst) (cons (cons n empty) empty)]
    [else
     (cons (cons n lst)
           (add-to-beginning (first lst)
                             (arrange-one-to-one n (rest lst))))]))

(define (add-to-beginning n lst-of-lst)
  (cond
    [(empty? lst-of-lst) empty]
    [else
     (cons (cons n (first lst-of-lst))
           (add-to-beginning n (rest lst-of-lst)))]))
           
