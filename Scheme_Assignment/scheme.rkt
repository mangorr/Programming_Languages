;; Q1
;; (byTwos n m) returns the list of every other integer starting with n up to m.
;; Base Case: if n > m, the result is the empty list.
;; Hypothesis: Assume (byTwos (+ n 2) m) returns the list of every other integer
;; from n+2 up to m.
;; Recursive step: (byTwos n m) returns (cons n (byTwos (+ n 2) m))
(define (byTwos n m)
  (cond
    ((> n m) '())
    (else (cons n (byTwos (+ n 2) m)))))

;; Q2
;; (compress L) returns a list of all the atoms (non-list values) contained
;; in L or in any nested list within L.
;; Base Case: if L is null, return '()
;; Hypothesis: Assume (append (compress (car L)) (compress (cdr L))) returns
;; the list of compressed L
;; Recursive step: (compress L) returns (append (compress (car L)) (compress (cdr L)))
(define (compress L)
  (cond
    ((null? L) '())
    ((not (list? L)) (list L))
    (else (append (compress (car L)) (compress (cdr L))))))

;; Q3
;; (rev-all L) reverses the elements of a list L and,
;; if the L contains nested lists, reverses those nested lists as well
;; Base case: if old is null, return new
;; Hypothesis: Assume 
;; Recursive step: (1)If (car old) is a list, (rev (cdr old) (cons(rev (car old) '()) new))
;;                 (2)If not, (rev (cdr old) (cons (car old) new))

(define (rev old new)
  (cond
    ((null? old) new)
    ((list? (car old)) (rev (cdr old) (cons(rev (car old) '()) new)))
    (else (rev (cdr old) (cons (car old) new)))))

(define (rev-all L) (rev L '()))


;; Q4
;; (equalTo? x y) works the same as (equal? x y), return wether two lists or atoms are
;; equal or not
;; Base case: if one of the parameters is null while the other is not empty, return false
;; Hypothesis: Assume (equalTo? (cdr x) (cdr y) returns wehter tow parameters are equal
;; or not after knowing that the former parts are equal
;; Recursive step: ((and (list? x) (list? y)) (if(eq? (car x) (car y)) (if(and (null? (cdr x))
;; (null? (cdr y))) (eq? (car x) (car y)) (equalTo? (cdr x) (cdr y))) (eq? (car x) (car y))))
(define (equalTo? x y)
  (cond
    ((or (and (null? x) (not(null? y))) (and (null? y) (not(null? x)))) #f)
    ((and (list? x) (list? y)) (if(eq? (car x) (car y)) (if(and (null? (cdr x)) (null? (cdr y))) (eq? (car x) (car y)) (equalTo? (cdr x) (cdr y))) (eq? (car x) (car y))))
    (else (eq? x y))))


;; Q5
;; (equalFns? fn1 fn2 domain) returns true if fn1 and fn2 always returns the
;; same value when applied to the same element of domain
(define (equalFns? fn1 fn2 domain)
  (if(equal? (map fn1 domain) (map fn2 domain)) #t #f))


;; Q6
;; (same-vals fn1 fn2 domain) returns the list of all elements x of
;; domain such that (fn1 x) and (fn2 x) return the same value
;; Base case: if domain is null, return '()
;; Hypothesis: Assume (cons (car (map fn2 domain)) (same-vals fn1 fn2 (cdr domain))
;; returns required elements in the remaning lists
;; Recursive step: ((equal? (car (map fn1 domain)) (car (map fn2 domain)))
;; (cons (car (map fn2 domain)) (same-vals fn1 fn2 (cdr domain))))
(define (same-vals fn1 fn2 domain)
  (cond
    ((null? domain) '())
    ((equal? (car (map fn1 domain)) (car (map fn2 domain))) (cons (car (map fn2 domain)) (same-vals fn1 fn2 (cdr domain))))
    (else (same-vals fn1 fn2 (cdr domain)))))


;; Q7
;; (split x L) that returns a list containing two lists:
;; The first list contains the numbers in L less than or equal to x and
;; the second list contains the numbers in L greater than x
;; Base case: if L is null, return '(()())
;; Hypothesis: Assume (1)When (>= x (car L)), the element should be add to (car result),
;;                       represented as : (cons (car L) (car (split x (cdr L))))
;;                    (2)When (< x (car L)), the element should be add to (cadr result),
;;                       represented as : (cons (car L) (cadr (split x (cdr L))))
;; Recursive step: (1)When (>= x (car L)), (list (cons (car L) (car (split x (cdr L))))
;;                                               (cadr (split x (cdr L))))
;;                 (2)When (< x (car L)), (list (car (split x (cdr L)))
;;                                              (cons (car L) (cadr (split x (cdr L)))))
(define (split x L)
  (cond
    ((null? L) '(()()))
    ((>= x (car L)) (list (cons (car L) (car (split x (cdr L)))) (cadr (split x (cdr L)))))
    ((< x (car L)) (list (car (split x (cdr L))) (cons (car L) (cadr (split x (cdr L))))))))


;; Q8
;; (psort L) returns ordered list by QuickSort.
;; Base case: if L is null, return '()
;; Hypothesis: Assume (psort (car x)) returns the ordered list of (car (split x l)) and
;; (psort (cadr x)) returns the ordered list of (cadr (split x l)).
;; Recursive step: (let ((x (split (car L) (cdr L))))
;;                (append (psort (car x)) (cons (car L) (psort (cadr x)))))
;; ps: (car L) is the pivot and should be cons to the position between (car (split x l))
;; and (cadr (split x l))

(define (psort L)
  (cond
    ((null? L) '())
    ((null? (cdr L)) (list (car L)))
    (else (let ((x (split (car L) (cdr L))))
               (append (psort (car x)) (cons (car L) (psort (cadr x))))))))


;; Q9
;; (applyToList f) returns a function that takes a list L as a parameter and
;; applies f to every element of L,returning the resulting list as the result.
(define (applyToList f)
  (lambda (L) (map f L)))

;; Q10
;; (newApplyToList f) returns a function that takes a list L as a parameter and
;; applies f to every element of L,returning the resulting list as the result.
(define (newApplyToList f)
  (lambda (L)
    (letrec(
            (myMap(lambda (fun L_)
                    (cond
                    ((null? L_) '())
                    (else (cons (fun (car L_)) (myMap fun (cdr L_))))))))
      (myMap f L))))
