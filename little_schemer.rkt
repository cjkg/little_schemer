#lang racket

(define (atom? x)
    (and (not (pair? x))
         (not (null? x))))

(define (lat? x)
  (cond
    ((null? x) #t)
    ((atom? (car x)) (lat? (cdr x)))
    (else #f)))

(define (member? x lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) x)
              (member? x (cdr lat))))))

(define (rember s l)
  (cond ((null? l) '())
        ((equal? (car l) s) (cdr l))
        (else (cons (car l)
                    (rember s (cdr l))))))

(define (firsts l)
  (cond ((null? l) '())
        (else (cons (car (car l))
                    (firsts (cdr l))))))

(define (seconds l)
  (cond ((null? l) '())
        (else (cons (cadar l) (seconds (cdr l))))))

(define (insertR new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons (car lat)
                                   (cons new (cdr lat))))
        (else (cons (car lat)
                    (insertR new
                             old
                             (cdr lat))))))

(define (insertL new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat)
                    (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new
                                   (cdr lat)))
        (else (cons (car lat)
                    (subst new old (cdr lat))))))

(define (subst2 new old1 old2 lat)
  (cond ((null? lat) '())
        ((or (eq? (car lat) old1)
            (eq? (car lat) old2)) (cons new
                                    (cdr lat)))
        (else (cons (car lat)
                    (subst2 new old1 old2 (cdr lat))))))

(define (multirember a lat)
  (cond ((null? lat) '())
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (else (cons (car lat)
                    (multirember a (cdr lat))))))

(define (multiinsertR new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons (car lat)
                                   (cons new
                                         (multiinsertR new old (cdr lat)))))
        (else (cons (car lat)
                    (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new
                                   (cons (car lat)
                                         (multiinsertL new old (cdr lat)))))
        (else (cons (car lat)
                    (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new
                                   (multisubst new old (cdr lat))))
        (else (cons (car lat)
                    (multisubst new old (cdr lat))))))

(define (add x y)
  (cond ((zero? y) x)
        (else (add (add1 x) (sub1 y)))))

(define (subtract x y)
  (cond ((zero? y) x)
        (else (subtract (sub1 x) (sub1 y)))))

(define (addtup tup)
  (cond ((null? tup) 0)
        (else (+ (car tup)
                 (addtup (cdr tup))))))

(define (x n m)
  (cond ((zero? m) 0)
        (else (+ n (x n (sub1 m))))))

(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (+ (car tup1) (car tup2))
                    (tup+ (cdr tup1) (cdr tup2))))))

(define (gt n m)
  (cond ((zero? n) #f)
        ((zero? m) #t)
        (else (gt (sub1 n) (sub1 m)))))

(define (lt n m)
  (cond ((zero? m) #f)
        ((zero? n) #t)
        (else (lt (sub1 n) (sub1 m)))))

(define (eq n m)
  (cond ((and (zero? m) (zero? n)) #t)
        ((or (zero? m) (zero? n)) #f)
        (else (eq (sub1 n) (sub1 m)))))

(define (eq2 n m)
  (cond ((zero? m) (zero? n))
        ((zero? n) #f)
        (else (eq2 (sub1 n) (sub1 m)))))

(define (eq3 n m)
  (cond ((gt n m) #f)
        ((lt n m) #f)
        (else #t)))

(define (pow b x)
  (cond ((zero? x) 1)
        (else (* b (pow b (sub1 x))))))

(define (divide n m)
  (cond ((< n m) 0)
        (else (add1 (divide (- n m) m)))))

(define (len lat)
  (cond ((null? lat) 0)
        (else (add1 (len (cdr lat))))))

(define (pick n lat)
  (cond ((zero? (sub1 n)) (car lat))
        (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond ((zero? (sub1 n)) (cdr lat))
        (else (cons (car lat)
                    (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)) (no-nums (cdr lat)))
        (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond ((null? lat) '())
        ((not (number? (car lat))) (all-nums (cdr lat)))
        (else (cons (car lat)
                    (all-nums (cdr lat))))))

(define (all-nums2 lat)
  (cond ((null? lat) '())
        ((number? (car lat)) (cons (car lat)
                                   (all-nums (cdr lat))))
        (else (all-nums (cdr lat)))))

(define (eqan? a b)
  (cond ((and (number? a) (number? b)) (= a b))
        ((not (or (number? a) (number? b))) (eq? a b))
        (else #f)))

(define (eqan2? a b)
  (cond ((and (number? a) (number? b)) (= a b))
        ((or (number? a) (number? b)) #f)
        (else (eq? a b))))

(define (occur a lat)
  (cond ((null? lat) 0)
        ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat)))))

(define (one? n)
  (= n 1))

(define (rempick2 n lat)
  (cond ((one? n) (cdr lat))
        (else (cons (car lat) (rempick2 (sub1 n) (cdr lat))))))

(define (rember* a l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eqan? (car l) a) (rember* a (cdr l)))
               (else (cons (car l) (rember* a (cdr l))))))
        (else (cons (rember* a (car l)) (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eqan? (car l) old) (cons (car l)
                                        (cons new
                                              (insertR* new old (cdr l)))))
               (else (cons (car l)
                           (insertR* new old (cdr l))))))
        (else (cons (insertR* new old (car l))
                    (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond ((null? l) 0)
        ((atom? (car l))
         (cond ((eqan? (car l) a) (add1 (occur* a (cdr l))))
               (else (occur* a (cdr l)))))
        (else (+ (occur* a (car l)) (occur* a (cdr l))))))

(define (subst* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eqan? (car l) old) (cons new
                                          (subst* new old (cdr l))))
               (else (cons (car l) (subst* new old (cdr l))))))
        (else (cons (subst* new old (car l))
                    (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eqan? (car l) old) (cons new
                                          (cons (car l)
                                                (insertL* new old (cdr l)))))
               (else (cons (car l) (insertL* new old (cdr l))))))
         (else (cons (insertL* new old (car l))
                     (insertL* new old (cdr l))))))

(define (member* a l)
  (cond ((null? l) #f)
        ((atom? (car l))
         (or (eqan? (car l) a)
             (member* (cdr l))))
        (else (or (member* a (car l))
                  (member* a (cdr l))))))

(define (leftmost l)
  (cond ((atom? (car l)) (car l))
        (else (leftmost (car l)))))

(define (eqlist? l1 l2)
  (cond ((and (null? l1)
              (null? l2) #t))
        ((or (null? l1)
             (null? l2)) #f)
        (else
         (and (equal? (car l1) (car l2))
              (eqlist? (cdr l1) (cdr l2))))))

(define (equal? s1 s2)
  (cond ((and (atom? s1)
              (atom? s2)) (eqan? s1 s2))
        ((or (atom? s1) (atom? s2)) #f)
        (else (eqlist? s1 s2))))

(define (numbered? aexp)
  (cond ((atom? aexp) (number? aexp))
        (else
         (and (numbered? (car aexp))
              (numbered? (car (cdr (cdr aexp))))))))

(define (1st-sub-exp aexp)
  (car aexp))

(define (2nd-sub-exp aexp)
  (caddr aexp))

(define (operator aexp)
  (cadr aexp))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) (quote +))
     (+ (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) (quote *))
     (* (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))
    (else
     (pow (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp))))))

(define (pzero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (pluz m n)
  (cond ((null? m) n)
        (else (edd1 (pluz n (zub1 m))))))

(define (zet? lat)
  (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (zet? (cdr lat)))))

(define (makeset lat)
  (cond ((null? lat) '())
        ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
        (else (cons (car lat) (makeset (cdr lat))))))

(define (makeset2 lat)
  (cond ((null? lat) '())
        (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))

(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    ((member? (car set1) set2)
     (subset? (cdr set1) set2))
    (else #f)))

(define (subset2? set1 set2)
  (cond
    ((null? set1) #t)
    (else (and (member? (car set1) set2)
               (subset2? (cdr set1) set2)))))

(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(define (intersect? s1 s2)
  (cond ((null? s1) #f)
        ((member? (car s1) s2) #t)
        (else (intersect? (cdr s1) s2))))

(define (intersector? s1 s2)
  (cond ((null? s1) #f)
        ((or (member? (car s1) s2)
             (intersect? (cdr s1) s2)))))


(define (intersect s1 s2)
  (cond ((null? s1) '())
        ((member? (car s1) s2)
         (cons (car s1) (intersect (cdr s1) s2)))
        (else (intersect (cdr s1) s2))))

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((member? (car s1) s2)
         (union (cdr s1) s2))
        (else (cons (car s1) (union (cdr s1) s2)))))

(define (difference s1 s2)
  (cond ((null? s1) '())
        ((member? (car s1) s2)
         (difference (cdr s1) s2))
        (else (cons (car s1) (difference (cdr s1) s2)))))

(define (intersectall l-set)
  (cond ((null? (cdr l-set)) (car l-set))
        (else (intersect (car l-set)
                         (intersectall (cdr l-set))))))

(define (a-pair? p)
  (cond ((atom? p) #f)
        ((null? p) #f)
        ((null? (cdr p)) #f)
        ((null? (cdr (cdr p))) #t)
        (else #f)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build a b)
  (cons a (cons b '())))

(define (fun? rel)
  (zet? (firsts rel)))

(define (revrel1 rel)
  (cond
    ((null? rel) '())
    (else (cons (build (second (car rel)) (first (car rel)))
              (revrel1 (cdr rel))))))

(define (revpair p)
  (build (second p) (first p)))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (revpair (car rel))
                (revrel (cdr rel))))))  

(define (fullfun? rel)
  (and (fun? rel)
       (set? (seconds rel))))

(define (one-to-one? fun)
  (fun? (revrel fun)))

(define (rember-f test? a l)
  (cond ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    (rember-f test? a (cdr l))))))

(define (eq?-c a)
  (lambda (x)
    (eq? x a)))

(define (rember-f-lambda test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? (car l) a) (cdr l))
          (else (cons (car l)
                      ((rember-f-lambda test?)
                      a
                      (cdr l)))))))

(define rember-eq? (rember-f-lambda eq?))

(define (insertL-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons new l))
          (else (cons (car l)
                      ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons (car l) (cons new (cdr l))))
          (else (cons (car l)
                      ((insertR-f test?) new old (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old (cdr l))))

(define (seqR new old l)
  (cons old (cons new (cdr l))))

(define (insert-g test? seq)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (seq new old l))
          (else (cons (car l)
                      ((insert-g test? seq) new old (cdr l)))))))

(define insertR-g (insert-g equal? seqR))
(define insertL-g (insert-g equal? seqL))

(define insertR-g2 (insert-g equal?
                             (lambda (new old l)
                               (cons new (cons old (cdr l))))))

(define insertL-g2 (insert-g equal?
                             (lambda (new old l)
                               (cons old (cons new (cdr l))))))

(define (seqS new old l)
  (cons new (cdr l)))

(define subst-g (insert-g equal? seqS))

(define (multirember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? a (car l)) ((multirember-f test?) a (cdr l)))
          (else (cons (car l) ((multirember-f test?) a (cdr l)))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define (multiremberT test-c? lat)
    (cond
      ((null? lat) '())
      ((test-c? (car lat)) (multiremberT test-c? (cdr lat)))
      (else (cons (car lat) (multiremberT test-c? (cdr lat))))))

(define (a-friend x y)
  (null? y))

(define (new-friend newlat seen)
  (a-friend newlat
            (cons 'tuna seen)))
