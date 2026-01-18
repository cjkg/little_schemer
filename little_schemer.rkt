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

(define (rember x lat)
  (cond ((null? lat) '())
        ((eq? (car lat) x) (cdr lat))
        (else (cons (car lat)
                    (rember x (cdr lat))))))

(define (firsts l)
  (cond ((null? l) '())
        (else (cons (car (car l))
                    (firsts (cdr l))))))

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
