(define-module (noisesmith clojure)
               #:export (~> ~>> ht conj get keys get-in update-in part))

(use-modules
  (ice-9 vlist)
  (oop goops)
  (srfi srfi-1))

;;; threading
(define-syntax ~>
  (syntax-rules
    ()
    ((_)
     #f)
    ((_ x)
     x)
    ((_ x (f . (f-rest ...)))
     (f x f-rest ...))
    ((_ x f)
     (f x))
    ((_ x (f . (f-rest ...)) rest ...)
     (~> (f x f-rest ...) rest ...))
    ((_ x f rest ...)
     (~> (f x) rest ...))))

(define-syntax ~>>
  (syntax-rules
    ()
    ((_)
     #f)
    ((_ x)
     x)
    ((_ x (f ...))
     (f ... x))
    ((_ x f)
     (f x))
    ((_ x (f ...) rest ...)
     (~>> (f ... x) rest ...))
    ((_ x f rest ...)
     (~>> (f x) rest ...))))


;;; lists
(define (part n l)
  (letrec ((p (lambda (acc l)
                (cond ((eq? l '())
                       (list (reverse acc)))
                      ((= (count (lambda (_) #t) acc) n)
                       (cons (reverse acc)
                             (p (list (car l)) (cdr l))))
                      ((p (cons (car l) acc) (cdr l)))))))
    (p '() l)))

;;; hash tables
(define-class
  <ht> ()
  (vhash
    #:init-keyword #:vh
    #:init-form (alist->vhash '())
    #:getter vh))

(define-method
  (keys (ht <ht>))
  (map car (vlist->list (vh ht))))

(define-method
  (equal? (a <ht>) (b <ht>))
  (let* ((max-hash-arg (1- (expt 2 64)))
         (dumb-compare (lambda (x y)
                         (< (hash x max-hash-arg)
                            (hash y max-hash-arg))))
         (ka (sort (keys a) dumb-compare))
         (kb (sort (keys b) dumb-compare)))
    (and (equal? ka kb)
         (reduce (lambda (t? k)
                   (and t?
                        (equal? (get a k) (get b k))))
                 #t
                 ka))))

(define (ht-conj-helper ht kvs)
  (reduce (lambda (kv hash)
            (if (eq? kv '())
              hash
              (vhash-cons (car kv) (cadr kv) hash)))
          #f
          (cons ht kvs)))

(define-method
  (conj (ht <ht>) . kvs)
  (make <ht>
        #:vh
        (ht-conj-helper (vh ht) (part 2 kvs))))

(define (ht . args)
  (apply conj (make <ht>) args))

(define-method
  (get (ht <ht>) k not-found-f)
  (let ((found (vhash-assoc k (vh ht))))
    (if found
      (cdr found)
      (not-found-f ht))))

(define-method
  (get (ht <ht>) k)
  (get ht k (lambda (_) #f)))

(define-method
  (display (ht <ht>) port)
  (display "hash:" port)
  (map (lambda (pair)
         (display "[" port)
         (display (car pair) port)
         (display "=" port)
         (display (cdr pair) port)
         (display "]" port))
       (vlist->list (vh ht)))
  (newline port))

(define-method
  (write (ht <ht>) port)
  (display "#h(" port)
  (let ((pairs (vlist->list (vh ht)))
        (write-pair (lambda (p)
                      (write (car p) port)
                      (display " " port)
                      (write (cdr p) port))))
    (write-pair (car pairs))
    (map (lambda (pair)
           (display " " port)
           (write-pair pair))
         (cdr pairs)))
  (display ")"))

(read-hash-extend
  #\h
  (lambda (chr port)
    (let ((payload (read port)))
      `(ht ,@payload))))
