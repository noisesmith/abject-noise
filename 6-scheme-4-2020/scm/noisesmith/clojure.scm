(define-module (noisesmith clojure)
               #:export (~> ~>> assj comp conj disj empty? get get-in ht keys
                            part seq update-in vals))

(use-modules
  (ice-9 vlist)
  (oop goops)
  (srfi srfi-1)
  (srfi srfi-26))

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

(define-method
  (empty? (l <list>))
  (equal? l '()))

;;; lists
(define (part n l)
  (letrec ((p (lambda (acc l)
                (cond ((empty? l)
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
  (empty? (h <ht>))
  (empty? (vlist->list (vh h))))

(define-method
  (keys (ht <ht>))
  (map car (vlist->list (vh ht))))

(define-method
  (vals (ht <ht>))
  (map cdr (vlist->list (vh ht))))

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

(define-method
  (disj (source <ht>) . ks)
  (make <ht>
        #:vh (apply disj (vh source) ks)))

(define-method
  (disj (source <top>) . ks)
  (if (empty? ks)
    source
    (apply disj
           (vhash-delete (car ks) source)
           (cdr ks))))

(define (ht-assj-helper ht kvs)
  (fold (lambda (kv hash)
            (if (empty? kv)
              hash
              (~>> hash
                   (vhash-delete (car kv))
                   (vhash-cons (car kv) (cadr kv)))))
          ht
          kvs))

(define-method
  (assj (ht <ht>) . kvs)
  (make <ht>
        #:vh
        (ht-assj-helper (vh ht) (part 2 kvs))))

(define-method
  ;; so that (assj #f #:k "v") works, like clojure nil punning
  (assj (b <boolean>) . kvs)
  (apply assj (ht) kvs))

(define (ht . args)
  (apply assj (make <ht>) args))

(define-method
  (get (_ <top>) k not-found-f)
  (not-found-f _))

(define-method
  (get (ht <ht>) k not-found-f)
  (let ((found (vhash-assoc k (vh ht))))
    (if found
      (cdr found)
      (not-found-f ht))))

(define-method
  (get x k)
  (get x k (lambda (_) #f)))

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
    (if (not (empty? pairs))
      (begin
        (write-pair (car pairs))
        (map (lambda (pair)
               (display " " port)
               (write-pair pair))
             (cdr pairs)))))
  (display ")" port))

(read-hash-extend
  #\h
  (lambda (chr port)
    (let ((payload (read port)))
      `(ht ,@payload))))

(define-method
  (get-in m ks)
  (if (empty? ks)
    m
    (get-in (get m (car ks)) (cdr ks))))

(define-method
  (select-keys (source <ht>) (selected <list>))
  (fold (lambda (k h)
          (assj h (get source k)))
        (ht)
        selected))

(define-method
  (update-in m ks f)
  (if (empty? ks)
    (f m)
    (assj m (car ks)
          (update-in (get m (car ks)) (cdr ks) f))))

(define (comp . fs)
  (reduce (lambda (f g)
            (lambda (x) (g (f x))))
          identity
          fs))

(define-method (seq (ht <ht>))
  (vlist->list (vh ht)))
