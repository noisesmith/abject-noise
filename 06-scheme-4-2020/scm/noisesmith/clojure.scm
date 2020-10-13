(define-module (noisesmith clojure)
               #:export (-> ->> assj comp conj constantly disj empty? get get-in
                            hmerge ht keys keyword key-list name part reduce-kv seq
                            update-in vals))

(use-modules
  (ice-9 vlist)
  (noisesmith debug)
  (oop goops)
  (srfi srfi-1)
  (srfi srfi-26))

(define debug (->catalog))

(define (constantly x)
  (lambda (. _)
    x))

;;; threading
(define-syntax ->
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
     (-> (f x f-rest ...) rest ...))
    ((_ x f rest ...)
     (-> (f x) rest ...))))

(define-syntax ->>
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
     (->> (f ... x) rest ...))
    ((_ x f rest ...)
     (->> (f x) rest ...))))

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
    #:getter _vh))

(define (vh ht)
  (or (debug #:accessor (_vh ht))
      (alist->vhash '())))

(define-method
  (empty? (h <ht>))
  (empty? (vlist->list (vh h))))

(define-method
  (keys (ht <ht>))
  (map car (vlist->list (vh ht))))

(define-method
  (vals (ht <ht>))
  (map cdr (vlist->list (vh ht))))

(define (ht-assj-helper ht kvs)
  (fold (lambda (kv hash)
            (if (empty? kv)
              hash
              (->> hash
                   (vhash-delete (car kv))
                   (vhash-cons (car kv) (cadr kv)))))
          ht
          kvs))

(define-method
  (assj (ht <ht>) . kvs)
  (make <ht>
        #:vh
        (ht-assj-helper (vh ht) (part 2 kvs))))


(define (update-in m ks f . args)
  (if (empty? ks)
    (apply f m args)
    (assj m (car ks)
          (apply update-in (get m (car ks)) (cdr ks) f args))))

(define-method (seq (ht <ht>))
  (vlist->list (vh ht)))

(define-method
  (equal? (a <ht>) (b <ht>))
  (let ((paired-off
          (fold (lambda (kv m)
                    (update-in m (list kv)
                               (lambda (x)
                                 (if (not x)
                                     1
                                     (+ 1 x)))))
                  (make <ht>)
                  (append (seq a)
                          (seq b)))))
    (every (lambda (x) (= x 2))
           (vals paired-off))))


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

(define (comp . fs)
  (reduce (lambda (f g)
            (lambda (x) (g (f x))))
          identity
          fs))

(define-method
  (seq (l <list>))
  l)

(define-method
  (seq (b <boolean>))
  '())

(define-method
  (reduce-kv f init s)
  (fold (lambda (kv acc)
          (f acc (car kv) (cdr kv)))
        init
        (seq s)))

(define-method
  (reduce-kv f s)
  (reduce-kv f (ht) s))

(define (hmerge h1 h2)
  (reduce-kv assj h1 h2))

(define-method
  (name (k <keyword>))
  (name (keyword->symbol k)))

(define-method
  (name (s <symbol>))
  (symbol->string s))

(define-method
  (name (s <string>))
  s)

(define (keyword . xs)
  ((comp symbol->keyword string->symbol)
   (apply string-append (map name xs))))

(define (key-list l)
  "not strictly copied from clojure, but often needed at the edges
  of scheme and clojure code"
  (if (eq? l '())
      '()
      (cons (car l)
            (key-list (cddr l)))))
