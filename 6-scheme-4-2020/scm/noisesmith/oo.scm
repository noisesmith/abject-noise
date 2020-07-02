(define-module (noisesmith oo)
               #:export (->thing)
               #:use-module ((ice-9 match)
                             #:select ()
                             #:renamer (symbol-prefix-proc 'match:)))

(define-syntax _thing_match
  (syntax-rules ()
    ((_thing_match m k v)
     (if (eq? m k)
         v))
    ((_thing_match m k v kvs ...)
     (if (eq? m k)
         v
         (_thing_match m kvs ...)))))

(define-syntax ->thing
  (syntax-rules ()
    ((->thing k v kvs ...)
     (lambda (. args)
       (_thing_match (car args)
                     k v kvs ...)))))

(define (create-table)
  ;;; ...
  )

(define (get-from-table t k)
  ;;; ...
  )

(define (put-in-table t k v)
  ;;; ...
  )

;; TODO: this is almost it - it is close to clojure's defmulti, without the
;; binding part, and without the heirarchic lookup of dispatch built in
;; the inheriting dispatch can be a wrapper for the dispatch-lambda
;; would a separate version that only dispatches on the first arg's type be an optimization?
(define (defmulti dispatch-lambda)
  (let ((dispatch-table (create-table)))
    (values (lambda (extension-symbol f) ; extend
              (put-in-table dispatch-table extension-symbol f))
            (lambda (. args) ; invoke
              (let* ((dispatch (apply dispatch-lambda args))
                     (method (get-from-table dispatch-table dispatch)))
                (apply method args))))))
