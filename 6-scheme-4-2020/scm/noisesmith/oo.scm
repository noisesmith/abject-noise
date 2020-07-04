(define-module (noisesmith oo)
               #:export (defmulti type)
               #:use-module ((ice-9 match)
                             #:select ()
                             #:renamer (symbol-prefix-proc 'match:))
               #:use-module ((oo goops)
                             #:select (class-of)))

(define (create-table)
  (make-hash-table))

(define (get-from-table t k)
  (hash-ref t k (lambda (. args)
                  (throw 'not-found "method not implemented"))))

(define (put-in-table t k v)
  (hash-set! t k v))

;; looks like this goops function might be my best bet here
(define type class-of)

;; TODO: this is almost it - it is close to clojure's defmulti, without the
;; binding part, and without the heirarchic lookup of dispatch built in
;; the inheriting dispatch can be a wrapper for the dispatch-lambda
;; would a separate version that only dispatches on the first arg's type be an optimization?
(define (multi . dispatch-lambda)
  (let ((dispatch-lambda (or (car dispatch-lambda)
                             type))
        (dispatch-table (create-table)))
    `(,(lambda (extension-symbol f) ; extend
         (put-in-table dispatch-table extension-symbol f))
      ,(lambda (. args) ; invoke
         (let* ((dispatch (apply dispatch-lambda args))
                (method (get-from-table dispatch-table dispatch)))
           (apply method args))))))
