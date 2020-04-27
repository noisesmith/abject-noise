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
