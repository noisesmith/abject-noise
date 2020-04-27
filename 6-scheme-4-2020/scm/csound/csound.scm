(define-module (csound csound)
               #:use-module (oop goops)
               #:export (emit get-table))

;; consider moving this up a level
;; consider using (bytestructures guile) for interop:
;; /usr/share/guile/site/2.2/bytestructures/guile.scm

(define-generic emit)

(define-class
  <table-allocator> ()
  (counter
    #:init-value 1
    #:init-keyword #:counter
    #:getter counter))

(define-method
  (get-table (allocator <table-allocator>))
  (let ((n (counter allocator)))
    (cons n
          (make <table-allocator>
                #:counter (1+ n)))))

(define-method
  (get-table (n <number>))
  (get-table (make <table-allocator> #:counter n)))
