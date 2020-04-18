(define-module (csound csound)
               #:use-module (oop goops)
               #:export (emit get-table))

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
