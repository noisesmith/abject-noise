(define-module (noisesmith debug)
               #:export (->catalog))

(use-modules
  (ice-9 match)
  (srfi srfi-1)
  (srfi srfi-111))

(define (->catalog)
  (let ((store (box '())))
    (match-lambda*
      (()
       (unbox store))
      ((#:clear)
       (set-box! store '()))
      ((#:pop)
       (let ((s (unbox store)))
         (set-box! store (cdr s))
         (car s)))
      ((#:peek)
       (set-box! store (list (car (unbox store)))))
      ((#:box)
       store)
      ((tag-to-search)
       (filter (lambda
                 (entry)
                 (equal? (car entry) tag-to-search))
               (unbox store)))
      ((tag-to-add datum)
       (set-box! store
                 (cons (list tag-to-add datum (make-stack #t) (current-time))
                       (unbox store)))
       datum))))

; (define (inspect-catalog-entry entry . args)
;   (let* ((stack (list-ref entry 2))
;          (stacks-count (stack-length stack)))
;     (if (equal? args '())
;         (begin (display (string-append (number->string stacks-count) " frames\n"))
;                (display-backtrace stack (current-output-port))
;                stack)
;         (let ((command (car args))
;               (spec (cdr args)))
;           (cond ((eq? command #:t)
;                  stack)))))
;   ;; stack-ref, display-backtrace etc.
;   ;; https://www.gnu.org/software/guile/docs/docs-1.8/guile-ref/Examining-the-Stack.html#Examining-the-Stack
;   ;; stack-ref, frame-arguments, frame-source, frame-procedure etc.
;   ;; https://www.gnu.org/software/guile/docs/docs-1.8/guile-ref/Examining-Stack-Frames.html#Examining-Stack-Frames
;   ;; addendum for frame-source:
;   ;; https://www.gnu.org/software/guile/docs/docs-1.8/guile-ref/Decoding-Memoized-Source-Expressions.html#Decoding-Memoized-Source-Expressions
;   ;; also - registers?
;   )
; 
; (define (inspect-catalog catalog query . args)
;   (if (equal? args '())
;       (catalog query)
;       (apply inspect-catalog-entry (list-ref (catalog query) (car args)) (cdr args))))
