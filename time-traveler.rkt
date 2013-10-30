#lang racket

;; The following is meant as an exploratory exercise in macros
;; and should be viewed (and/or) used as such.

(define-syntax (time-travel-init stx)
  (syntax-case stx ()
    [(func)
     (with-syntax ([hash-name (datum->syntax #'func '*time-travel-hash*)])
       #'(define hash-name (make-hash)))]))

(define-syntax (time-set! stx)
  (syntax-case stx ()
    [(func object value)
     (with-syntax ([hash-name (datum->syntax #'func '*time-travel-hash*)]
                   [object-name (symbol->string (syntax-e #'object))])
       #'(begin
           (hash-set! hash-name
                      object-name
                      (cons object
                            (hash-ref hash-name object-name '())))
           (set! object value)))]
    
    [(func object value label)
     (let ([objstring (symbol->string (syntax-e #'object))]
           [label (symbol->string (syntax-e #'label))])
       (with-syntax ([hash-name (datum->syntax #'func '*time-travel-hash*)]
                     [object+label (format "~a//~a" objstring label)])
         #'(begin
             (hash-set! hash-name object+label value)
             (set! object value))))]))

(define-syntax (time-rewind stx)
  (syntax-case stx ()
    [(func object steps)
     (with-syntax ([hash-name (datum->syntax #'func '*time-travel-hash*)]
                   [object-name (symbol->string (syntax-e #'object))])
       (cond
        [(number? (syntax-e #'steps))
         #'(let ([time-list (hash-ref hash-name object-name)])
             (cond
              [(< steps 1) object]
              [(<= steps (length time-list)) (list-ref time-list (sub1 steps))]
              [else (last time-list)]))]
        
        [(symbol? (syntax-e #'steps))
         (let ([objstring (symbol->string (syntax-e #'object))]
               [label (symbol->string (syntax-e #'steps))])
           (with-syntax ([object+label (format "~a//~a" objstring label)])
             #'(hash-ref hash-name object+label)))]))]
    
    [(func object)
     (with-syntax ([hash-name (datum->syntax #'func '*time-travel-hash*)]
                   [object-name (symbol->string (syntax-e #'object))])
       #'(last (hash-ref hash-name object-name)))]))

(module+ main  
  (time-travel-init) ; Define time travel hash
  
  (define time-test 42)

  (time-set! time-test 1337)
  (time-set! time-test 0)
  (printf "~a~n" *time-travel-hash*)
  (time-rewind time-test 2) ; => 42
  (time-rewind time-test 1) ; => 1337
  (time-rewind time-test 0) ; => 0
  (time-rewind time-test) ; => 42

  (time-set! time-test 322 solo)
  time-test ; => 322
  (printf "~a~n" *time-travel-hash*)
  (time-rewind time-test solo) ; => 322
  )