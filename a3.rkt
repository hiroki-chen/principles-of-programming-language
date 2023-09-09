#lang racket

(define idx
  (λ (s l)
    (cond
      ((null? l) #f)
      ((eqv? s (car l)) 0)
      (else (cond
              ((natural? (idx s (cdr l)))
               (add1 (idx s (cdr l))))
              (else #f))))))

(define lex
  (λ (e l)
    (match e
      [`,y #:when (symbol? y) (cond
                                ((natural? (idx y l))
                                 `(var ,(idx y l)))
                                (else y))]
      [`(lambda (,y) ,body) #:when (symbol? y)
                            `(lambda ,(lex body (cons y l)))]
      [`(,rator ,rand) `(,(lex rator l) ,(lex rand l))])))



(require racket/trace)
; representation-dependent version
(define value-of
  (λ (e env)
    (match e
      ; literals
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(if (,c) ,b1 ,b2) (cond
                          ((value-of c env) (value-of b1 env))
                          (else (value-of b2 env)))]
      ; simple functions
      [`(zero? ,rand1 ,rand2) (zero? (value-of rand1 env) (value-of rand2 env))]
      [`(sub1 ,rand1 ,rand2) (sub1 (value-of rand1 env) (value-of rand2 env))]
      [`(* ,rand1 ,rand2) (* (value-of rand1 env) (value-of rand2 env))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (λ (arg)
         ; We also need to extend the environment
         (value-of body
                 (λ (y)
                   (cond
                     ((eqv? y x) arg)
                     (else (env y))))))]
      [`,y #:when (symbol? y) (env y)]
      ; application
      [`(,rator ,rand) (value-of rator env) (value-of rand env)])))

(trace value-of)
(value-of
   '((lambda (x) (if (zero? x)
                     12
                     47))
     0)
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(define empty-env-fn
  (λ () '()))

(define extend-env-fn
  (λ (env s) '()))

(define apply-env-fn
  (λ () '()))

(define apply-clos-fn
  (λ () '()))

(define make-clos-fn
  (λ () '()))

; representation-independent version
(define value-of-fn
  (λ (e env err) e))
