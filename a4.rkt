#lang racket

; representation-independent version
(define value-of-ds
  (λ (e env)
    (match e
      ; literals
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(if ,c ,b1 ,b2) (cond
                          ((value-of-ds c env) (value-of-ds b1 env))
                          (else (value-of-ds b2 env)))]
      ; value binding
      [`(let ([,var ,e1]) ,e2) #:when (symbol? var)
                                   (value-of-ds e2 (extend-env-ds var (value-of-ds e1 env) env)) ]
      ; simple functions
      [`(zero? ,rand) (zero? (value-of-ds rand env))]
      [`(sub1 ,rand) (sub1 (value-of-ds rand env))]
      [`(* ,rand1 ,rand2) (* (value-of-ds rand1 env) (value-of-ds rand2 env))]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      ; make closure and apply it
      [`(lambda (,x) ,body) #:when (symbol? x) (make-closure-ds x body env)]
      ; application
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))]
      )))

(define empty-env-ds
  (λ () 'empty-env-ds))

(define extend-env-ds
  (λ (key value env)
    `((,key . ,value) . ,env)))

(define make-closure-ds
  (λ (x body env)
    `(make-closure (lambda (,x) ,body) ,env)))

(define apply-closure-ds
  (λ (f arg)
    (match f
      [`(make-closure (lambda (,x) ,body) ,env)
       (value-of-ds body (extend-env-ds x arg env))])))

(define apply-env-ds
  (λ (env y)
    (match env
      [`empty-env-ds (error 'value-of-ds "unbound variable ~s" y)]
      [`((,key . ,value) . ,env) (cond
                                   ((eqv? key y) value)
                                   (else (apply-env-ds env y)))]
      )))
