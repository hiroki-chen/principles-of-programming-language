#lang racket

(define empty-env-ds
  (λ () `()))

(define extend-env-ds
  (λ (y val env) `((,y . ,val) . ,env)))

(define apply-env-ds
  (λ (env y)
    (match env
      [`() error "Unbound variable ~a" y]
      [`((,x . ,val) . ,other) (cond
                        ((eqv? y x) val)
                        (else (apply-env-ds other y)))])))

(define make-closure-ds
  (λ (x body)
    `(make-closure (lambda (,x) ,body))))

(define apply-closure-ds
  (λ (f arg env)
    (match f
      [`(make-closure (lambda (,x) ,body))
       (value-of-ds body (extend-env-ds x arg env))])))

(define value-of-ds
  (λ (e env)
    (match e
      (`,n #:when (number? n) n)
      (`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-ds nexpr1 env)
          (value-of-ds nexpr2 env)))
      (`(if-null ,l ,null-expr ,not-null-expr)
       (if (null? (value-of-ds l env))
           (value-of-ds null-expr env)
           (value-of-ds not-null-expr env)))
      (`(cons ,car-expr ,cdr-expr)
       (cons (value-of-ds car-expr env)
             (value-of-ds cdr-expr env)))
      (`(car ,l-expr)
       (car (value-of-ds l-expr env)))
      (`(cdr ,l-expr)
       (cdr (value-of-ds l-expr env)))
      (`empty '())
      (`,y #:when (symbol? y) (apply-env-ds env y))
      (`(let ([,x ,val]) ,body)
       #:when (symbol? x) (value-of-ds body (extend-env-ds x (value-of-ds val env) env)))
      (`(λ (,x) ,body)
       #:when (symbol? x)
       (make-closure-ds x body))
      (`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env) env)))))

(define empty-env
  (λ () `()))

(define extend-env
  (λ (env y val)
    `(extend-env ,env ,y ,val)))

(define apply-env
  (λ (env y)
    (match env
      [`() error "Unbound variable ~a" y]
      [`(extend-env ,con ,x ,val) (cond
                                    ((eqv? x y) val)
                                    (else (apply-env con y)))])))

(define make-closure
  (λ (x body env)
    `(make-closure (lambda (,x) ,body) ,env)))

(define apply-closure
  (λ (f arg)
    (match f
      [`(make-closure (lambda (,x) ,body ,env))
       (value-of body (extend-env x arg env))])))

(define value-of
  (λ (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                  (value-of conseq env)
                                  (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x)
       (set-box! (apply-env env x)
                 (value-of expr env))]
      [`(random ,n) (random (value-of n env))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))

(define value-of-cbv
  (λ (e env) `()))