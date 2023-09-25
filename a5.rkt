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

(define make-closure-cbv
  (λ (x body env)
    `(make-closure-cbv (lambda (,x) ,body) ,env)))

(define make-closure-cbr
  (λ (x body env)
    `(make-closure-cbr (lambda (,x) ,body) ,env)))

(define make-closure-cbname
  (λ (x body env)
    `(make-closure-cbname (lambda (,x) ,body) ,env)))

(define make-closure-cbneed
  (λ (x body env)
    `(make-closure-cbneed (lambda (,x) ,body) ,env)))


(define apply-closure
  (λ (f arg)
    (match f
      [`(make-closure-cbv (lambda (,x) ,body) ,env)
       (val-of-cbv body (extend-env env x arg))]
      [`(make-closure-cbr (lambda (,x) ,body) ,env)
       (val-of-cbr body (extend-env env x arg))]
      [`(make-closure-cbname (lambda (,x) ,body) ,env)
       (val-of-cbname body (extend-env env x arg))]
      [`(make-closure-cbneed (lambda (,x) ,body) ,env)
       (val-of-cbneed body (extend-env env x arg))]
      )))

(define val-of-cbv
  (λ (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x)
       (set-box! (apply-env env x)
                 (val-of-cbv expr env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

(define val-of-cbr
  (λ (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(set! ,x ,expr) #:when (symbol? x)
       (set-box! (apply-env env x)
                 (val-of-cbr expr env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbr rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))

(define val-of-cbname
  (λ (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbname rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (λ () (val-of-cbname rand env))))])))


(define val-of-cbneed
  (λ (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`,y #:when (symbol? y) (let [(sym (apply-env env y))]
                                (let [(v ((unbox sym)))]
                                  (begin
                                    (set-box! sym (λ () v))
                                    v)))]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbneed rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (λ () (val-of-cbneed rand env))))])))

