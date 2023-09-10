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


(define extend-env
  (λ (x arg env)
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else(env y))))))

; the support for `set!` and `begin2`.
;; TODO!!!
(define value-of-ext
  (λ (e env) e))

(require racket/trace)
; representation-dependent version
(define value-of
  (λ (e env)
    (match e
      ; literals
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(if (zero? ,rand) ,b1 ,b2) (cond
                          ((zero? (value-of rand env)) (value-of b1 env))
                          (else (value-of b2 env)))]
      ; value binding
      [`(let ([,var ,e1]) ,e2) #:when (symbol? var)
                                   (value-of e2 (extend-env var (value-of e1 env) env)) ]
      ; simple functions
      [`(sub1 ,rand) (sub1 (value-of rand env))]
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
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))]
      [_ (value-of-ext e env)]
      )))

(define empty-env-fn
  (λ () `empty-env-fn))

(define extend-env-fn
  (λ (key value env) `(extend-env-fn ,key ,value ,env)))

(define apply-env-fn
  (λ (env y)
    (match env
      [`(empty-env-fn) (error 'value-of "unbound variable ~s" y)]
      [`(extend-env-fn ,key ,value ,env) (cond
                                           ((eqv? key y) value)
                                           (else (apply-env-fn env y)))])))

(define make-clos-fn
  (λ (x body env)
   `(make-clos-fn ,x ,body ,env)))

(define apply-clos-fn
  (λ (f arg)
    (match f
    [`(make-clos-fn ,x ,body ,env) (value-of-fn body (extend-env-fn x arg env))])))

; representation-independent version
(define value-of-fn
  (λ (e env)
    (match e
      ; literals
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(if (,c ,rand) ,b1 ,b2) (cond
                          ((c (value-of rand env)) (value-of b1 env))
                          (else (value-of b2 env)))]
      ; value binding
      [`(let ([,var ,e1]) ,e2) #:when (symbol? var)
                                   (value-of e2 (extend-env var (value-of e1 env) env)) ]
      ; simple functions
      [`(sub1 ,rand) (sub1 (value-of rand env))]
      [`(* ,rand1 ,rand2) (* (value-of rand1 env) (value-of rand2 env))]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      ; make closure and apply it
      [`(lambda (,x) ,body) #:when (symbol? x) (make-clos-fn x body env)]
      ; application
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))]
      )))
