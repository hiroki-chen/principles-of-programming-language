#lang racket

; Complete the following definition of last-non-zero, a function which takes a list of numbers and return the last cdr whose car is 0. In other words, when starting from the right of the list, it should be all numbers before the first 0 is reached. See the test cases below and student test file for examples. Your solution should be naturally recursive, and should not contain any calls to member-like operations, nor should you be reversing the list.
(define last-non-zero
  (λ (ls)
    (let/cc k ; k captures nothing, meaning that this continuation simply
      ; "pitchs away" previous context.
      (letrec
          ((last-non-zero
            (λ (ls)
              (cond
                ((null? ls) '())
                ((eqv? (car ls) 0) (k (last-non-zero (cdr ls))))
                (else (cons (car ls) (last-non-zero (cdr ls))))
                ))))

        (last-non-zero ls)))))

(define index
  (λ (var ls)
    (match ls
      ['() (error "Not found")]
      [`(,a . ,_) #:when (eqv? a var)
                  0]
      [`(,_ . ,d)
       (add1 (index var d))])))

(define lex
  (λ (e ls)
    (match e
      [`,y #:when (symbol? y)
           `(var ,(index y ls))]
      [`,y #:when (number? y)
           `(const ,y)]
      [`(lambda (,x) ,body) #:when (symbol? x)
                            `(lambda ,(lex body (cons x ls)))]
      [`(zero? ,nexp) `(zero ,(lex nexp ls))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 ls) ,(lex nexp2 ls))]
      [`(catch ,cname ,cexp) `(catch ,(lex cexp (cons cname ls)))]
      [`(pitch ,exp1 ,cexp) `(pitch ,(lex exp1 ls) ,(lex cexp ls))]
      [`(,rator ,rand)
       `(app ,(lex rator ls) ,(lex rand ls))])))

(define cont-sub1
  (λ (k^)
    `(cont-sub1 ,k^)))

(define cont-zero
  (λ (k^)
    `(cont-zero ,k^)))

(define cont-if
  (λ (conseq^ alt^ env^ k^)
    `(cont-if ,conseq^ ,alt^ ,env^ ,k^)))

(define cont-pitch
  (λ (v-exp env^)
    `(cont-pitch ,v-exp ,env^)))

(define cont-let
  (λ (body^ env^ k^)
    `(cont-let ,body^ ,env^ ,k^)))

(define cont-mult1
  (λ (x2^ env^ k^)
    `(cont-mult1 ,x2^ ,env^ ,k^)))

(define cont-mult2
  (λ (lhs k^)
    `(cont-mult2 ,lhs ,k^)))

(define cont-app1
  (λ (rand^ env^ k^)
    `(cont-app1 ,rand^ ,env^ ,k^)))

(define cont-app2
  (λ (rator k^)
    `(cont-app2 ,rator ,k^)))

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env (cont-mult1 x2 env k))]
      [`(sub1 ,x)
       (value-of-cps x env (cont-sub1 k))]
      [`(zero ,x)
       (value-of-cps x env (cont-zero k))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env (cont-if conseq alt env k))]
      [`(catch ,body)
       (value-of-cps body (extend-env env k)
                     ; eta-reduction
                     k)]
      [`(pitch ,k-exp ,v-exp)
       (value-of-cps k-exp env (cont-pitch v-exp env))]
      [`(let ,e ,body)
       (value-of-cps e env (cont-let body env k))]
      [`(var ,y) (apply-env env y k)]
      [`(lambda ,body)
       (apply-k k (make-closure body env))]
      [`(app ,rator ,rand)
       (value-of-cps rator env (cont-app1 rand env k))]
      )))

(define empty-env
  (λ ()
    `(empty-env)))

(define empty-k
  (λ ()
    `(empty-k)))

(define apply-env
  (λ (env y k^)
    (match env
      [`(empty-env) (error "apply-env: Unbound variable ~a" y)]
      [`(extend-env ,env^ ,y^) (if (zero? y)
                                   (apply-k k^ y^)
                                   (apply-env env^ (sub1 y) k^))])))

(define make-closure
  (λ (body env)
    `(make-closure ,body ,env)))

(define apply-closure
  (λ (clos v k)
    (match clos
      [`(make-closure ,body ,env)
       (value-of-cps body (extend-env env v) k)]
      )))

(define apply-k
  (λ (k v)
    (match k
      [`(cont-zero ,k^) (apply-k k^ (zero? v))]
      [`(cont-sub1 ,k^) (apply-k k^ (sub1 v))]
      [`(cont-if ,conseq^ ,alt^ ,env^ ,k^)
       (if v
           (value-of-cps conseq^ env^ k^)
           (value-of-cps alt^ env^ k^))]
      [`(cont-let ,body^ ,env^ ,k^)
       (value-of-cps body^ (extend-env env^ v) k^)]
      [`(cont-pitch ,v-exp ,env^)
       (value-of-cps v-exp env^ v)]
      [`(cont-mult1 ,x2^ ,env^ ,k^)
       (value-of-cps x2^ env^ (cont-mult2 v k^))]
      [`(cont-mult2 ,lhs ,k^)
       (apply-k k^ (* lhs v))]
      [`(cont-app1 ,rand^ ,env^, k^)
       (value-of-cps rand^ env^ (cont-app2 v k^))]
      [`(cont-app2 ,rator^ ,k^)
       (apply-closure rator^ v k^)]
      [`(empty-k) v]
      )))

(define extend-env
  (λ (env^ y^)
    `(extend-env ,env^ ,y^)))

(require rackunit)

(check-equal? (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
(check-equal? (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
(check-equal? (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
(check-equal? (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
(check-equal? (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
(check-equal? (value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(catch (pitch (const 5) (pitch (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(check-equal? (value-of-cps '(mult (const 3) (catch (pitch (const 5) (pitch (var 0) (const 5))))) (empty-env) (empty-k)) 15)
(check-equal? (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                            (empty-env)
                            (empty-k))
              4)
(check-equal? (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                            (empty-env)
                            (empty-k))
              4)
(check-equal? (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                  (lambda
                                      (lambda
                                          (if (zero (var 0))
                                              (const 1)
                                              (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                            (empty-env)
                            (empty-k))
              1)

(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))

(define car$ car)

(define cdr$
  (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
  (λ (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $)
                  (let ((n- (sub1 n)))
                    (cond
                      ((zero? n-) '())
                      (else (take$ n- (cdr$ $))))))))))

(define worlds-worst-random
  (delay (random 4)))

; delay creates a promise that will be evaluated when force is called
; force evaluates a promise
; So, we have a list whose cdr is a promise that will be evaluated when force is called.

; A list of tribonacci numbers (cdr is a promise that will be evaluated when force is called)
; (define trib$
;   (cons$ 0 (cons$ 0 (cons$ 1 (map$ + trib$ (map$ + (cdr$ trib$) (cdr$ (cdr$ trib$))))))))

(define trib-cps
  (λ (n k)
    (cond
      ((zero? n) (k 0))
      ((zero? (sub1 n)) (k 1))
      ((zero? (sub1 (sub1 n))) (k 1))
      (else
       (trib-cps (sub1 n)
                 (λ (x)
                   (trib-cps (sub1 (sub1 n))
                             (λ (y)
                               (trib-cps (sub1 (sub1 (sub1 n)))
                                         (λ (z)
                                           (k (+ x y z))))))))))))


(define trib$
  (cons$ 0
         (cons$ 1
                (cons$ 1
                       '()))))