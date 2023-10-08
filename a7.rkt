#lang racket

; (+ 2 (call/cc
;       (λ (k)
;         (+ 30 (k 10)))))

; Complete the following definition of last-non-zero, a function which takes a list of numbers and return the last cdr whose car is 0. In other words, when starting from the right of the list, it should be all numbers before the first 0 is reached. See the test cases below and student test file for examples. Your solution should be naturally recursive, and should not contain any calls to member-like operations, nor should you be reversing the list.
(define last-non-zero
  (λ (ls)
    (let/cc k ; k captures nothing, meaning that this continuation simply
      ; "throws away" previous context.
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
           (index y ls)]
      [`(λ (,x) ,body) #:when (symbol? x)
                       `(λ ,(lex body (cons x ls)))]
      [`(zero? ,nexp) `(zero ,(lex nexp ls))]
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 ls) ,(lex nexp2 ls))]
      [`(catch ,cname ,cexp) `(catch ,(lex cexp (cons cname ls)))]
      [`(pitch ,exp1 ,cexp) #:when (symbol? cexp)
                            `(pitch ,(lex exp1 ls) ,(lex cexp ls))]
      [`(,rator ,rand)
       `(app ,(lex rator ls) ,(lex rand ls))])))

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env
                     (λ (lhs)
                       (value-of-cps x2 env
                                     (λ (rhs)
                                       (k (* lhs rhs))))))]
      [`(sub1 ,x)
       (value-of-cps x env (λ (v) (k (sub1 v))))]
      [`(zero ,x)
       (value-of-cps x env (λ (v) (k (zero? v))))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env (λ (test)
                                (if test
                                    (value-of-cps conseq env k)
                                    (value-of-cps alt env k))))]
      [`(catch ,body)
       (value-of-cps body
                     (λ (y k^)
                       (if (zero? y)
                           (k^ k)
                           (env (sub1 y) k^)))
                     ; eta-reduction
                     k)]
      [`(pitch ,k-exp ,v-exp)
       (value-of-cps k-exp env
                     (λ (k^)
                       (value-of-cps v-exp env
                                     (λ (v)
                                       (k^ v)))))]
      [`(let ,e ,body)
       (value-of-cps e env
                     (λ (v)
                       (value-of-cps body
                                     (λ (y k^)
                                       (if (zero? y)
                                           (k^ v)
                                           (env (sub1 y) k^)))
                                     k)))]
      [`(var ,y) (env y k)]
      [`(lambda ,body)
       (k (λ (a k^)
            (value-of-cps body
                          (λ (y k^^)
                            (if (zero? y)
                                (k^^ a)
                                (env (sub1 y) k^^)))
                          k^)))]
      [`(app ,rator ,rand)
       (value-of-cps rator env (λ (rator)
                                 (value-of-cps rand env
                                               (λ (rand)
                                                 (rator rand k)))))]
      )))

(define empty-env
  (λ ()
    (λ (y k)
      (error 'value-of-cps "unbound indentifier" ~a y))))

(define empty-k
  (λ ()
    (λ (v) v)))

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
