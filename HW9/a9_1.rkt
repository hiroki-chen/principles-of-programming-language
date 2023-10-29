#lang racket
(require "parenthec.rkt")
(require "pc2py.rkt")

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (catch body)
  (pitch kexp vexp)
  (let exp body)
  (lambda body)
  (app rator rand))

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
  (λ (e env k)
    (union-case e expr
                [(const e) (apply-k k e)]
                [(mult x1 x2)
                 (value-of-cps x1 env (cont-mult1 x2 env k))]
                [(sub1 x)
                 (value-of-cps x env (cont-sub1 k))]
                [(zero x)
                 (value-of-cps x env (cont-zero k))]
                [(if test conseq alt)
                 (value-of-cps test env (cont-if conseq alt env k))]
                [(catch body)
                 (value-of-cps body (extend-env env k)
                               ; eta-reduction
                               k)]
                [(pitch k-exp v-exp)
                 (value-of-cps k-exp env (cont-pitch v-exp env))]
                [(let e body)
                 (value-of-cps e env (cont-let body env k))]
                [(var y) (apply-env env y k)]
                [(lambda body)
                 (apply-k k (make-closure body env))]
                [(app rator rand)
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

(define main
  (lambda ()
    (value-of-cps
     (expr_let
      (expr_lambda
       (expr_lambda
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult
          (expr_var 0)
          (expr_app
           (expr_app (expr_var 1) (expr_var 1))
           (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_catch
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_pitch
          (expr_var 0)
          (expr_app
           (expr_app (expr_var 1) (expr_var 1))
           (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(main)