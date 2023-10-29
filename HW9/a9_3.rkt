#lang racket
(require "parenthec.rkt")
(require "pc2py.rkt")

; Registerize the interpreter. Turn each let* expression to a begin block: the former let* bindings will become set! expressions, and the body becomes the invocation of a function of no arguments. Change all serious functions to be functions of no arguments. Define your global registers using define-registers at the top of the program.

(define-registers *e* *env* *k* *v* *clos* *y*)

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
  (λ ()
    (union-case *e* expr
                [(const e)
                 (begin
                   (set! *v* e)
                   (apply-k))]
                [(mult x1 x2)
                 (begin
                   (set! *k* (cont-mult1 x2 *env* *k*))
                   (set! *e* x1)
                   (value-of-cps))]
                [(sub1 x)
                 (begin
                   (set! *k* (cont-sub1 *k*))
                   (set! *e* x)
                   (value-of-cps))]
                [(zero x)
                 (begin
                   (set! *k* (cont-zero *k*))
                   (set! *e* x)
                   (value-of-cps))]
                [(if test conseq alt)
                 (begin
                   (set! *k* (cont-if conseq alt *env* *k*))
                   (set! *e* test)
                   (value-of-cps))]
                [(catch body)
                 (begin
                   (set! *env* (extend-env *env* *k*))
                   (set! *e* body)
                   (value-of-cps))]
                [(pitch k-exp v-exp)
                 (begin
                   (set! *k* (cont-pitch v-exp *env*))
                   (set! *e* k-exp)
                   (value-of-cps))]
                [(let e body)
                 (begin
                   (set! *k* (cont-let body *env* *k*))
                   (set! *e* e)
                   (value-of-cps))]
                [(var y)
                 (begin
                   (set! *y* y)
                   (apply-env))]
                [(lambda body)
                 (begin
                   (set! *v* (make-closure body *env*))
                   (apply-k))]
                [(app rator rand)
                 (begin
                   (set! *k* (cont-app1 rand *env* *k*))
                   (set! *e* rator)
                   (value-of-cps))]
                )))

(define empty-env
  (λ ()
    `(empty-env)))

(define empty-k
  (λ ()
    `(empty-k)))

(define apply-env
  (λ ()
    (match *env*
      [`(empty-env) (error "apply-env: Unbound variable" *y*)]
      [`(extend-env ,env^ ,y^) (if (zero? *y*)
                                   (begin
                                     (set! *v* y^)
                                     (apply-k))
                                   (begin
                                     (set! *y* (sub1 *y*))
                                     (set! *env* env^)
                                     (apply-env)))])))

(define make-closure
  (λ (body env)
    `(make-closure ,body ,env)))

(define apply-closure
  (λ ()
    (match *clos*
      [`(make-closure ,body ,env)
       (begin
         (set! *env* (extend-env env *v*))
         (set! *e* body)
         (value-of-cps))])))

(define apply-k
  (λ ()
    (match *k*
      [`(cont-zero ,k^)
       (begin
         (set! *k* k^)
         (set! *v* (zero? *v*))
         (apply-k))]
      [`(cont-sub1 ,k^)
       (begin
         (set! *k* k^)
         (set! *v* (sub1 *v*))
         (apply-k))]
      [`(cont-if ,conseq^ ,alt^ ,env^ ,k^)
       (begin
         (set! *k* k^)
         (set! *env* env^)
         (if *v*
             (set! *e* conseq^)
             (set! *e* alt^))
         (value-of-cps))]
      [`(cont-let ,body^ ,env^ ,k^)
       (begin
         (set! *k* k^)
         (set! *env* (extend-env env^ *v*))
         (set! *e* body^)
         (value-of-cps))]
      [`(cont-pitch ,v-exp ,env^)
       (begin
         (set! *k* *v*)
         (set! *e* v-exp)
         (set! *env* env^)
         (value-of-cps))]
      [`(cont-mult1 ,x2^ ,env^ ,k^)
       (begin
         (set! *k* (cont-mult2 *v* k^))
         (set! *e* x2^)
         (set! *env* env^)
         (value-of-cps))]
      [`(cont-mult2 ,lhs ,k^)
       (begin
         (set! *k* k^)
         (set! *v* (* lhs *v*))
         (apply-k))]
      [`(cont-app1 ,rand^ ,env^, k^)
       (begin
         (set! *k* (cont-app2 *v* k^))
         (set! *e* rand^)
         (set! *env* env^)
         (value-of-cps))]
      [`(cont-app2 ,rator^ ,k^)
       (begin
         (set! *k* k^)
         (set! *clos* rator^)
         (apply-closure))]
      [`(empty-k) *v*]
      )))

(define extend-env
  (λ (env^ y^)
    `(extend-env ,env^ ,y^)))

(define main
  (lambda ()
    (begin
      (set! *e* (expr_let
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
                  (expr_const 5))))
      (set! *env* (empty-env))
      (set! *k* (empty-k))
      (value-of-cps))))

(main)
