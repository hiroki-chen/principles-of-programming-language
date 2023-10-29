#lang racket
(require "parenthec.rkt")
(require "pc2py.rkt")

; Change all of your (define name (lambda () ...)) statements to instead use define-label. Define your program counter at the top of the program using define-program-counter.

(define-registers *e* *env* *k* *v* *clos* *y*)
(define-program-counter *pc*)

; Becomes expr_xxx
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

(define-union clos
  (closure body env))

(define-union envr
  (empty)
  (extend env y))

(define-union kt
  (empty)
  (sub1 k^)
  (zero k^)
  (if conseq^ alt^ env^ k^)
  (pitch v-exp env^)
  (let body^ env^ k^)
  (mult1 x2^ env^ k^)
  (mult2 lhs k^)
  (app1 rand^ env^ k^)
  (app2 rator k^))

(define-label value-of-cps
  (union-case *e* expr
              [(const e)
               (begin
                 (set! *v* e)
                 (apply-k))]
              [(mult x1 x2)
               (begin
                 (set! *k* (kt_mult1 x2 *env* *k*))
                 (set! *e* x1)
                 (value-of-cps))]
              [(sub1 x)
               (begin
                 (set! *k* (kt_sub1 *k*))
                 (set! *e* x)
                 (value-of-cps))]
              [(zero x)
               (begin
                 (set! *k* (kt_zero *k*))
                 (set! *e* x)
                 (value-of-cps))]
              [(if test conseq alt)
               (begin
                 (set! *k* (kt_if conseq alt *env* *k*))
                 (set! *e* test)
                 (value-of-cps))]
              [(catch body)
               (begin
                 (set! *env* (envr_extend *env* *k*))
                 (set! *e* body)
                 (value-of-cps))]
              [(pitch k-exp v-exp)
               (begin
                 (set! *k* (kt_pitch v-exp *env*))
                 (set! *e* k-exp)
                 (value-of-cps))]
              [(let e body)
               (begin
                 (set! *k* (kt_let body *env* *k*))
                 (set! *e* e)
                 (value-of-cps))]
              [(var y)
               (begin
                 (set! *y* y)
                 (apply-env))]
              [(lambda body)
               (begin
                 (set! *v* (clos_closure body *env*))
                 (apply-k))]
              [(app rator rand)
               (begin
                 (set! *k* (kt_app1 rand *env* *k*))
                 (set! *e* rator)
                 (value-of-cps))]
              ))

(define-label apply-env
  (union-case *env* envr
              [(empty) (error "apply-env: Unbound variable" *y*)]
              [(extend env^ y^) (if (zero? *y*)
                                    (begin
                                      (set! *v* y^)
                                      (apply-k))
                                    (begin
                                      (set! *y* (sub1 *y*))
                                      (set! *env* env^)
                                      (apply-env)))]))

(define-label apply-closure
  (union-case *clos* clos
              [(closure body env)
               (begin
                 (set! *env* (envr_extend env *v*))
                 (set! *e* body)
                 (value-of-cps))]))

(define-label apply-k
  (union-case *k* kt
              [(zero k^)
               (begin
                 (set! *k* k^)
                 (set! *v* (zero? *v*))
                 (apply-k))]
              [(sub1 k^)
               (begin
                 (set! *k* k^)
                 (set! *v* (sub1 *v*))
                 (apply-k))]
              [(if conseq^ alt^ env^ k^)
               (begin
                 (set! *k* k^)
                 (set! *env* env^)
                 (if *v*
                     (set! *e* conseq^)
                     (set! *e* alt^))
                 (value-of-cps))]
              [(let body^ env^ k^)
               (begin
                 (set! *k* k^)
                 (set! *env* (envr_extend env^ *v*))
                 (set! *e* body^)
                 (value-of-cps))]
              [(pitch v-exp env^)
               (begin
                 (set! *k* *v*)
                 (set! *e* v-exp)
                 (set! *env* env^)
                 (value-of-cps))]
              [(mult1 x2^ env^ k^)
               (begin
                 (set! *k* (kt_mult2 *v* k^))
                 (set! *e* x2^)
                 (set! *env* env^)
                 (value-of-cps))]
              [(mult2 lhs k^)
               (begin
                 (set! *k* k^)
                 (set! *v* (* lhs *v*))
                 (apply-k))]
              [(app1 rand^ env^ k^)
               (begin
                 (set! *k* (kt_app2 *v* k^))
                 (set! *e* rand^)
                 (set! *env* env^)
                 (value-of-cps))]
              [(app2 rator^ k^)
               (begin
                 (set! *k* k^)
                 (set! *clos* rator^)
                 (apply-closure))]
              [(empty) *v*]))

(define-label main
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
    (set! *env* (envr_empty))
    (set! *k* (kt_empty))
    (value-of-cps)))

(main)