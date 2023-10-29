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
  (λ (*e* *env* *k*)
    (union-case *e* expr
                [(const e)
                 (let* ([*v* e])
                   (apply-k *k* *v*))]
                [(mult x1 x2)
                 (let* ([*k* (cont-mult1 x2 *env* *k*)]
                        [*e* x1])
                   (value-of-cps *e* *env* *k*))]
                [(sub1 x)
                 (let* ([*k* (cont-sub1 *k*)]
                        [*e* x])
                   (value-of-cps *e* *env* *k*))]
                [(zero x)
                 (let* ([*k* (cont-zero *k*)]
                        [*e* x])
                   (value-of-cps *e* *env* *k*))]
                [(if test conseq alt)
                 (let* ([*k* (cont-if conseq alt *env* *k*)]
                        [*e* test])
                   (value-of-cps *e* *env* *k*))]
                [(catch body)
                 (let* ([*env* (extend-env *env* *k*)]
                        [*e* body])
                   (value-of-cps *e* *env* *k*))]
                [(pitch k-exp v-exp)
                 (let* ([*k* (cont-pitch v-exp *env*)]
                        [*e* k-exp])
                   (value-of-cps *e* *env* *k*))]
                [(let e body)
                 (let* ([*k* (cont-let body *env* *k*)]
                        [*e* e])
                   (value-of-cps *e* *env* *k*))]
                [(var y)
                 (let* ([*y* y])
                   (apply-env *env* *y* *k*))]
                [(lambda body)
                 (let* ([*v* (make-closure body *env*)])
                   (apply-k *k* *v*))]
                [(app rator rand)
                 (let* ([*k* (cont-app1 rand *env* *k*)]
                        [*e* rator])
                   (value-of-cps *e* *env* *k*))]
                )))

(define empty-env
  (λ ()
    `(empty-env)))

(define empty-k
  (λ ()
    `(empty-k)))

(define apply-env
  (λ (*env* *y* *k*)
    (match *env*
      [`(empty-env) (error "apply-env: Unbound variable ~a" *y*)]
      [`(extend-env ,env^ ,y^) (if (zero? *y*)
                                   (let* ([*v* y^])
                                     (apply-k *k* *v*))
                                   (let* ([*y* (sub1 *y*)]
                                          [*env* env^])
                                     (apply-env *env* *y* *k*)))])))

(define make-closure
  (λ (body env)
    `(make-closure ,body ,env)))

(define apply-closure
  (λ (*clos* *v* *k*)
    (match *clos*
      [`(make-closure ,body ,env)
       (let* ([*env* (extend-env env *v*)]
              [*e* body])
         (value-of-cps *e* *env* *k*))])))

(define apply-k
  (λ (*k* *v*)
    (match *k*
      [`(cont-zero ,k^)
       (let* ([*k* k^]
              [*v* (zero? *v*)])
         (apply-k *k* *v*))]
      [`(cont-sub1 ,k^)
       (let* ([*k* k^]
              [*v* (sub1 *v*)])
         (apply-k *k* *v*))]
      [`(cont-if ,conseq^ ,alt^ ,env^ ,k^)
       (if *v*
           (let* (
                  [*k* k^]
                  [*e* conseq^]
                  [*env* env^]
                  )
             (value-of-cps *e* *env* *k*))
           (let* ([*k* k^]
                  [*e* alt^]
                  [*env* env^]
                  )
             (value-of-cps *e* *env* *k*)))]
      [`(cont-let ,body^ ,env^ ,k^)
       (let* ([*k* k^]
              [*env* (extend-env env^ *v*)]
              [*e* body^])
         (value-of-cps *e* *env* *k*))]
      [`(cont-pitch ,v-exp ,env^)
       (let* ([*k* *v*]
              [*e* v-exp]
              [*env* env^])
         (value-of-cps *e* *env* *k*))]
      [`(cont-mult1 ,x2^ ,env^ ,k^)
       (let* ([*k* (cont-mult2 *v* k^)]
              [*e* x2^]
              [*env* env^])
         (value-of-cps *e* *env* *k*))
       ]
      [`(cont-mult2 ,lhs ,k^)
       (let* ([*k* k^]
              [*v* (* lhs *v*)])
         (apply-k *k* *v*))]
      [`(cont-app1 ,rand^ ,env^, k^)
       (let* ([*k* (cont-app2 *v* k^)]
              [*e* rand^]
              [*env* env^])
         (value-of-cps *e* *env* *k*))]
      [`(cont-app2 ,rator^ ,k^)
       (let* ([*k* k^]
              [*clos* rator^])
         (apply-closure *clos* *v* *k*))]
      [`(empty-k) *v*]
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