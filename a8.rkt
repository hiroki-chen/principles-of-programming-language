#lang racket

; Below are registers for the arguments of the `fib` function and helper functions.
; The registers are represented as locations in the store.
(define *n* #f)
(define *k* #f)
(define *a-k* #f)
(define *v* #f)

; (define apply-k
;   (λ (k v)
;     (match k
;       (`(make-inner-fib-k ,fib-sub1-n^ ,k^)
;        (let* ((k k^)
;               (v (+ fib-sub1-n^ v)))
;          (apply-k k v)))
;       (`(make-outer-fib-k ,n^ ,k^)
;        (let* ((k (make-inner-fib-k v k^))
;               (n (sub1 (sub1 n^))))
;          (fib-cps n k)))
;       (`(init-k) v))))

(define apply-k
  (λ ()
    (match *a-k*
      [`(init-k) *v*]
      [`(make-inner-fib-k ,fib-sub1-n^ ,k^)
       (begin
         (set! *a-k* k^)
         (set! *v* (+ fib-sub1-n^ *v*))
         (apply-k))]
      [`(make-outer-fib-k ,n^ ,k^)
       (begin
         (set! *k* (make-inner-fib-k *v* k^))
         (set! *n* (sub1 (sub1 n^)))
         (fib-cps))]
      )))

(define make-inner-fib-k
  (λ (fib-sub1-n^ k^)
    `(make-inner-fib-k ,fib-sub1-n^ ,k^)))

(define make-outer-fib-k
  (λ (n^ k^)
    `(make-outer-fib-k ,n^ ,k^)))

(define init-k
  (λ ()
    `(init-k)))

; This definition of fibonacci is different than the one without `k
; because here we use continutation passing style to capture the
; operation that should be performed after the current value is
; computed. Also, this definition is different because we use
; `apply-k` to apply the continuation `k` to the value `v` instead o
; just returning `v`, making it representation independent w.r.t.
; functions.
;
; Another notable difference is that we use `let*` binding to bind
; `k` and `v` to the values that should be used in the continuation.
; This creates locations for `k` and `v` in the store, which is not
; the case in the definition without `k`.
(define fib-cps
  (λ ()
    (cond
      ((or (zero? *n*) (zero? (sub1 *n*)))
       (begin
         (set! *a-k* *k*)
         (set! *v* 1)
         (apply-k)))
      (else
       (begin
         (set! *k* (make-outer-fib-k *n* *k*))
         (set! *n* (sub1 *n*))
         (fib-cps))))))

(begin
  (set! *k* (init-k))
  (set! *n* 5)
  (fib-cps))

(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (apply-k-times k 1)]
      [(zero? (car ls)) (apply-k-times k 0)]
      [else (times-cps (cdr ls) (make-k-times k (car ls)))])))

(define empty-k
  (λ (jumpout)
    `(empty-k ,jumpout)))

(define make-k-times
  (λ (k^ v^)
    `(make-k ,v^ ,k^)))

(define apply-k-times
  (λ (k v)
    (match k
      [`(empty-k ,jumpout) (jumpout v)]
      [`(make-k ,v^ ,k^) (apply-k-times k^ (* v^ v))]
      )))

(define bi-tramp
  (λ (th1 th2)
    (bi-tramp th2 (th1))))

(let/cc jumpout
  (bi-tramp (times-cps '(4 3 0 1 ) (empty-k jumpout))
            (times-cps '() (empty-k jumpout))))

(let/cc jumpout
  (bi-tramp (times-cps '(5 5 5 5) (empty-k jumpout))
            (empty-k jumpout)))