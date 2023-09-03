#lang racket
 (require racket/trace)

(define list-ref
  (lambda (ls n)
    (letrec
      ([nth-cdr (lambda (n)
                  (cond
                    ((zero? n) ls)
                    (else (cdr (nth-cdr (sub1 n)))))
                  )])
      (car (nth-cdr n)))))


(define union
  (λ (lhs rhs)
    (cond
      ((null? lhs) rhs)
      ((memv (car lhs) rhs) (union (cdr lhs) rhs))
      (else (union (cdr lhs) (cons (car lhs) rhs))))))

(define stretch
  (λ (pred n)
    (λ (arg)
        (or (pred arg) (eqv? arg n)))))

((stretch even? 1) 2)
(filter (stretch (stretch (stretch even? 1) 3) 7) '(0 1 2 3 4 5))

(define walk-symbol
  (λ (x s)
    (cond
      ((assv x s) (cond
                    ((symbol? (cdr (assv x s))) (walk-symbol (cdr (assv x s)) (remv (assv x s) s)))
                    (else (cdr (assv x s)))))
      (else #f))))

(walk-symbol 'd '((a . 5) (b    1 2)  (c . a) (e . c) (d . e)))

(define lambda-exp?
  (λ (E)
    ))

(define parse/match
  (λ (e)
    (match e
      (`,y #:when (symbol? y) y)
      (`(λ (,x) ,body) #:when (symbol? x)
       `(lambda (,x) ,(parse/match body)))
      (`(,rator ,rand)
       `(,(parse/match rator) ,(parse/match rand))))))
