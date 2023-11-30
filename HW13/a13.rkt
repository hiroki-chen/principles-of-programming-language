#lang racket

(require "monads.rkt")


; The Racket function findf takes a list and a predicate against which to test the list’s elements.
; In our implementation, we will return either the leftmost list element to pass the predicate or
; (Nothing) if none of the list’s elements pass.
(define findf-maybe
  (λ (f l)
    (match l
      (`() (Nothing))
      (`(,x . ,xs)
       (if (f x)
           (Just x)
           (findf-maybe f xs)
           )
       ))))
(findf-maybe number? '(a b c))
(findf-maybe boolean? '(#f 1 2 c))
(findf-maybe symbol? '(1 2 c))

; The writer monad provides a mechanism to write data separately from the actual return value.
; If we use a list to represent these writes, we can use this monad to implement some rather useful
; functions. The function partition-writer takes a predicate and a list, returning a dotted pair
; with the values that do not pass the predicate in the writer part of the monad and the values that
; do in the value/pure part. Implement this using the writer monad.
(define partition-writer
  (λ (f l)
    (match l
      (`() (inj-writer '()))
      (`(,x . ,xs)
       (if (f x)
           (go-on
            (next <- (partition-writer f xs))
            (inj-writer (cons x next)))
           (go-on
            (tell x)
            (partition-writer f xs)
            )
           )))))

(run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))

(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       ;  (* x (power x (sub1 n)))
       (go-on
        (next <- (powerXpartials x (sub1 n)))
        (tell next)
        (inj-writer (* x next))
        )
       ]
      [(even? n) (let ((nhalf (/ n 2)))
                   (go-on
                    (y <- (powerXpartials x nhalf))
                    (tell y)
                    (inj-writer (* y y))
                    ))])))
(run-writer (powerXpartials 2 6))
(run-writer (powerXpartials 5 7))

(define replace-with-count
  (λ (x tr)
    (cond
      ((null? tr) (inj-state '()))
      ((symbol? tr)
       (if (eqv? x tr)
           (go-on
            (val <- (get))
            (put (add1 val))
            (inj-state val))
           (inj-state tr)))
      ((pair? tr)
       (go-on
        (lhs <- (replace-with-count x (car tr)))
        (rhs <- (replace-with-count x (cdr tr)))
        (inj-state (cons lhs rhs)))))
    ))

((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)

(define traverse
  (lambda (inj bind f)
    (letrec
        ((trav
          (lambda (tree)
            (cond
              [(pair? tree)
               (go-on (a <- (trav (car tree)))
                      (d <- (trav (cdr tree)))
                      (inj (cons a d)))]
              [else (f tree)]))))
      trav)))

(define reciprocal
  (λ (n)
    (cond
      ((zero? n) (Nothing))
      (else (Just (/ 1 n))))
    ))

(define traverse-reciprocal
  (traverse Just bind-maybe reciprocal))

(define halve
  (λ (n)
    (cond
      ((even? n) (inj-writer (/ n 2)))
      (else (go-on
             (tell n)
             (inj-writer n))))
    ))
(define traverse-halve
  (traverse inj-writer bind-writer halve))

(define state/sum
  (λ (n)
    (go-on
     (val <- (get))
     (put (+ val n))
     (inj-state val))
    ))

(define traverse-state/sum
  (traverse inj-state bind-state state/sum))

(define empty-env
  (λ ()
    (λ () (error "error"))))

(define apply-env
  (λ (env y)
    (env y)))

(define extend-env
  (λ (x arg env)
    (λ (y)
      (if (eqv? y x)
          arg
          (apply-env env y)))))

(define apply-proc
  (λ (clos arg)
    (clos arg)))

(define closure
  (λ (x body env)
    (λ (arg)
      (value-of-cps body (extend-env x arg env)))))

(define value-of-cps
  (lambda (expr env)
    (match expr
      [`,n #:when (natural? n) (inj-k expr)]
      [`,b #:when (boolean? b) (inj-k b)]
      [`,y #:when (symbol? y) (inj-k (apply-env env y))]
      [`(* ,x1 ,x2) (go-on
                     (lhs <- (value-of-cps x1 env))
                     (rhs <- (value-of-cps x2 env))
                     (inj-k (* lhs rhs)))]
      [`(sub1 ,x) (go-on (val <- (value-of-cps x env))
                         (inj-state (sub1 val)))]
      [`(zero? ,x) (go-on (val <- (value-of-cps x env))
                          (inj-k (zero? val)))]
      [`(if ,test ,conseq ,alt) (go-on (test <- (value-of-cps test env))
                                       (conseq <- (value-of-cps conseq env))
                                       (alt <- (value-of-cps alt env))
                                       (inj-k (if test conseq alt)))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                        (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) (go-on (k <- (value-of-cps k-exp env))
                                      (v <- (value-of-cps v-exp env))
                                      (k v))]
      [`(lambda (,id) ,body) (inj-k (closure id body env))]
      [`(,rator ,rand) (go-on (func <- (value-of-cps rator env))
                              (arg <- (value-of-cps rand env))
                              (inj-k (apply-proc func arg)))])))
(define fact-5
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

((run-k (value-of-cps fact-5 (empty-env))) (lambda (v) v))
(define capture-fun
  '((lambda (x) x) 5))
((run-k (value-of-cps capture-fun (empty-env))) (lambda (v) v))