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
