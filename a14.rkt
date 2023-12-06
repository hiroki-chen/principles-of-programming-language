#lang pie

(claim intriguing-word Atom)
(define intriguing-word 'hello)
(claim lucky-num Nat)
(define lucky-num (add1 0))
(claim to-go-order (Pair Nat Atom))
(define to-go-order (cons 1 'hello))
(claim MyFirstType U)
(define MyFirstType Nat)
(claim my-thing-and-Atom (Pair MyFirstType U))
(define my-thing-and-Atom (cons 1 Nat))

(claim with-Nats
  (-> (-> Nat Nat
        Nat)
      (Pair Nat Nat)
    Nat))

(define with-Nats
  (λ (f p)
    (f (car p) (cdr p))))
  
(check-same Nat (with-Nats (λ (n m) n) (cons 1 2)) 1)
(check-same Nat (with-Nats (λ (n m) (add1 m)) (cons 1 2)) 3)

(claim at-least-two?
  (-> Nat
    Atom))

(define at-least-two?
  (λ (n)
    (which-Nat n
      'nil
      (λ (n-1)
        (which-Nat n-1
          'nil
          (λ (n-2)
            't
          )
        )
      )
      )
  ))
(check-same Atom (at-least-two? 0) 'nil)
(check-same Atom (at-least-two? 1) 'nil)
(check-same Atom (at-least-two? 41) 't)

(claim + (-> Nat Nat
           Nat))
(define + (λ (n m) (rec-Nat n
                     m
                     (λ (k k+m) (add1 k+m)))))
 
(claim * (-> Nat Nat
           Nat))
(define * (λ (n m) (rec-Nat n
                     0
                     (λ (k k*m) (+ m k*m)))))

(claim expt (-> Nat Nat Nat))

(define expt
  (λ (n m)
    (rec-Nat m
      1
      (λ (m-1 res)
        (* n res)
      )
    )
  )
)

(claim map
  (Π ((A U)
      (B U))
    (→ (→ A B) (List A)
       (List B))))

(define map
  (λ (A B)
    (λ (f l)
      (rec-List l
        (the (List B) nil)
        (λ (a d r)
          (:: (f a) r)
        )
      )
    )
  )
)

(claim nth
  (Π ((A U))
    (→ (List A) A Nat
       A)))

(define nth
  (λ (A)
    (λ (l)
      (rec-List l
        (the (-> A Nat A) (lambda (default n) default))
        (λ (a d r)
          (λ (res n)
            (which-Nat n
              a
              (λ (n-1)
                (r res n-1)
              )
            )
          )
        )
      )
)))

(nth Nat (:: 1 (:: 2 (:: 3 nil))) 0 2)