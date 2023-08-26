#lang racket

(define countdown
  (λ (n)
    (cond
      ((zero? n) (cons 0 null))
      (else (cons n (countdown (sub1 n)))))))

(countdown 5)

(define insertL
  (λ (x y l)
    (cond
      ((null? l) null)
      ((eqv? x (car l)) (cons y (cons x (insertL x y (cdr l)))))
      (else (cons (car l) (insertL x y (cdr l)))))))

(insertL 'x 'y '(x z z x y x))

(define remv-1st
  (λ (x l)
    (cond
      ((null? l) null)
      ; Do not remove others
      ((eqv? x (car l)) (cdr l))
      (else (cons (car l)(remv-1st x (cdr l)))))))

(define remove-from
  (λ (pred? l)
    (cond
      ((null? l) null)
      ((pred? (car l)) (remove-from pred? (cdr l)))
      (else (cons (car l) (remove-from pred? (cdr l)))))))

;(filter-out even? '(1 2 3 4 5 6))

(define map
  (λ (f l)
    (cond
      ((null? l) null)
      (else (cons (f (car l)) (map f (cdr l)))))))

(map sub1 '(1 2 3 4))

(define zip
  (λ (lhs rhs)
    (cond
      ((null? lhs) null)
      ((null? rhs) null)
      (else (cons (cons (car lhs) (car rhs)) (zip (cdr lhs) (cdr rhs)))))))

(define list-index-ofv-helper
  (λ (cur x l)
    (cond
      ((eqv? x (car l)) cur)
      (else (list-index-ofv-helper (add1 cur) x (cdr l))))))

(define list-index-ofv
  (λ (x l)
    (list-index-ofv-helper 0 x l)))

(define append
  (λ (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (append (cdr l1) l2))))))

(append '(a b c) '(cat dog))

(define reverse
  (λ (l)
    (cond
      ((null? l) null)
      (else (append (reverse (cdr l)) (cons (car l) null))))))

(reverse '(a 3 x))

(define repeat
  (λ (l n)
    (cond
      ((zero? n) null)
      (else (append l (repeat l (sub1 n)))))))

(repeat '(4 8 11) 4)

(define same-lists*
  (λ (lhs rhs)
    (cond
      ((and (null? lhs) (null? rhs)) #t)
      ((or (null? lhs) (null? rhs)) #f)
      ((eqv? (car lhs) (car rhs)) (same-lists* (cdr lhs) (cdr rhs)))
      ((and (pair? (car lhs)) (pair? (car rhs)))
       (cond
         ((same-lists* (car lhs) (car rhs)) (same-lists* (cdr lhs) (cdr rhs)))
         (else #f)))
      (else #f))))

; ((w x) y (z)) is equal to
; ((w . (x . ())) . (y . ((z . ()))))
(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))
(same-lists* '(1 2 3 4) '(1 2 3 4 5))
(same-lists* '(a (b c) d) '(a (b) c d))
(same-lists* '((a) b (c d) d) '((a) b (c d) d))
(define binary->natural
  (λ (num)
    (cond
      ((null? num) 0)
      (else (+ (car num) (* 2 (binary->natural (cdr num))))))))

(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))

(define div
  (λ (m n)
    (cond
      ((zero? m) 0)
      (else (add1 (div (- m n) n))))))

(define append-map
  (λ (proc list)
    (cond
      ((null? list) null)
      (else (append (proc (car list)) (append-map proc (cdr list)))))))

(append-map countdown (countdown 5))

(define contains?
  (λ (elem l)
    (cond
      ((null? l) #f)
      ((eqv? elem (car l)) #t)
      (else (contains? elem (cdr l))))))

(define set-difference
  (λ (lhs rhs)
    (cond
      ((null? lhs) null)
      ((contains? (car lhs) rhs) (set-difference (cdr lhs) rhs))
      (else (cons (car lhs) (set-difference (cdr lhs) rhs))))))

(set-difference '(1 2 3 4 5) '(2 6 4 8))

(define powerset
  (λ (l)
    (cond
      ((null? l) '(()))
      ; (car l) [...powerset of the rest set]
      ; [...rest] ++ [(car l) + [each element in the list]].
      (else (append (powerset (cdr l)) (map (λ (sb) (cons (car l) sb)) (powerset (cdr l))))))))

(define do-cartesian-single
  (λ (elem l)
    (cond
      ((null? l) null)
      (else (cons (list elem (car l)) (do-cartesian-single elem (cdr l)))))))

(define cartesian-product-helper
  (λ (lhs rhs)
    (cond
      ((null? lhs) null)
      (else (append (do-cartesian-single (car lhs) rhs) (cartesian-product-helper (cdr lhs) rhs))))))

(define cartesian-product
  (λ (l) (cartesian-product-helper (car l) (car (cdr l)))))

(cartesian-product '((5 4) (3 2 1)))
