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

(define remove-from
  (λ (x l)
    (cond
      ((null? l) null)
      ; Do not remove others
      ((eqv? x (car l)) (cdr l))
      (else (cons (car l)(remove-from x (cdr l)))))))

(define filter-out
  (λ (pred? l)
    (cond
      ((null? l) null)
      ((pred? (car l)) (filter-out pred? (cdr l)))
      (else (cons (car l) (filter-out pred? (cdr l)))))))

(filter-out even? '(1 2 3 4 5 6))

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

(define same-list*
  (λ (lhs rhs)
    (cond
      ((null? lhs) (cond
                     ((null? rhs) #t)
                     (else #f)))
      ((eqv? (car lhs) (car rhs)) (same-list* (cdr lhs) (cdr rhs)))
      (else #f))))

; ((w x) y (z)) is equal to
; ((w . (x . ())) . (y . ((z . ()))))

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
