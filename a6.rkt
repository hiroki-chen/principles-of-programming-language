#lang racket

(define id (λ (v) v))

(define empty-k
  (λ ()
    (let ((once-only #f))
      (λ (v)
        (if once-only
        (error 'empty-k "You can only invoke empty continuation once!")
        (begin
          (set once-only #t)
          v))))))

(define binary-to-decimal-cps
  (λ (n k)
    (cond
      ((null? n) (k 0))
      (else (binary-to-decimal-cps (cdr n) (λ (v)
                                             (k (+ (car n) (* 2 v)))))))))

(define star-cps
  (λ (m k)
    (k (λ (n k)
         (k (* m n))))))

(define times-cps
  (λ (ls k)
    (cond
      ((null? ls) (k 1))
      ((zero? (car ls)) (k 0))
      (else (times-cps (cdr ls) (λ (v) (k (* (car ls) v))))))))

; Since 0 is absorbing, we can abort when car ls = 0.
(define times-cps-shortcut
  (λ (ls k)
    (cond
      ((null? ls) (k 1))
      ((zero? (car ls)) 0)
      (else (times-cps-shortcut (cdr ls) (λ (v) (k (* (car ls) v))))))))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k ls)]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls)
                          (λ (fst)
                            (remv-first-9*-cps (cdr ls)
                                               (λ (snd)
                                                 (k (cond
                                                   ((equal? (car ls) fst)
                                                    (cons (car ls) snd))
                                                   (else (cons fst (cdr ls)))))))))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls)
                               (λ (v)
                                 (k (cons (car ls) v))))])))

(define cons-cell-count-cps
  (λ (ls k)
    (cond
      ((pair? ls)
       (cons-cell-count-cps (car ls)
                            (λ (v)
                              (cons-cell-count-cps (cdr ls)
                                                   (λ (w)
                                                     (k (add1 (+ v w))))))))
      (else (k 0)))))

(define find-cps
  (λ (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s (λ (v) (k v)))
          (k u)))))

(define ack-cps
  (λ (m n k)
    (cond
      ((zero? m) (k (add1 n)))
      ((zero? n) (ack-cps (sub1 m) 1 k))
      ; Eta-reduction
      (else (ack-cps m (sub1 n) (λ (v) (ack-cps (sub1 m) v (λ (w) (k w)))))))))

(require racket/trace)
;(trace ack-cps)

(define fib-cps
  (λ (n k)
    ((λ (fib-cps k)
       (fib-cps fib-cps n k))
     (λ (fib-cps n k)
       (cond
     [(zero? n) (k 0)]
     [(zero? (sub1 n)) (k 1)]
     [else (fib-cps fib-cps (sub1 n)
                    (λ (v) (fib-cps fib-cps (sub1 (sub1 n))
                                    (λ (w) (k (+ w v))))))])) k)))

(define unfold-cps
  (λ (p f g seed k)
    ((λ (h k)
      (h h (λ (v) (v seed '() k))))
    (λ (h k)
      (k (λ (seed ans k)
           (p seed
              (λ (v)
                (if v
                    (k ans)
                    (h h (λ (hh)
                           (f seed
                              (λ (w)
                                (g seed
                                   (λ (x)
                                     (hh x (cons w ans) k)))))))))))))
    k)))
(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

(define car-cps
    (lambda (pr k)
      (k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

(define empty-s
  (lambda ()
    '()))

(define unify-cps
  (λ (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (unify-cps (find-cps (car u) s k) (find-cps (car v) s k) s
                  (λ (v)
                    (unify-cps (find-cps (cdr u) s k) (find-cps (cdr v) s k) s
                               (λ (w)
                                 (if (pair? v)
                                     (if v
                                         (k w)
                                         (k #f))
                                     #f))))))
      (else #f))))

(define M-cps
  (λ (f k)
    (k (λ (ls k)
      (cond
        ((null? ls) (k '()))
        (else (M-cps f
                     (λ (v)
                       (v (cdr ls)
                          (λ (w)
                            (f (car ls)
                               (λ (x)
                                 (k (cons x w))))))))))))))


(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))

(define strange-cps
  (λ (x k)
    ((λ (g k) (k (λ (x k) (g g k))))
     (λ (g k) (k (λ (x k) (g g k)))) (λ (v) v))))

(define use-of-strange-cps
  (let ([strange^ (((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))])
    (((strange^ 8 (empty-k)) 9 (empty-k)) 10 (empty-k))))

(use-of-M-cps)
