#lang racket

(require "mk.rkt")
(require "numbers.rkt")

; The value for the following the expression should be (5) although we requsted 2 solutions.
;
; Let '/\' denote conjunction and '\/' denote disjunction.
; The expression is equivalent to:
; ((5 = q) \/ ((5 = q) \/ ((6 = q) /\ (5 = q)))) /\ (5 = q)
; Apparently, for (6 = q /\ 5 = q), it cannot hold since q cannot either be 6 or 5.
; Thus, the expression is equivalent to:
; ((5 = q) \/ ((5 = q) \/ #f)) /\ (5 = q)
; which further simplifies to:
; ((5 = q) \/ (5 = q)) /\ (5 = q)
; which is equivalent to:
; (5 = q) /\ (5 = q)
; which is equivalent to:
; 5 = q
(run 2 (q)
     (conj
      (== 5 q)
      (disj
       (conj
        (disj
         (conj
          (== 5 q)
          (== 6 q)))
        (== 5 q))
       (== q 5)
       )))

; The value for the following the expression should be ((_0 _1)) where _0 and _1 just mean
; 'anything' and 'anything' (but they are not the same 'anything') respectively, and _0 must
; be a symbol according to the constraint (symbolo a). Besides, 'tag must not appear in q,
; which is required by (absento 'tag q).
(run! 1 (q)
      (fresh (a b)
             (conj
              (== `(,a ,b) q)
              (absento 'tag q)
              (symbolo a))))

; ==: equal?

; =/=: not equal?

; numbero: number?

; symbolo: symbol?

; absento: not member?

(defrel (assoco x ls o)
  (fresh (a d aa da)
         (conj
          (== `(,a . ,d) ls)
          (== `(,aa . ,da) a)
          (disj
           (conj
            (== x aa)
            (== a o))
           (conj
            (=/= x aa)
            (assoco x d o)
            ))))
  )

(run*! q (assoco 'x '((x . 6) (x . 5)) q))
(run*! q (assoco 'x '((x . 6) (x . 5)) '(x . 6)))

(defrel (reverseo ls o)
  (disj
   (conj
    (== '() ls)
    (== '() o))
   (fresh (a d res)
          (== `(,a . ,d) ls)
          (reverseo d res)
          (appendo res `(,a) o)
          ))
  )
(run*! q (fresh (x) (reverseo `(a b c d) `(d    ,q . ,x))))

(defrel (stuttero ls o)
  (disj
   (conj
    (== `() ls)
    (== '() o))
   (fresh (a d res)
          (== `(,a . ,d) ls)
          (== `(,a ,a . ,res) o)
          (stuttero d res)
          )
   )
  )
(run! 5 q (fresh (x y z)
                 (assoco x y z)
                 (== `(,x ,y ,z) q)))

(defrel (lengtho ls o)
  (disj
   (conj
    (== '() ls)
    (== '() o))
   (fresh (a d res)
          (== `(,a . ,d) ls)
          (pluso '(1) res o)
          (lengtho d res)))
  )

(run*! q (stuttero q '(1 1 2 2 3 3)))