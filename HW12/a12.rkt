#lang racket

(require "mk-func.rkt")

(define appendo
  (λ (l1 s2 o)
    (λ (S)
      (λ () ;; be sure to use the same trick here to make your definitons work!
        (
         (disj
          (conj (== '() l1) (== s2 o))
          (call/fresh 'a
                      (λ (a)
                        (call/fresh 'd
                                    (λ (d)
                                      (call/fresh 'res
                                                  (λ (res)
                                                    (conj
                                                     (== `(,a . ,d) l1)
                                                     (== `(,a . ,res) o)
                                                     (appendo d s2 res)))))))))
         S)))))

(run*-g!
 (λ (q)
   (call/fresh 'r
               (λ (r)
                 (call/fresh 's
                             (λ (s)
                               (conj
                                (== `(,r ,s) q)
                                (appendo r s '(a b c d e)))))))))

(define (reverseo ls o)
  (λ (S)
    (λ ()
      ((disj
        (conj
         (== '() ls)
         (== '() o))
        (call/fresh 'a
                    (λ (a)
                      (call/fresh 'd (λ (d)
                                       (call/fresh 'res (λ (res)
                                                          (conj (== `(,a . ,d) ls)
                                                                (reverseo d res)
                                                                (appendo res `(,a) o))
                                                          )))))))
       S))
    )
  )

(run*-g (λ (x)
          (reverseo `(a b c d) `(d . ,x))))


(define (stuttero ls o)
  (λ (S)
    (λ ()
      ((disj
        (conj
         (== `() ls)
         (== '() o))
        (call/fresh 'a (λ (a)
                         (call/fresh 'd (λ (d)
                                          (call/fresh 'res (λ (res)
                                                             (conj
                                                              (== `(,a . ,d) ls)
                                                              (== `(,a ,a . ,res) o)
                                                              (stuttero d res)))))))))
       S))
    )
  )
