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
      ((symbol? x) x)
      (else #f))))

(define lambda-exp?
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [`,y #:when (symbol? y) #t]
              [`(lambda (,x) ,body) #:when (symbol? x) (p body)]
              [`(,rator, rand) (and (p rator) (p rand))]
              [else #f]))])
      (p E))))

(define var-occurs?
  (λ (var e)
    (match e
      [`,y #:when (symbol? y) (eqv? var y)]
      [`(lambda (,x) ,body) (var-occurs? var body)]
      [`(,rator ,rand) (or (var-occurs? var rator) (var-occurs? var rand))])))

(define vars
  (λ (e)
    (match e
      [`,y #:when (symbol? y) (cons y null)]
      [`(lambda (,x) ,body) (vars body)]
      [`(,rator ,rand) (append (vars rator) (vars rand))])))

(define unique-vars
  (λ (e)
    (match e
      [`,y #:when (symbol? y) (cons y null)]
      [`(lambda (,x) ,body) (union (cons x null) (unique-vars body))]
      [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))])))

(define var-occurs-free?
  (λ (x e)
    (match e
      [`,y #:when (symbol? y) (eqv? x y)]
      [`(lambda (,y) ,body) (and (not (eqv? x y)) (var-occurs-free? x body))]
      [`(,rator ,rand) (or (var-occurs-free? x rator) (var-occurs-free? x rand))])))

(define var-occurs-bound?
  (λ (x e)
    (match e
      [`,y #:when (symbol? y) #f]
      [`(lambda (,y) ,body) (or (and (eqv? x y) (var-occurs-free? x body)) (var-occurs-bound? x body))]
      [`(,rator ,rand) (or (var-occurs-bound? x rator) (var-occurs-bound? x rand))])))

(define unique-free-vars
  (λ (e)
    (match e
      [`,y #:when (symbol? y) (cons y null)]
      [`(lambda (,x) ,body) (remv x (unique-free-vars body))]
      [`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand))])))

(define unique-bound-vars
  (λ (e)
    (match e
      [`,y #:when (symbol? y) '()]
      [`(lambda (,x) ,body)
       (cond
         ((var-occurs? x body) (union (cons x null) (unique-bound-vars body)))
         (else (unique-bound-vars body)))]
      [`(,rator ,rand) (union (unique-bound-vars rator) (unique-bound-vars rand))])))

(define index
  (λ (var ls)
    (match ls
      ('() (error "Not found"))
      (`(,a . ,_) #:when (eqv? a var)
       0)
      (`(,_ . ,d)
       (add1 (index var d))))))

(define lex
  (λ (e ls)
    (match e
      (`,y #:when (symbol? y)
           (index y ls))
      (`(λ (,x) ,body) #:when (symbol? x)
       `(λ ,(lex body (cons x ls))))
      (`(,rator ,rand)
       `(,(lex rator ls) ,(lex rand ls))))))

(lex '(λ (y) (λ (x) y))
       '())

(define e1=e2?
  (λ (e1 e2)
    (letrec ([p (λ (e1 e2)
               (match `(,e1 ,e2)
                 [`(,y1 ,y2) #:when (and (natural? y1) (natural? y2)) (eqv? y1 y2)]
                 [`((λ ,body1) (λ ,body2))
                  (p body1 body2)]
                 [`((,rator1 ,rand1) (,rator2 ,rand2)) (and (p rator1 rator2) (p rand1 rand2))]))])
      (p (lex e1 '()) (lex e2 '())))))

(define t-fact
  (λ (n result)
    (match n
      (`,n #:when (zero? n) result)
      (else (t-fact (sub1 n) (* n result))))))
(trace t-fact)
(t-fact 5 1)
(define walk-symbol-update
  (λ (x l)
    (cond
      ; find the associated value and update the boxed value.
      ((assv x l) (let
                      [(v (assv x l))]
                    (cond
                      ((symbol? (unbox (cdr v))) (begin
                                                   (set-box! (cdr v) (walk-symbol-update (unbox (cdr v)) l))
                                                   (unbox (cdr v))))
                      (else (unbox (cdr v))))))
      ((symbol? x) x)
      (else #f))))

(define var-occurs-both?
  (λ (x e)
    (match e
      [`,y #:when (symbol? y) (cons (eqv? y x) #f)]
      [`(lambda (,y) ,body) (let ([v (var-occurs-both? x body)])
                              (cons (and (not (eqv? y x)) (car v)) (or (and (eqv? y x) (car v)) (cdr v))))]
      [`(,rator ,rand) (let ([lhs (var-occurs-both? x rator)]
                             [rhs (var-occurs-both? x rand)])
                         (cons (or (car lhs) (car rhs))
                               (or (cdr lhs) (cdr rhs))))])))
