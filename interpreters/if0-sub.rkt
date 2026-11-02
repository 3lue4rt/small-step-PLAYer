#lang play
#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<sym> <expr>} <expr>}
         | <id>
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id s))
 
 
;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b)
     (with x (parse e) (parse b))]))
 
;; subst :: Expr symbol Expr
(define (subst expr sub-id val)
  (match expr
    [(num n) expr]
    [(add l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [(sub l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [(if0 c t f) (if0 (subst c sub-id val)
                      (subst t sub-id val)
                      (subst f sub-id val))]
    [(with bound-id named-expr body)
     (with bound-id
           (subst named-expr sub-id val)
           (if (symbol=? bound-id sub-id)
               body
               (subst body sub-id val)))]
    [(id x) (if (symbol=? x sub-id) val expr)]))
 
 
;; calc :: Expr -> number
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(if0 c t f)
     (if (zero? (calc c))
         (calc t)
         (calc f))]
    [(with bound-id named-expr bound-body)
     (calc (subst bound-body
                  bound-id
                  (num (calc named-expr))))]
    [(id x) (error 'calc "free identifier: ~a" x)]))
 
;; run :: s-expr -> number
(define (run prog)
  (calc (parse prog)))

  