#lang play
#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<sym> <expr>} <expr>}
         | <id>
         | {<sym> <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id s)
  (app f-name arg-expr))
 
;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
              | (list <sym> <s-expr>)
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
     (with x (parse e) (parse b))]
    [(list f a) (app f (parse a))]))
 
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
    [(id x) (if (symbol=? x sub-id) val expr)]
    [(app f arg-expr) (app f
                           (subst arg-expr sub-id val))]))
 
;; function definition
(deftype FunDef
  (fundef name arg body))
 
;; lookup-fundef :: sym Listof(FunDef) -> FunDef
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))
 
;; interp :: Expr Listof(FunDef) -> number
(define (interp expr funs)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l funs) (interp r funs))]
    [(sub l r) (- (interp l funs) (interp r funs))]
    [(if0 c t f)
     (if (zero? (interp c funs))
         (interp t funs)
         (interp f funs))]
    [(with bound-id named-expr bound-body)
     (interp (subst bound-body
                  bound-id
                  (num (interp named-expr funs)))
             funs)]
    [(id x) (error 'interp "free identifier: ~a" x)]
    [(app f arg-expr)
     (def (fundef _ farg fbody) (lookup-fundef f funs))
     (interp (subst fbody
                    farg
                    (num (interp arg-expr funs)))
             funs)]))
 
;; run :: s-expr [listof(FunDef)] -> number
(define (run prog [funs '()])
  (interp (parse prog) funs))