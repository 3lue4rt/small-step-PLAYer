#lang play
(require "enviroment-adt.rkt")
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
 
;; interp :: Expr Listof(FunDef) Env -> number
(define (interp expr funs env)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l funs env) (interp r funs env))]
    [(sub l r) (- (interp l funs env) (interp r funs env))]
    [(if0 c t f)
     (if (zero? (interp c funs env))
         (interp t funs env)
         (interp f funs env))]
    [(with bound-id named-expr bound-body)
     (interp bound-body
             funs
             (extend-env bound-id
                         (interp named-expr funs env)
                         env))]
    [(id x) (env-lookup x env)]
    [(app f arg-expr)
     (def (fundef _ farg fbody) (lookup-fundef f funs))
     (interp fbody
             funs
             (extend-env farg
                         (interp arg-expr funs env)
                         empty-env))]))
 
;; run :: s-expr [listof(FunDef)] -> number
(define (run prog [funs '()])
  (interp (parse prog) funs empty-env))