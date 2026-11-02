#lang play
(require "enviroment-adt.rkt")
#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<sym> <expr>} <expr>} --> syntactic sugar
         | <id>
         | {<expr> <expr>}
         | {fun {<sym>} <expr>}
         | {rec {<sym> <expr>} <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id s)
  (app fun-expr arg-expr)
  (fun id body)
  (rec id named-expr body))
 
;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
              | (list 'fun (list <sym>) <s-expr>)
              | (list 'rec (list <sym> <s-expr>) <s-expr>)
              | (list <s-expr> <s-expr>)
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
     (app (fun x (parse b)) (parse e))]
    [(list 'rec (list x e) b)
     (rec x (parse e) (parse b))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]))
 
;; values
(deftype Val
  (numV n)
  (closureV id body env))
 
 
#|-----------------------------
Environment abstract data type
(updated with recursive case)
 
empty-env  :: Env
env-lookup :: Sym Env -> Val
extend-env :: Sym Val Env -> Env
rec-extend-env :: Sym Box(Val) Env -> Env
 
|#
(deftype Env
  (mtEnv)
  (aEnv id val env)
  (aRecEnv id bval env))
 
(def empty-env  (mtEnv))
(def extend-env aEnv)
(def rec-extend-env aRecEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]
    [(aRecEnv id val rest)
     (if (symbol=? id x)
         (unbox val)
         (env-lookup x rest))]))
;;;;;;;
 
 
;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(add l r) (num+ (interp l env) (interp r env))]
    [(sub l r) (num- (interp l env) (interp r env))]
    [(if0 c t f)
     (if (num-zero? (interp c env))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(app fun-expr arg-expr)
     (def (closureV id body fenv) (interp fun-expr env))
     (interp body
             (extend-env id
                         (interp arg-expr env)
                         fenv))]
    [(rec id named-expr body)
       (interp body
               (cyclic-env id named-expr env))]))
 
;; cyclic-env:: id expr env -> env (assumption: expr is a fun)
(define (cyclic-env id expr env)
  (def value-holder (box 'unspecified))
  (def new-env (rec-extend-env id value-holder env))
  (def fun-val (interp expr new-env))
  (set-box! value-holder fun-val)
  new-env)
 
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))
 
;; run :: s-expr -> number
(define (run prog)
  (match (interp (parse prog) empty-env)
    [(numV n) n]
    [(and f (closureV id body env)) f]))