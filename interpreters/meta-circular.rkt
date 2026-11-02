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
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id s)
  (app fun-expr arg-expr)
  (fun id body))
 
;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
              | (list 'fun (list <sym>) <s-expr>)
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
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]))
 
;; interp :: Expr Env -> number/procedure
(define (interp expr env)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l env) (interp r env))]
    [(sub l r) (- (interp l env) (interp r env))]
    [(if0 c t f)
     (if (zero? (interp c env))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
 
    [(fun id body) (λ (arg-val)
                     (interp body
                             (extend-env id arg-val env)))]
 
    [(app fun-expr arg-expr)
     ((interp fun-expr env) (interp arg-expr env))]))
 
;; run :: s-expr -> number
(define (run prog)
  (interp (parse prog) empty-env))