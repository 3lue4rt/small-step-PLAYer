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
 
;; values
(deftype Val
  (numV n)
  (closureV id body env)
  (exprV expr env cache))
 
;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(add l r) (num+ (strict (interp l env)) (strict (interp r env)))]
    [(sub l r) (num- (strict (interp l env)) (strict (interp r env)))]
    [(if0 c t f)
     (if (num-zero? (strict (interp c env)))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(app fun-expr arg-expr)
     (def (closureV id body fenv) (strict (interp fun-expr env)))
     (interp body
             (extend-env id
                         (exprV arg-expr env (box #f))
                         fenv))]))
 
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))
 
(define (strict e)
  (match e
    [(exprV expr env cache)
     (if (not (unbox cache))
         (local ([def val (strict (interp expr env))])
           (printf "Forcing exprV to ~v~n" val)
           (set-box! cache val)
           val)
         (begin
           (printf "Using cached value~v~n" (unbox cache))
           (unbox cache)))]
    [else e]))
 
;; run :: s-expr -> number
(define (run prog)
  (match (strict (interp (parse prog) empty-env))
    [(numV n) n]
    [(and f (closureV id body env)) f]))