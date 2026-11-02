#lang play
#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<sym> <expr>} <expr>} --> syntactic sugar
         | <id>
         | {<expr> <expr>}
         | {fun {<sym>} <expr>}
         | {refun {<sym>} <expr>} // call by ref
         | {newbox <expr>}
         | {openbox <expr>}
         | {setbox <expr> <expr>}
         | {set <sym> <expr>}
         | {seqn <expr> <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id name)
  (app fun-expr arg-expr)
  (fun id body)
  (refun id body)
  (newbox val-expr)
  (openbox box-expr)
  (setbox box-expr val-expr)
  (set id val-expr)
  (seqn expr1 expr2))
 
;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | <sym>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
              | (list 'if0 <s-expr> <s-expr> <s-expr>)
              | (list 'fun (list <sym>) <s-expr>)
              | (list 'refun (list <sym>) <s-expr>)
              | (list 'with (list <sym> <s-expr>) <s-expr>)
              | (list 'newbox <s-expr>)
              | (list 'openbox <s-expr>)
              | (list 'setbox <s-expr> <s-expr>)
              | (list 'set <sym> <s-expr>)
              | (list 'seqn <s-expr> <s-expr>)
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
    [(list 'refun (list x) b) (refun x (parse b))]
    [(list 'newbox e) (newbox (parse e))]
    [(list 'openbox e) (openbox (parse e))]
    [(list 'setbox e1 e2) (setbox (parse e1) (parse e2))]
    [(list 'set id e) (set id (parse e))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list f a) (app (parse f) (parse a))]))
 
;; values
(deftype Val
  (numV n)
  (closureV id body env)
  (refclosV id body env)
  (boxV location))
 
;; Env and Sto as before
 
(deftype Value*Store
  (v*s val sto))
 
;; interp :: Expr Env Sto -> Value*Store
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]
 
    [(fun id body) (v*s (closureV id body env) sto)]
    [(refun id body) (v*s (refclosV id body env) sto)]
 
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num+ l-val r-val) r-sto)]
 
    [(sub l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num- l-val r-val) r-sto)]
 
    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto))
     (if (num-zero? c-val)
         (interp t env c-sto)
         (interp f env c-sto))]
 
    [(id x) (v*s (sto-lookup (env-lookup x env) sto) sto)]
 
    [(app fun-expr arg-expr)
     (def (v*s fun-val fun-sto) (interp fun-expr env sto))
     (match fun-val
      [(closureV id body fenv)
       (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto))
       (def new-loc (next-location arg-sto))
       (interp body
               (extend-env id new-loc fenv)
               (extend-sto new-loc arg-val arg-sto))]
       [(refclosV id body fenv)
        (def loc (env-lookup (id-name arg-expr) env))
        (interp body
                (extend-env id loc fenv)
                fun-sto)])]
 
    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto))
     (interp expr2 env sto1)]
 
    [(newbox val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (def new-loc (next-location val-sto))
     (v*s (boxV new-loc)
          (extend-sto new-loc val-val val-sto))]
 
    [(openbox box-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (v*s (sto-lookup loc box-sto) box-sto)]
 
    [(setbox box-expr val-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (def (v*s val-val val-sto) (interp val-expr env box-sto))
     (v*s val-val
          (extend-sto loc val-val val-sto))]
 
    [(set id val-expr)
     (def loc (env-lookup id env))
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (v*s val-val
          (extend-sto loc val-val val-sto))]))
 
(define (next-location sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ rest) (add1 (next-location rest))]))
 
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))
 
;; run :: s-expr -> number
(define (run prog)
  (match (interp (parse prog) empty-env empty-sto)
    [(v*s (numV n) s) n]
    [(v*s (and b (boxV _)) s) b]
    [(v*s (and f (closureV id body env)) s) f]))