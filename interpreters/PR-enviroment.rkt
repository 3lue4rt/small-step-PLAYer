#lang play
#|-----------------------------
Environment abstract data type
 
empty-env  :: Env
extend-env :: Sym Val Env -> Env
env-lookup :: Sym Env -> Val
 
Procedural representation:
an environment is a function Sym -> Val
|#
 
(def empty-env
  (λ (s) (error "free identifier: ~a" s)))
 
(define (env-lookup x env)
  (env x))
 
(define (extend-env s v env)
  (λ (id)
    (if (symbol=? id s)
        v
        (env id))))