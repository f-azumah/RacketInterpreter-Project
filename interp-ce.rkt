#lang racket

;; Project 3: Metacircular interpreter for Scheme
;; CIS352 -- Fall 2022

(provide interp-ce)

; Your task is to write a CE interpreter for a substantial subset of Scheme/Racket. 
; A CE interpreter is meta-circular to a large degree (e.g., a conditional in the target
; language (scheme-ir?) can be implemented using a conditional in the host language (Racket),
; recursive evaluation of a sub-expression can be implemented as a recursive call to the
; interpreter, however, it's characterized by creating an explicit closure value for lambdas
; that saves its static environment (the environment when it's defined). For example, a CE
; interpreter for the lambda calculus may be defined:
(define (interp-ce-lambda exp [env (hash)])
  (match exp
         [`(lambda (,x) ,body)
          ; Return a closure that pairs the code and current (definition) environment
          `(closure (lambda (,x) ,body) ,env)]
         [`(,efun ,earg)
          ; Evaluate both sub-expressions
          (define vfun (interp-ce-lambda efun env))  
          (define varg (interp-ce-lambda earg env))
          ; the applied function must be a closure
          (match-define `(closure (lambda (,x) ,body) ,env+) vfun)
          ; we extend the *closure's environment* and interp the body
          (interp-ce-lambda body (hash-set env+ x varg))]
         [(? symbol? x)
          ; Look up a variable in the current environment
          (hash-ref env x)]))

; Following is a predicate for the target language you must support. You must support any
; syntax allowed by scheme-ir that runs without error in Racket, returning a correct value..
(define (scheme-ir? exp)
  ; You should support a few built-in functions bound to the following variables at the top-level
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (match exp
         [`(lambda ,(? (listof symbol?)) ,(? scheme-ir?)) #t] ; fixed arguments lambda
         [`(lambda ,(? symbol?) ,(? scheme-ir?)) #t] ; variable argument lambda
         [`(if ,(? scheme-ir?) ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [`(let ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(let* ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(and ,(? scheme-ir?) ...) #t]
         [`(or ,(? scheme-ir?) ...) #t]
         [`(apply ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [(? (listof scheme-ir?)) #t]
         [(? prim?) #t]
         [(? symbol?) #t]
         [(? number?) #t]
         [(? boolean?) #t]
         [''() #t]
         [_ #f]))

; Interp-ce must correctly interpret any valid scheme-ir program and yield the same value
; as DrRacket, except for closures which must be represented as `(closure ,lambda ,environment).
; (+ 1 2) can return 3 and (cons 1 (cons 2 '())) can yield '(1 2). For programs that result in a 
; runtime error, you should return `(error ,message)---giving some reasonable string error message.
; Handling errors and some trickier cases will give bonus points. 
(define (interp-ce exp)
  ; Might add helpers or other code here...             
  (define (interp exp env) 
    (match exp
      [(? symbol? x) (hash-ref env x)]
      
      [`(lambda ,args ,body) 
       ;; closures must be represented this way
       `(closure ,exp ,env)]
      
      [`(lambda (,xs ...) ,e)
       `(closure (lambda (,@xs) ,e) ,env)]

      #;[`((lambda ,args ,body) ,eargs ...)
       (match eargs
         [`(lambda ,args ,body) `(closure (lambda ,args ,body) , env)]
         [`((lambda ,args ,body) . ,rest)
          (let ([inner-closure `(closure (lambda ,args ,body) ,env)])
            (cons inner-closure (interp env rest)))])]
      
      [`(if ,e-g ,e-t ,e-f)
       (if (interp e-g env)
           (interp e-t env)
           (interp e-f env))]
      
      [`(let ([,x ,e0]) ,e-body)
       (define new-env (hash-set env x (interp e0 env)))
       (interp e-body new-env)]
      
      [`(let ([,xs ,es] ...) ,e-body)
       (define vs (map (lambda (e) (interp e env)) es))
       (define env+
         (foldl (lambda (x v env+) (hash-set env+ x v)) env xs vs))
       (interp e-body env+)]
      
      [`(let* () ,e-body) (interp e-body env)]
      
      [`(let* ([,x ,e0]) ,e-body)
       (interp e-body (hash-set env x (interp e0 env)))]
      
      [`(let* ([,xs ,es] ,b ...) ,e-body)
       (define env+
         (foldl (lambda (b env)
                  (match b
                    [`(,x ,e)
                     (hash-set env x (interp e env))]))
                env
                `((,xs ,es) ,@b)))
       (interp e-body env+)]
      
      [`(and) #t] 
      [`(and ,e0 ,e-rest ...)
       (if (interp e0 env)
           (interp `(and ,@e-rest) env)
           #f)]
      
      [`(or) #f]
      [`(or ,es ,e-rest ...)
       (or (interp es env)
           (interp `(or ,@e-rest) env))]
      [(? number? n) n]
      [(? boolean? b) b]
      [`(list ,@es) (map (lambda (e) (interp e env)) es)]
      [`(cons ,e0 ,e1)
       (define v0 (interp e0 env))
       (define v1 (interp e1 env))
       (cons v0 v1)]
      [`(car ,e0) (car (interp e0 env))]
      [`(cdr ,e0) (cdr (interp e0 env))]
      [`(null? ,e0) (null? (interp e0 env))]
      [''() '()]
      [`(,ef ,earg0, earg1)
       (match (interp ef env)
         [`(closure (lambda (,x ,y) ,e-body) ,env+)
          (define env++
            (hash-set (hash-set env+ x (interp earg0 env)) y (interp earg1 env)))
          (interp e-body env++)]
         [`(closure (lambda ,xs ,e-body) ,env+)
          (define env++
            (foldl (lambda (x v env)
                     (hash-set env x v))
                   env+
                   xs
                   (list (interp earg0 env) (interp earg1 env))))
          (interp e-body env++)]
         [(? procedure? p)
          (apply p (list (interp earg0 env)(interp earg1 env)))])]
      [`(,ef ,eargs ...)
       (match (interp ef env)
         [`(closure (lambda ,xs ,e-body) ,env+)
          (define vs (map (lambda (e) (interp e env)) eargs))
          (interp e-body (foldl (lambda (x v env)
                                  (hash-set env x v))
                                env+
                                xs
                                vs))]
         [(? procedure? p)
          (apply p (map (lambda (e) (interp e env)) eargs))])]))
        
  ;; you need to cook up a starting environment: at first it can just
  ;; be the empty hash, but later on you may want to add things like
  ;; builtins here.
  (define starting-env 
    (hash '+ +
          '- -
          '* *
          '= =
          'cons cons
          'car car
          'cdr cdr
          'list list
          'null? null?
          'equal? equal?))
  (interp exp starting-env))
