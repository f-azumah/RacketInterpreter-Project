#lang racket

;; Project 3: Metacircular interpreter for Scheme
;; CIS352 -- Fall 2022

(provide interp-ce)

; Following is a predicate for the my language.
(define (scheme-ir? exp)
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

; Interp-ce interprets any valid scheme-ir program and yields the same value
; as DrRacket, except for closures which are represented as `(closure ,lambda ,environment).

(define (interp-ce exp)
  ; Might add helpers or other code here...             
  (define (interp exp env) 
    (match exp
      [(? symbol? x) (hash-ref env x)]
      
      [`(lambda ,args ,body) 
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
        
  ;; starting environment: 
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
