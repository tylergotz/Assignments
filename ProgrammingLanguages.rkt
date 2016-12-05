;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |day 11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define listOfOperators '(+ - * /))

(define contains
  (lambda (val lst)
    (if (null? lst)
        #f
        (if (eq? (car lst) val)
            #t
            (contains val (cdr lst))))))

(define empty-env
  (lambda () '()))

(define empty-scope
  (lambda () '()))

(define extend-scope
  (lambda (var val scope)
    (cons (list var val) scope)))

(define extend-env
  (lambda (scope env)
    (cons scope env)))

(define apply-scope
  (lambda (var scope)
    (cond
      ((null? scope) #f)
      ((eq? (caar scope) var) (cadar scope))
      (else (apply-scope var (cdr scope))))))

(define apply-env
  (lambda (var env)
    (cond
      ((null? env) #f)
      (else (let ((resolved (apply-scope var (car env))))
        (if (eq? resolved #f)
            (apply-env var (cdr env))
            resolved))))))
      
(define var-exp
  (lambda (id) id))

(define lit-exp
  (lambda (lit) lit))

(define lambda-exp
  (lambda (vexp lexp)
    (list 'lambda vexp lexp)))

(define app-exp 
  (lambda (rator rands)
    (append (list rator) rands)))

(define op?
  (lambda (s)
    (contains s listOfOperators)))

(define parse-exp
  (lambda (lcExp)
    (cond
      ((number? lcExp) (list 'lit-exp lcExp))
      ((symbol? lcExp) (if (op? lcExp)
                           (list 'op-exp lcExp)
                           (list 'var-exp lcExp)))
      ((eq? (car lcExp) 'lambda)
       (list 'lambda-exp
            (cadr lcExp)
            (parse-exp (caddr lcExp))))
      (else (cons 'app-exp (append (list (parse-exp (car lcExp)))
                  (map parse-exp (cdr lcExp))))))))

(define unparse-exp
  (lambda (lcStruct)
    (cond
      ((eq? (car lcStruct) 'lit-exp) (lit-exp (cadr lcStruct)))
      ((eq? (car lcStruct) 'var-exp)  (var-exp (cadr lcStruct)))
      ((eq? (car lcStruct) 'lambda-exp) (lambda-exp
                                        (list-ref lcStruct 1)
                                        (unparse-exp (list-ref lcStruct 2))))
      (else
      (app-exp (unparse-exp (cadr lcStruct))
               (map unparse-exp (caddr lcStruct)))))))


(define extend-env-4-lambda-helper
  (lambda (lovars lovals scope)
    (cond
      ((not (null? lovars)) (extend-env-4-lambda-helper
                           (cdr lovars)
                           (cdr lovals)
                           (extend-scope (car lovars) (car lovals) scope)))
    (else scope))))

(define extend-env-4-lambda
  (lambda (lovars lovals env)
    (extend-env
     (extend-env-4-lambda-helper lovars lovals (empty-scope)) env)))

(define eval-op-exp
  (lambda (appExp env)
    (let ((op1 (eval-exp (cadr appExp) env))
          (op2 (eval-exp (caddr appExp) env))
          (theOp (cadar appExp)))
      (cond
        ((eq? theOp '+) (+ op1 op2))
        ((eq? theOp '-) (- op1 op2))
        ((eq? theOp '*) (* op1 op2))
        ((rq? theOp '/) (/ op1 op2))
        (else
         #f)))))
    
(define eval-exp
  (lambda (lce env)
    (cond
      ((eq? (car lce) 'lit-exp) (cadr lce))
      ((eq? (car lce) 'var-exp)
       (apply-env (cadr lce) env))
      ((eq? (car lce) 'lambda-exp)
       (eval-exp (caddr lce) env))
      (else
       (cond
         ((eq? (list-ref (list-ref lce 1) 0) 'lambda-exp)
           (eval-exp (list-ref (list-ref lce 1) 2)
                     (extend-env-4-lambda
                      (list-ref (list-ref lce 1) 1)
                      (map (lambda (x)
                             (if (eq? (car x) 'lambda-exp)
                                 x
                                 (eval-exp x env))) (cddr lce)) env)))
          ((eq? (list-ref (list-ref lce 1) 0) 'op-exp)
           (eval-op-exp (cdr lce) env))
          (else
           (let ((theLambda (eval-exp (list-ref lce 1) env))
                 (theInputs (map (lambda (x)
                    (if (eq? (car x) 'lambda-exp)
                        x
                        (eval-exp x env))) (cddr lce))))
             (eval-exp theLambda (extend-env-4-lambda (list-ref theLambda 1)
                                                      theInputs
                                                      env))))))))
             
                    
           
(define run-program
  (lambda (lce)
    (eval-exp lce (empty-env))))