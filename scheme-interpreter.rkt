#lang racket

(define (add-to-E E key val)
  (hash-set! (car E) key val))

(define (self-evaluating? expr)
  (or (boolean? expr) (number? expr) (char? expr) (string? expr) (procedure? expr) ))


(define (is-var var E)
  (if (self-evaluating? var) var
      (is-var (hash-ref (car E) var) E)))
(define (search-value var E)
  (cond [(null? E) raise-user-error "The variable is undefined!"]
        [(hash-has-key? (car E) var) (is-var var E)]
        [else (search-value var (cdr E))]))

(define (op-or-var? var E)
  (hash-has-key? (car E) var))

(define (eval-sexpr expr E)
  (define (evaluate expr)
    (cond [(self-evaluating? expr) expr]
          [(op-or-var? expr E) (search-value expr E)]
          [else (let ((op (car expr))
                      (args (cdr expr)))
                  (apply (evaluate op)
                         (map evaluate args)))]))
  (evaluate expr))

(define (if? expr)
  (and (list? expr) (eq? (car expr) 'if)))

(define (eval-if expr E)
  (let ((bool-expr (car (cdr expr)))
        (then (car (cdr (cdr expr))))
        (else (car (cdr (cdr (cdr expr))))))
    (if (eval-sexpr bool-expr E) (eval-sexpr then E)
        (eval-sexpr else E))))

(define (cond? expr)
  (and (list? expr) (eq? (car expr) 'cond)))

(define (eval-cond expr E)
  (cond [(null? (cdr expr)) (evall (cdr (car expr)) E)]
        [(evall (car (car expr)) E) (evall (cdr (car expr)) E)]
        [else (eval-cond (cdr expr) E)]))

(define (define? expr)
  (and (list? expr) (eq? (car expr) 'define)))

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))


(define (eval-define expr E)
  (if (atom? (car (cdr expr))) (add-to-E E (car (cdr expr)) (car (cdr (cdr expr))))
      #t))

(define (lambda? expr)
  (and (list? expr) (or (eq? (car expr) 'lambda) (eq? (car (car expr)) 'lambda))))

(define (eval-seq expr E)
  (cond [(null? (cdr expr)) (evall (car expr) E)]
        [else (evall (car expr) E)
              (eval-seq (cdr expr) E)]))

(define (extend-E  vars vals base-E)
  (add-to-E base-E vars vals))

(define (eval-lambda expr E)
  (let* ((params (car (car (cdr (car expr)))))
        (body (cdr (cdr (car expr))))
        (args (car (cdr expr)))
        (extension (extend-E params args E)))
    (eval-seq body E)))

(define (evall expr E)
  (cond [(self-evaluating? (car expr)) (car expr)]
        [(op-or-var? (car expr) E) (eval-sexpr expr E)]
        [(if? expr) (eval-if expr E)]
        [(cond? expr) (eval-cond (cdr expr) E)]
        [(define? expr) (eval-define expr E)]
        [(lambda? expr)
         (if (self-evaluating? (car (cdr expr))) (eval-lambda expr E)
             #f)]
        [else (raise-user-error "Rewrite your S-Expression and try again!")]))

(define (make-E p)
  (cons (make-hash) p))

(define E (make-E '()))
  
(add-to-E E '+ +)
(add-to-E E '- -)
(add-to-E E '* *)
(add-to-E E '/ /)
(add-to-E E 'abs abs)
(add-to-E E '< <)
(add-to-E E '<= <=)
(add-to-E E '> >)
(add-to-E E '>= >=)
(add-to-E E '= =)
(add-to-E E 'cons cons)
(add-to-E E 'list list)
(add-to-E E 'car car)
(add-to-E E 'cdr cdr)
(add-to-E E 'true true)
(add-to-E E '#t #t)
(add-to-E E 'false false)
(add-to-E E '#f #f)
(add-to-E E 'null? null?)
(add-to-E E 'number? number?)

(define (interpret code)
  (apply (lambda (proc) (evall proc E)) code))