#lang racket
(require syntax/parse/define
         chk)

(struct Num (n) #:transparent) 
(struct BinOp (op L R) #:transparent)
(struct Bool (b) #:transparent) 


(struct Context0 (if C et ef) #:transparent)
(struct Context1 (if et C ef) #:transparent)
(struct Context2 (if et ef C) #:transparent)

;(struct Context (e1 e2 e3) #:transparent)

(struct if () #:transparent)

(define (desugar sexp)
  (match sexp
    [(? number? n)(Num n)]
    [(list op L R)
     (BinOp op (desugar L) (desugar R))]));recursively call desugar


(define (make-arith-op op)
  (lambda (L R)
    (define L-v (Num-n L))
    (define R-v (Num-n R))
    (define result (op L-v R-v))
    (Num result)))

(define (make-bool-op op)
  (lambda (L R)
    (define L-v (Num-n L))
    (define R-v (Num-n R))
    (define result (op L-v R-v))
    (Bool result)))

(define bin-op
  (hash '+ (make-arith-op +)
        '- (make-arith-op -)
        '* (make-arith-op *)
        '< (make-bool-op <)
        '> (make-bool-op >)
        '>= (make-bool-op >=)
        '<= (make-bool-op <=)
        ))

(define (eval exp)
  (match exp
    [(Num n) (Num n)]
    [ (Bool b) (Bool b)]
    [(BinOp op L R)
     (define op-fn (hash-ref bin-op op))
     (op-fn (eval L) (eval R))])
    
  )

(define-simple-macro (test1 exp ans)
  (chk (#:src #' exp (eval (desugar exp)))
       (#:src #' ans (eval (desugar ans)))))

(define-simple-macro (test2 exp ans)
  (chk (#:src #' exp (eval (desugar exp)))
       (#:src #' exp (eval (desugar ans)))))
                      
(module+ test1
  (test1 5 5)
  (test1 (+ 1 2) 3)
  (test1 (+ 1 2) (+ 2 1))

  )
(module+ test2
  (test2 (< 1 2) #t)
  (test2 (> 1 2) #f)
  (test2 (>= 2 2) #t)
  (test2 (>= 3 2) #t)
  


  )



  
