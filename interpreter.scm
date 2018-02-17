(require "simpleParser.scm")

; TODO: Update the assignment function to be able to assign the value of an expression to a variable


; defining a function for variable declaration
(define varDeclaration
  (lambda (dec state)
    (cond
      ((null? (cddr dec)) (cdr dec))
      (else (cons (cadr dec) (evaluate (caddr dec) state))))))

;defining a function that returns the value of an expression
(define evaluate
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((symbol? exp) (lookupvar exp state))
      ((eq? (car exp) '+) (+ (evaluate (cadr exp) state) (evaluate (caddr exp) state)))
      ((eq? (car exp) '-) (- (evaluate (cadr exp) state) (evaluate (caddr exp) state)))
      ((eq? (car exp) '*) (* (evaluate (cadr exp) state) (evaluate (caddr exp) state)))
      ((eq? (car exp) '/) (/ (evaluate (cadr exp) state) (evaluate (caddr exp) state)))
      (else (error "unknown operator")))))

; defining a function for assignment
(define assignment
  (lambda (asg state)
    (cond
      ((null? state) (error "Variable not declared yet"))
      ((eq? (caar state) (cadr asg)) (append (list (cons (cadr asg) (evaluate (caddr asg) state))) (cdr state)))
      ((not (null? (cdr state))) (append (list(car state)) (assignment asg (cdr state))))
      (else (error "Variable not declared yet!")))))

; defining a function that returns a value of a variable if initialized or an error message if not
(define lookupvar
  (lambda (var state)
    (cond
      ((null? state) (error "Variable not declared!"))
      ((and (eq? var (caar state)) (null? (cdar state))) (error "Variable not initialized!"))
      ((eq? var (caar state)) (cadar state))
      ((null? (cdr state)) (error "Variable not declared!"))
      (else (lookupvar var (cdr state))))))