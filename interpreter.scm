(require "simpleParser.scm")

; defining a function for variable declaration
(define varDeclaration
  (lambda (dec)
    (cond
      ((null? (cddr dec)) (cdr dec))
      (else (cdr dec)))))



; defining a function for assignment
(define assignment
  (lambda (asg state)
    (cond
      ((null? state) (cdr asg))
      ((eq? (caar state) (cadr asg)) (append (list (cdr asg)) (cdr state)))
      ((not (null? (cdr state))) (append (list(car state)) (assignment asg (cdr state))))
      (else (error "Variable not declared yet!")))))

; defining a function for return