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
      ((eq? (caar state) (cdar asg)) (append (cdr asg) (cdr state)))
      ((not (null? (cdr state))) (append (car state) (assign (asg (cdr state)))))
      (else (error "Variable not declared yet!")))))

; defining a function for return