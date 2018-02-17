(require "simpleParser.scm")



; defining a function for variable declaration so that it returns the state after the declaration statement
(define varDeclaration
  (lambda (dec state)
    (cond
      ((null? (cddr dec)) (list (cons (cadr dec) (evaluate (caddr dec) state))))
      (else (append (list (cons (cadr dec) (evaluate (caddr dec) state))) (state))))))

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

; defining a function for assignment so that it returns a state after the assignment
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
      ((eq? var (caar state)) (cdar state))
      ((null? (cdr state)) (error "Variable not declared!"))
      (else (lookupvar var (cdr state))))))

; defining a function for the return statement that returns the value of the expression being returned
(define return
  (lambda (stmt state)
    (cond
      ((null? (cadr stmt)) (error "Nothing to return"))
      (else (append state (list (cons 'return (evaluate (cadr stmt) state))))))))


; defining a function that returns a boolean based on the input statement
(define M_bool
  (lambda (stmt state)
    (cond
      ((null? stmt) (error "Conditional statement needed!"))
      ((eq? (car stmt) '==) (= (evaluate (cadr stmt) state) (evaluate (caddr stmt) state)))
      ((eq? (car stmt) '<) (< (evaluate (cadr stmt) state) (evaluate (caddr stmt) state)))
      ((eq? (car stmt) '>) (> (evaluate (cadr stmt) state) (evaluate (caddr stmt) state)))
      ((eq? (car stmt) '>=) (>= (evaluate (cadr stmt) state) (evaluate (caddr stmt) state)))
      ((eq? (car stmt) '<=) (<= (evaluate (cadr stmt) state) (evaluate (caddr stmt) state)))
      ((eq? (car stmt) '!=) (not (= (evaluate (cadr stmt) state) (evaluate (caddr stmt) state))))
      ((eq? (car stmt) '&&) (and (M_bool (cadr stmt) state) (M_bool (caddr stmt) state)))
      ((eq? (car stmt) '||) (or (M_bool (cadr stmt) state) (M_bool (caddr stmt) state)))
      ((eq? (car stmt) '!) (not (M_bool (cadr stmt) state)))
      (else (error "Invalid conditional statement!")))))

; defining a function that returns a state after an if statement
(define ifstmt
  (lambda (stmt state)
    (cond
      ((M_bool (cadr ifstmt) state) (M_state (caddr ifstmt) state))
      (else (M_state (cadddr ifstmt) state)))))

; defining a function that takes an initial state and a list of statements and returns the final state after runing the statements in the list
(define run
  (lambda (stmtlis state)
    (cond
      ((null? stmtlis) state)
      ((null? (cdr stmtlis)) (M_state (car stmtlis) state))
      (else (run (cdr stmtlis) (M_state (car stmtlis) state))))))

;defining a function that returns a state after a while statement
(define while
  (lambda (stmt state)
    (cond
      ((M_bool (cadr stmt) state) (while stmt (run (cddr stmt) state)))
      (else state))))


;defining a function that returns a state after a statement
(define M_state
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((eq? (car stmt) 'var) (varDeclaration stmt state))
      ((eq? (car stmt) '=) (assignment stmt state))
      ((eq? (car stmt) 'return) (return stmt state))
      ((eq? (car stmt) 'if) (ifstmt stmt state))
      ((eq? (car stmt) 'while) (while stmt state))
      (else (error "Invalid statements")))))
