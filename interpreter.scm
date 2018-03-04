(require "simpleParser.scm")

;--------------------------------------------------------------------------------------------------------
;---------------------------Interpreter Implementation---------------------------------------------------

; definign a function that takes an input file to be executed and returns a value
(define interpret
  (lambda (filename)
    (cond
      ((not (string? filename)) (error "File name must be a string!"))
      (else (lookupvar 'M_state_return (run (parser filename) (M_state_nullState)))))))

; defining a function for variable declaration so that it returns the state after the declaration statement
(define M_state_declaration
  (lambda (dec state)
    (cond
      ((null? (cddr dec)) (M_state_Declaration_updateBinding (cdr dec) state))
      (else (M_state_Declaration_updateBinding (cons (cadr dec) (M_value (caddr dec) state)) state)))))

; defining a function that returns the value of an expression
(define M_value
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((eq? exp '#t) 'true)
      ((eq? exp '#f) 'false)
      ((symbol? exp) (lookupvar exp state))
      ((and (null? (cddr exp)) (eq? (car exp) '-)) (- 0 (M_value (cadr exp) state)))
      ((eq? (car exp) '+) (+ (M_value (cadr exp) state) (M_value (caddr exp) state)))
      ((eq? (car exp) '-) (- (M_value (cadr exp) state) (M_value (caddr exp) state)))
      ((eq? (car exp) '*) (* (M_value (cadr exp) state) (M_value (caddr exp) state)))
      ((eq? (car exp) '/) (quotient (M_value (cadr exp) state) (M_value (caddr exp) state)))
      ((eq? (car exp) '%) (modulo (M_value (cadr exp) state) (M_value (caddr exp) state)))
      ((or (eq? (car exp) '==)
           (or (eq? (car exp) '<)
               (or (eq? (car exp) '>)
                   (or (eq? (car exp) '<=)
                       (or (eq? (car exp) '>=)
                           (or (eq? (car exp) '!=)
                               (or (eq? (car exp) '&&)
                                   (or (eq? (car exp) '||)
                                       (or (eq? (car exp) '!)))))))))) (M_value (M_bool exp state) state))
      (else (error "unknown operator")))))

; defining a function for assignment so that it returns a state after the assignment
(define M_state_assignment
  (lambda (asg state)
    (M_state_Assignment_updateBinding (cons (cadr asg) (M_value (caddr asg) state)) state)))

; defining a function for the return statement that returns the value of the expression being returned
(define M_state_return
  (lambda (stmt state); return)
    (cond
      ((null? (cadr stmt)) (return (error "Nothing to M_state_return")))
      ;(else 'M_state_return_cps (M_value (cadr stmt) state)
      (else (append state (list (cons 'M_state_return (M_value (cadr stmt) state))))))))


; defining a function that returns a boolean based on the input statement
(define M_bool
  (lambda (stmt state)
    (cond
      ((null? stmt) (error "Conditional statement needed!"))
      ((eq? stmt 'true) '#t)
      ((eq? stmt 'false) '#f)
      ((symbol? stmt) (M_bool (lookupvar stmt state) state))
      ((eq? (car stmt) '==) (= (M_value (cadr stmt) state) (M_value (caddr stmt) state)))
      ((eq? (car stmt) '<) (< (M_value (cadr stmt) state) (M_value (caddr stmt) state)))
      ((eq? (car stmt) '>) (> (M_value (cadr stmt) state) (M_value (caddr stmt) state)))
      ((eq? (car stmt) '>=) (>= (M_value (cadr stmt) state) (M_value (caddr stmt) state)))
      ((eq? (car stmt) '<=) (<= (M_value (cadr stmt) state) (M_value (caddr stmt) state)))
      ((eq? (car stmt) '!=) (not (= (M_value (cadr stmt) state) (M_value (caddr stmt) state))))
      ((eq? (car stmt) '&&) (and (M_bool (cadr stmt) state) (M_bool (caddr stmt) state)))
      ((eq? (car stmt) '||) (or (M_bool (cadr stmt) state) (M_bool (caddr stmt) state)))
      ((eq? (car stmt) '!) (not (M_bool (cadr stmt) state)))
      (else (error "Invalid conditional statement!")))))

; defining a function that returns a state after an if statement
(define M_state_if
  (lambda (stmt state)
    (cond
      ((M_bool (cadr stmt) state) (M_state (caddr stmt) state))
      ((null? (cdddr stmt)) state)
      (else (M_state (cadddr stmt) state)))))

; defining a function that takes an initial state and a list of statements and returns the final state after runing the statements in the list
(define run
  (lambda (stmtlis state)
    (cond
      ((null? stmtlis) state)
      ((null? (cdr stmtlis)) (M_state (car stmtlis) state))
      (else (run (cdr stmtlis) (M_state (car stmtlis) state))))))

;defining a function that returns a state after a while statement
(define M_state_while
  (lambda (stmt state)
    (cond
      ((M_bool (cadr stmt) state) (M_state_while stmt (run (cddr stmt) state)))
      (else state))))

; defining a function that returns a state after a block
(define M_state_block
  (lambda (body state)
    (cond
      ((null? body) state)
      (else (M_state_removeLayer (run body (M_state_addLayer state)))))))


;defining a function that returns a state after a statement
(define M_state
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((eq? (car stmt) 'var) (M_state_declaration stmt state))
      ((eq? (car stmt) '=) (M_state_assignment stmt state))
      ((eq? (car stmt) 'return) (M_state_return stmt state))
      ((eq? (car stmt) 'if) (M_state_if stmt state))
      ((eq? (car stmt) 'while) (M_state_while stmt state))
      ;implement try
      ;implement catch
      ;implement finally
      ;implement begin (block)
         ;for each begin, cons a new list to the existing binding list
         ;if the begin ends, remove the (car list) of the current binding list
      (else (error "Invalid statements")))))

;----------------------------------------------------------------------------------------------------
;------------------------------------State Implementation--------------------------------------------
; the following are functions written to hide state implementation from the rest of interpreter

; This implementation of the state is a simple list of pairs, each pair contains a variable name and its value

; wrapper
(define M_state_Declaration_updateBinding_layers_wrapper
  (lambda (binding state layers)
    (call/cc
     (lambda (break)
       (M_state_Declaration_updateBinding_layers binding state layers break)))))

; defining a function that updates the bindings among all the layers
(define M_state_Declaration_updateBinding_layers
  (lambda (binding state layers break)
    (cond
      ((and (null? (M_state_topLayer layers)) (null? (M_state_previousLayers layers))) (break (list (cons binding (M_state_topLayer state)))))
      ((and (null? (M_state_topLayer layers)) (not (null? (M_state_previousLayers layers)))) (M_state_Declaration_updateBinding_layers binding state (M_state_previousLayers layers) break))
      ((M_state_Declaration_updateBinding_single binding (M_state_topLayer layers)) (break (error "Variable already declared")))
      ((and (not (M_state_Declaration_updateBinding_single binding (M_state_topLayer layers))) (null? (M_state_previousLayers layers))) (break (list (cons (car binding) (M_state_topLayer state)))))
      (else (M_state_Declaration_updateBinding_layers binding state (M_state_previousLayers state) break)))))
      
; defining a function that updates the bindings within a single layer
(define M_state_Declaration_updateBinding_single
  (lambda (binding layer)
    (cond
      ((null? layer) #f)
      ((eq? (car binding) (caar layer)) #t)
      ((not (null? (cdr layer))) (M_state_Declaration_updateBinding_single binding (cdr layer)))
      (else #f))))

;defining a function that updates the bindings in a given state in a assignment statement
;need to update the first layer first
(define M_state_Assignment_updateBinding
  (lambda (binding state)
    (cond
      ((null? state) (error "Variable not declared"))
      ((eq? (car binding) (caar state)) (append (list binding) (cdr state)))
      ((not (null? (cdr state))) (append (list (car state)) (M_state_Assignment_updateBinding binding (cdr state))))
      (else (error "Variable not declared")))))

; defining a function that takes no input and returns an empty state
(define M_state_nullState
  (lambda () '(())))

; defining a function that adds a layer to the bindings
(define M_state_addLayer
  (lambda (state)
    (cond
      ((null? state) M_state_nullState)
      (else (list '() state)))))

; defining a function that removes a layer from the bindings
(define M_state_removeLayer
  (lambda (state)
    (cond
      ((null? state) M_state_nullState)
      (else (cdr state)))))

; defining a function that returns the layer corresponding to the current scope
(define M_state_topLayer
  (lambda (state)
    (car state)))

; defining a function that returns the remaining layers of the current state
(define M_state_previousLayers
  (lambda (state)
    (cdr state)))

; defining a function that returns a value of a variable if initialized or an error message if not
;need to first look up first layer(car list), then second layer ...
(define lookupvar
  (lambda (var state)
    (cond
      ((null? state) (error "Variable not declared!"))
      ((and (eq? var (caar state)) (null? (cdar state))) (error "Variable not initialized!"))
      ((eq? var (caar state)) (cdar state))
      ((null? (cdr state)) (error "Variable not declared!"))
      (else (lookupvar var (cdr state))))))

