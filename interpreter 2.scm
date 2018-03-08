(require "simpleParser.scm")

;--------------------------------------------------------------------------------------------------------
;---------------------------Interpreter Implementation---------------------------------------------------

; definign a function that takes an input file to be executed and returns a value
(define interpret
  (lambda (filename)
    (cond
      ((not (string? filename)) (error "File name must be a string!"))
      (else (lookupvar 'M_state_return 
          (call/cc (lambda (return) (run (parser filename) M_state_nullState return '() '()))))))))


;; abstractions
(define addlayer cons)
(define emptyLayer '())
(define poplayer cdr)
(define topLayer car)
(define key car)
(define value cadr)
(define M_state_nullState '(()))
(define addBinding cons)
(define bind list)

; defining a function for variable declaration so that it returns the state after the declaration statement
(define M_state_declaration
  (lambda (dec state)
    (cond
      ((null? (cddr dec)) (M_state_Declaration_updateBinding (cdr dec) state))
      (else (M_state_Declaration_updateBinding (bind (cadr dec) (M_value (caddr dec) state)) state)))))

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
    (M_state_Assignment_updateBinding (bind (cadr asg) (M_value (caddr asg) state)) state)))

; defining a function for the return whileReturn throwReturn statement that returns the value of the expression being returned
(define M_state_return
  (lambda (stmt state)
    (cond
      ((null? (cadr stmt)) (error "Nothing to M_state_return"))
      (else (M_state_Declaration_updateBinding (bind 'M_state_return (M_value (cadr stmt) state)) state)))))

; defining a function for throw so that returns a state after throw
(define M_state_throw
  (lambda (stmt state)
    (cond
      ((null? (cadr stmt)) (error "Nothing to M_state_throw"))
      (else (M_state_Assignment_updateBinding (bind 'throw (M_value (cadr stmt) state)) state)))))


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
  (lambda (stmt state return whileReturn throwReturn)
    (cond
      ((M_bool (cadr stmt) state) (M_state (caddr stmt) state return whileReturn throwReturn))
      ((null? (cdddr stmt)) state)
      (else (M_state (cadddr stmt) state return whileReturn throwReturn)))))

; defining a function that takes an initial state and a list of statements and returns the final state after runing the statements in the list
(define run 
  (lambda (stmtlis state return whileReturn throwReturn)
    (cond
      ((null? stmtlis) state)
      ((null? (cdr stmtlis)) (M_state (car stmtlis) state return whileReturn throwReturn))
      (else (run (cdr stmtlis) (M_state (car stmtlis) state return whileReturn throwReturn) return whileReturn throwReturn)))))



;defining a function that returns a state after a while statement
(define M_state_while
  (lambda (stmt state return whileReturn throwReturn)
    (cond
      ((definedInTopBinding (bind 'gotype 'break) state) state)
      ((M_bool (cadr stmt) state) (M_state_while stmt (run (cddr stmt) state return whileReturn throwReturn) return whileReturn throwReturn ))
      (else state))))


;defining a function that returns a state after a statement
(define M_state
  (lambda (stmt state return whileReturn throwReturn)
    (cond
      ((null? stmt) state)
      ((eq? (car stmt) 'var) (M_state_declaration stmt state))
      ((eq? (car stmt) '=) (M_state_assignment stmt state))
      ((eq? (car stmt) 'return) (return (M_state_return stmt state)))
      ((eq? (car stmt) 'throw)  (if (null? throwReturn) (error "Error: throw not in try block") (throwReturn (M_state_throw stmt state ))))
      ((eq? (car stmt) 'if) (M_state_if stmt state return whileReturn throwReturn))
      ((eq? (car stmt) 'while) (M_state_while stmt (M_state_Declaration_updateBinding (bind 'gotype 0) state) return whileReturn throwReturn))
      ((eq? (car stmt) 'begin)  (poplayer (call/cc (lambda (whileReturn) (run (cdr stmt) (addlayer emptyLayer state) return whileReturn throwReturn)))))
      ((eq? (car stmt) 'continue) (whileReturn (M_state_Assignment_updateBinding (bind 'gotype 'continue) state)))
      ((eq? (car stmt) 'break) (whileReturn (M_state_Assignment_updateBinding (bind 'gotype 'break) state)))
      ((eq? (car stmt) 'try) (M_state_try stmt state return whileReturn throwReturn))
      (else (error "Invalid statements")))))

; abstraction
(define tryBody cadr)
(define catchBody caddr)
(define finalBody cadddr)

; defining a function for catch so that returns a state after catch
(define (M_state_catch stmt state return whileReturn throwReturn)
     (if (null? stmt) state
        (if (equal? (lookupvar 'throw state) 'none)
            state
            (run (catchBody stmt) 
                (M_state_Declaration_updateBinding (bind (catchVar stmt) (lookupvar 'throw state)) state)
                return whileReturn throwReturn))))

; defining a function for finally so that returns a state after finally
(define (M_state_final stmt state return whileReturn throwReturn)
      (if (null? stmt) state
        (run (finalStmt stmt) state return whileReturn throwReturn)))

; abstraction
(define finalStmt cadr)
(define catchVar caadr)
(define catchStmt caddr)

; defining a function for try statement so that it returns a state after try statement
(define (M_state_try stmt state return whileReturn throwReturn)
    (M_state_final (finalBody stmt)
    (M_state_catch (catchBody stmt)   (call/cc (lambda (throwReturn) 
                   (run (tryBody stmt) (M_state_Declaration_updateBinding (bind 'throw 'none) state) return whileReturn throwReturn)))
                   return whileReturn throwReturn)
             return whileReturn throwReturn))




;----------------------------------------------------------------------------------------------------
;------------------------------------State Implementation--------------------------------------------
; the following are functions written to hide state implementation from the rest of interpreter

; This implementation of the state is a list of list of pairs, each list of pairs is a layer,
; each pair contains a variable name and its value

; defining a function that updates the bindings in a given state in a delaration statement
(define 
    (M_state_Declaration_updateBinding binding state)
    (cond
       ((assq (key binding) (topLayer state)) (error "Variable already declared"))
       (else (cons (addBinding binding (topLayer state)) (cdr state)))))

; defining a function that updates the bindings in a given state in a assignment statement
(define (M_state_Assignment_updateBinding binding state)
    (cond
       ((null? state) (error "Variable not declared"))
       ((assq (key binding) (topLayer state)) (cons (addBinding binding (topLayer state)) (cdr state)))
       (else (cons (topLayer state) (M_state_Assignment_updateBinding binding (cdr state))))))


; defining a function that returns a value of a variable if initialized or an error message if not
(define (lookupvar var state)
     (if (findvar var state) (value (findvar var state)) (error "Variable not declared!")))


; defining a function that returns boolean indicating whether the binding defined in top layer
(define
   (definedInTopBinding binding state)
   (equal? (assq (key binding) (topLayer state)) binding))

; defining a function that adds a binding to state
(define
    (addBind binding state)
    (cons (addBinding binding (topLayer state)) (cdr state)))

; defining a function that finds the variable in state
(define (findvar var state)
    (cond
       ((null? state) #f)
       ((assq var (topLayer state)) (assq var (topLayer state)))
       (else (findvar var (cdr state)))))







