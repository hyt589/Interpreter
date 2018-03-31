(require "functionParser.scm")

;--------------------------------------------------------------------------------------------------------
;---------------------------Interpreter Implementation---------------------------------------------------

; defining a function that takes an input file to be executed and returns a value
(define interpret
  (lambda (filename)
    (cond
      ((not (string? filename)) (error "File name must be a string!"))
      (else (lookupvar 'M_state_return 
          (call/cc (lambda (return) (run (parser filename) M_state_nullState return '() '() '()))))))))


; abstractions
(define getFirst car)
(define getAfterFirst cdr)
(define getSecond cadr)
(define getAfterSecond cddr)
(define getThird caddr)
(define getAfterThird cdddr)
(define getFourth cadddr)

(define addlayer cons)
(define emptyLayer '())
(define poplayer cdr)
(define topLayer car)
(define key car)
(define value getSecond)
(define M_state_nullState '(()))
(define addBinding cons)
(define bind list)

; defining a function for variable declaration so that it returns the state after the declaration statement
(define M_state_declaration
  (lambda (dec state)
    (cond
      ((null? (getAfterSecond dec)) (M_state_Declaration_updateBinding (getAfterFirst dec) state))
      (else (M_state_Declaration_updateBinding (bind (getSecond dec) (box (M_value (getThird dec) state))) state)))))

; defining a function that returns the value of an expression
(define M_value
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((eq? exp '#t) 'true)
      ((eq? exp '#f) 'false)
      ((symbol? exp) (lookupvar exp state))
      ((and (null? (getAfterSecond exp)) (eq? (getFirst exp) '-)) (- 0 (M_value (getSecond exp) state)))
      ((eq? (getFirst exp) '+) (+ (M_value (getSecond exp) state) (M_value (getThird exp) state)))
      ((eq? (getFirst exp) '-) (- (M_value (getSecond exp) state) (M_value (getThird exp) state)))
      ((eq? (getFirst exp) '*) (* (M_value (getSecond exp) state) (M_value (getThird exp) state)))
      ((eq? (getFirst exp) '/) (quotient (M_value (getSecond exp) state) (M_value (getThird exp) state)))
      ((eq? (getFirst exp) '%) (modulo (M_value (getSecond exp) state) (M_value (getThird exp) state)))
      ((eq? (getFirst exp) 'funcall) (lookupvar 'M_state_return 
          (call/cc (lambda (return) (run (getFourth (lookupfunc (getSecond exp))) (createFuncLayer (getThird (lookupfunc (getSecond exp))) (getAfterSecond exp) state) return '() '() '())))))
      ((or (eq? (getFirst exp) '==)
           (or (eq? (getFirst exp) '<)
               (or (eq? (getFirst exp) '>)
                   (or (eq? (getFirst exp) '<=)
                       (or (eq? (getFirst exp) '>=)
                           (or (eq? (getFirst exp) '!=)
                               (or (eq? (getFirst exp) '&&)
                                   (or (eq? (getFirst exp) '||)
                                       (or (eq? (getFirst exp) '!)))))))))) (M_value (M_bool exp state) state))
      (else (error "unknown operator")))))

(define createFuncLayer
  (lambda (paramlis inputlis state)
    (cond
      ((null? paramlis) state)
      ((null? (getAfterFirst paramlis)) (M_state_Declaration_updateBinding (createBinding (getFirst paramlis) (getFirst inputlis) state) state))
      (else (createFuncLayer (getAfterFirst paramlis) (getAfterFirst inputlis) (M_state_Declaration_updateBinding (createBinding (getFirst paramlis) (getFirst inputlis) state) state))))))
    
(define createBinding
  (lambda (param input state)
    (list param (box (lookupvar input state)))))

; defining a function for assignment so that it returns a state after the assignment
; used box, defined (var (box)) as a binding
(define M_state_assignment
  (lambda (asg state)
    (M_state_Assignment_updateBinding (bind (getSecond asg) (box (M_value (getThird asg) state))) state)))

; defining a function for the return whileReturn throwReturn statement that returns the value of the expression being returned
(define M_state_return
  (lambda (stmt state)
    (cond
      ((null? (getSecond stmt)) (error "Nothing to M_state_return"))
      (else (M_state_Declaration_updateBinding (bind 'M_state_return (box (M_value (getSecond stmt) state))) state)))))

; defining a function for throw so that returns a state after throw
(define M_state_throw
  (lambda (stmt state)
    (cond
      ((null? (getSecond stmt)) (error "Nothing to M_state_throw"))
      (else (M_state_Assignment_updateBinding (bind 'throw (box (M_value (getSecond stmt) state))) state)))))


; defining a function that returns a boolean based on the input statement
(define M_bool
  (lambda (stmt state)
    (cond
      ((null? stmt) (error "Conditional statement needed!"))
      ((eq? stmt 'true) '#t)
      ((eq? stmt 'false) '#f)
      ((symbol? stmt) (M_bool (lookupvar stmt state) state))
      ((eq? (getFirst stmt) '==) (= (M_value (getSecond stmt) state) (M_value (getThird stmt) state)))
      ((eq? (getFirst stmt) '<) (< (M_value (getSecond stmt) state) (M_value (getThird stmt) state)))
      ((eq? (getFirst stmt) '>) (> (M_value (getSecond stmt) state) (M_value (getThird stmt) state)))
      ((eq? (getFirst stmt) '>=) (>= (M_value (getSecond stmt) state) (M_value (getThird stmt) state)))
      ((eq? (getFirst stmt) '<=) (<= (M_value (getSecond stmt) state) (M_value (getThird stmt) state)))
      ((eq? (getFirst stmt) '!=) (not (= (M_value (getSecond stmt) state) (M_value (getThird stmt) state))))
      ((eq? (getFirst stmt) '&&) (and (M_bool (getSecond stmt) state) (M_bool (getThird stmt) state)))
      ((eq? (getFirst stmt) '||) (or (M_bool (getSecond stmt) state) (M_bool (getThird stmt) state)))
      ((eq? (getFirst stmt) '!) (not (M_bool (getSecond stmt) state)))
      (else (error "Invalid conditional statement!")))))

; defining a function that returns a state after an if statement
(define M_state_if
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((M_bool (getSecond stmt) state) (M_state (getThird stmt) state return whileReturn throwReturn breakReturn))
      ((null? (getAfterThird stmt)) state)
      (else (M_state (getFourth stmt) state return whileReturn throwReturn breakReturn)))))

; defining a function that takes an initial state and a list of statements and returns the final state after runing the statements in the list
(define run-cps
  (lambda (stmtlis state return whileReturn throwReturn breakReturn cpsreturn)
    (cond
      ((null? stmtlis) (cpsreturn state))
      ((null? (getAfterFirst stmtlis)) (cpsreturn (M_state (getFirst stmtlis) state return whileReturn throwReturn breakReturn)))
      (else (cpsreturn (run-cps (getAfterFirst stmtlis) (M_state (getFirst stmtlis) state return whileReturn throwReturn breakReturn) return whileReturn throwReturn breakReturn cpsreturn))))))

; defining a wrapper for run-cps
(define run
  (lambda (stmtlis state return whileReturn throwReturn breakReturn)
    (run-cps stmtlis state return whileReturn throwReturn breakReturn (lambda (v) v))))


;defining a function that returns a state after a while statement
(define M_state_while-cps
  (lambda (stmt state return whileReturn throwReturn breakReturn cpsreturn)
    (cond
      ((definedInTopBinding (bind 'gotype 'break) state) (cpsreturn state))
      ((M_bool (getSecond stmt) state) (cpsreturn (M_state_while-cps stmt (run (getAfterSecond stmt) state return whileReturn throwReturn breakReturn) return whileReturn throwReturn breakReturn cpsreturn)))
      (else (cpsreturn state)))))

; defining a function that returns boolean indicating whether the binding defined in top layer
(define definedInTopBinding
  (lambda (binding state)
   (equal? (assq (key binding) (topLayer state)) binding)))

;defining a wrapper for while-cps
(define M_state_while
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (M_state_while-cps stmt state return whileReturn throwReturn breakReturn (lambda (v) v))))

; defining a function that returns the state after storing a function
(define M_state_function
  (lambda (func state)
    (M_state_Declaration_updateBinding func state)))
    

(define returnit (lambda(v) v))
;defining a function that returns a state after a statement
(define M_state
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((null? stmt) state)
      ((eq? (getFirst stmt) 'var) (M_state_declaration stmt state))
      ((eq? (getFirst stmt) '=) (M_state_assignment stmt state))
      ((eq? (getFirst stmt) 'return) (return (M_state_return stmt state)))
      ((eq? (getFirst stmt) 'throw)  (if (null? throwReturn) (error "Error: throw not in try block") (throwReturn (M_state_throw stmt state ))))
      ((eq? (getFirst stmt) 'if) (M_state_if stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) 'while) (returnit (call/cc (lambda (breakReturn) (M_state_while stmt (M_state_Declaration_updateBinding (bind 'gotype 0) state) return whileReturn throwReturn breakReturn)))))
      ((eq? (getFirst stmt) 'begin)  (poplayer (call/cc (lambda (whileReturn) (run (getAfterFirst stmt) (addlayer emptyLayer state) return whileReturn throwReturn breakReturn)))))
      ((eq? (getFirst stmt) 'continue) (whileReturn (M_state_Assignment_updateBinding (bind 'gotype 'continue) state)))
      ((eq? (getFirst stmt) 'break) (breakReturn (poplayer (M_state_Assignment_updateBinding (bind 'gotype 'break) state))))
      ((eq? (getFirst stmt) 'try) (M_state_try stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) 'function) (M_state_function stmt state))
      (else (error "Invalid statements")))))

; abstraction
(define tryBody getSecond)
(define catchBody getThird)
(define finalBody cadddr)

; defining a function for catch so that returns a state after catch
(define M_state_catch
  (lambda (stmt state return whileReturn throwReturn breakReturn)
     (if (null? stmt) state
        (if (equal? (lookupvar 'throw state) 'none)
            state
            (run (catchBody stmt) 
                (M_state_Declaration_updateBinding (bind (catchVar stmt) (lookupvar 'throw state)) state)
                return whileReturn throwReturn breakReturn)))))

; abstraction
(define finalStmt getSecond)
(define catchVar caadr)
(define catchStmt getThird)

; defining a function for finally so that returns a state after finally
(define M_state_final
  (lambda (stmt state return whileReturn throwReturn breakReturn)
      (if (null? stmt) state
        (run (finalStmt stmt) state return whileReturn throwReturn breakReturn))))

; defining a function for try statement so that it returns a state after try statement
(define M_state_try
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (M_state_final (finalBody stmt)
    (M_state_catch (catchBody stmt)   (call/cc (lambda (throwReturn) 
                   (run (tryBody stmt) (M_state_Declaration_updateBinding (bind 'throw 'none) state) return whileReturn throwReturn breakReturn)))
                   return whileReturn throwReturn breakReturn)
             return whileReturn throwReturn breakReturn)))




;----------------------------------------------------------------------------------------------------
;------------------------------------State Implementation--------------------------------------------
; the following are functions written to hide state implementation from the rest of interpreter

; This implementation of the state is a list of list of pairs, each list of pairs is a layer,
; each pair contains a variable name and its value

; defining a function that updates the bindings in a given state in a delaration statement
(define M_state_Declaration_updateBinding
  (lambda (binding state)
    (cond
       ((assq (key binding) (topLayer state)) (error "Variable already declared"))
       (else (cons (addBinding binding (topLayer state)) (getAfterFirst state))))))

; defining a function that updates the bindings in a given state in a assignment statement
(define M_state_Assignment_updateBinding-cps
  (lambda (binding state cpsreturn)
    (cond
       ((null? state) (cpsreturn (error "Variable not declared")))
       ;if the variable is already declared, update the box
       ((assq (key binding) (topLayer state)) (cpsreturn (updateBox (assq (key binding) (topLayer state)) binding state)))
       (else (M_state_Assignment_updateBinding-cps binding (getAfterFirst state) (lambda (v) (cpsreturn (cons (topLayer state) v))))))))

; if the box has been updated, return the state
(define updateBox
  (lambda (oldbinding newbinding state)
    (begin (set-box! (getSecond oldbinding) (unbox (getSecond newbinding))) state)))

; defining a wrapper for M_State_Assignment_updateBinding-cps
(define M_state_Assignment_updateBinding
  (lambda (binding state)
    (M_state_Assignment_updateBinding-cps binding state (lambda(v) v))))

; defining a function that returns a value of a variable if initialized or an error message if not
(define lookupvar
  (lambda (var state)
     (if (findvar var state)
         (unbox (getSecond (findvar var state)))
         ((error "Variable not declared!")))))

; defining a function that returns a function if defined or an error msg if not
(define lookupfunc
  (lambda (name state)
    (cond
      ((null? state) (error "Function not defined!"))
      ((and (assq 'function (topLayer state)) (equal? (getSecond (assq 'function (topLayer state))) name))
       (assq 'function (topLayer state)))
      (else (lookupfunc name (getAfterFirst state))))))

; defining a function that finds the binding of the variable in state
(define findvar-cps
  (lambda (var state cpsreturn)
    (cond
       ((null? state) (cpsreturn #f))
       ((assq var (topLayer state)) (cpsreturn (assq var (topLayer state))))
       (else (cpsreturn (findvar-cps var (getAfterFirst state) cpsreturn))))))

; defining a wrapper for findvar-cps
(define findvar
  (lambda (var state)
    (findvar-cps var state (lambda(v) v))))


; Used this line to test whether updateBinding is updating the boxes correctly
; (lookupvar 'x (M_state_Assignment_updateBinding (list 'x (box '6)) (M_state_Declaration_updateBinding (list 'x (box '3)) '(()))))
; x is initialized as 3 and the updateBinding updates x with a value of 6 and return the state,
; we then lookupvar x to see if the value of x has changed.
; the above line returns 6, so it works, I think...





