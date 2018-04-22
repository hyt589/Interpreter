(require "classParser.scm")

;--------------------------------------------------------------------------------------------------------
;---------------------------Interpreter Implementation---------------------------------------------------

; defining a function that takes an input file to be executed and returns a value
(define interpret
  (lambda (filename)
    (cond
      ((not (string? filename)) (error "File name must be a string!"))
      (else (lookupvar 'M_state_return
          (call/cc (lambda (return) (M_state_funcall '(funcall main) (run (parser filename) M_state_nullState '() '() '() '()) return '() '() '()))))))))


; abstractions
(define getFirst car)
(define getAfterFirst cdr)
(define getSecond cadr)
(define getAfterSecond cddr)
(define getThird caddr)
(define getAfterThird cdddr)
(define getFourth cadddr)

(define add cons)
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
  (lambda (dec state return whileReturn throwReturn breakReturn)
    (cond
      ((null? (getAfterSecond dec)) (M_state_Declaration_updateBinding (bind (getSecond dec) (box null)) state))
      (else (M_state_Declaration_updateBinding (bind (getSecond dec) (box (M_value (getThird dec) state return whileReturn throwReturn breakReturn))) state)))))

; defining a function that returns the value of an expression
(define M_value
  (lambda (exp state return whileReturn throwReturn breakReturn)
    (cond
      ((number? exp) exp)
      ((eq? exp '#t) 'true)
      ((eq? exp '#f) 'false)
      ((eq? exp 'true) 'true)
      ((eq? exp 'false) 'false)
      ((symbol? exp) (lookupvar exp state))
      ((and (null? (getAfterSecond exp)) (eq? (getFirst exp) '-)) (- 0 (M_value (getSecond exp) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst exp) '+) (+ (M_value (getSecond exp) state return whileReturn throwReturn breakReturn) (M_value (getThird exp) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst exp) '-) (- (M_value (getSecond exp) state return whileReturn throwReturn breakReturn) (M_value (getThird exp) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst exp) '*) (* (M_value (getSecond exp) state return whileReturn throwReturn breakReturn) (M_value (getThird exp) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst exp) '/) (quotient (M_value (getSecond exp) state return whileReturn throwReturn breakReturn) (M_value (getThird exp) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst exp) '%) (modulo (M_value (getSecond exp) state return whileReturn throwReturn breakReturn) (M_value (getThird exp) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst exp) 'funcall) (lookupvar 'M_state_return (call/cc (lambda (return) (M_state_funcall exp state return whileReturn throwReturn breakReturn)))))
      ((or (eq? (getFirst exp) '==)
           (or (eq? (getFirst exp) '<)
               (or (eq? (getFirst exp) '>)
                   (or (eq? (getFirst exp) '<=)
                       (or (eq? (getFirst exp) '>=)
                           (or (eq? (getFirst exp) '!=)
                               (or (eq? (getFirst exp) '&&)
                                   (or (eq? (getFirst exp) '||)
                                       (or (eq? (getFirst exp) '!)))))))))) (M_value (M_bool exp state return whileReturn throwReturn breakReturn) state))
      (else (error "unknown operator")))))


; defining a function for assignment so that it returns a state after the assignment
; used box, defined (var (box)) as a binding
(define M_state_assignment
  (lambda (asg state return whileReturn throwReturn breakReturn)
    (M_state_Assignment_updateBinding (bind (getSecond asg) (box (M_value (getThird asg) state return whileReturn throwReturn breakReturn))) state)))

; defining a function for the return whileReturn throwReturn statement that returns the value of the expression being returned
(define M_state_return
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((null? (getSecond stmt)) (error "Nothing to M_state_return"))
      (else (M_state_Declaration_updateBinding (bind 'M_state_return (box (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn))) (poplayer state))))))

; defining a function for throw so that returns a state after throw
(define M_state_throw
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((null? (getSecond stmt)) (error "Nothing to M_state_throw"))
      (else (M_state_Assignment_updateBinding (bind 'throw (box (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn))) state)))))


; defining a function that returns a boolean based on the input statement
(define M_bool
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((null? stmt) (error "Conditional statement needed!"))
      ((eq? stmt 'true) '#t)
      ((eq? stmt 'false) '#f)
      ((eq? stmt '#t) '#t)
      ((eq? stmt '#f) '#f)
      ((symbol? stmt) (M_bool (lookupvar stmt state) state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) '==) (= (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_value (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '<) (< (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_value (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '>) (> (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_value (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '>=) (>= (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_value (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '<=) (<= (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_value (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '!=) (not (= (M_value (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_value (getThird stmt) state return whileReturn throwReturn breakReturn))))
      ((eq? (getFirst stmt) '&&) (and (M_bool (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_bool (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '||) (or (M_bool (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_bool (getThird stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) '!) (not (M_bool (getSecond stmt) state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) 'funcall) (lookupvar 'M_state_return (M_state_funcall stmt state return whileReturn throwReturn breakReturn)))
      (else (error "Invalid conditional statement!")))))

; defining a function that returns a state after an if statement
(define M_state_if
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((M_bool (getSecond stmt) state return whileReturn throwReturn breakReturn) (M_state (getThird stmt) state return whileReturn throwReturn breakReturn))
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
      ((M_bool (getSecond stmt) state return whileReturn throwReturn breakReturn) (cpsreturn (M_state_while-cps stmt (run (getAfterSecond stmt) state return whileReturn throwReturn breakReturn) return whileReturn throwReturn breakReturn cpsreturn)))
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
    (M_state_Declaration_function func state)))

; defining a function that returns the state after running a function
(define M_state_funcall
  (lambda (funcallstat state return whileReturn throwReturn breakReturn)
    (cond
      ((null? (cdr state)) (run (getFourth (lookupfunc (getSecond funcallstat) state)) (createFuncLayer (getThird (lookupfunc (getSecond funcallstat) state)) (getAfterSecond funcallstat) (addlayer '() state)) return whileReturn throwReturn breakReturn))
      ((null? (getAfterSecond funcallstat)) (run (getFourth (lookupfunc (getSecond funcallstat) state)) (addlayer '() (cdr state)) return whileReturn throwReturn breakReturn))
      (else (run (getFourth (lookupfunc (getSecond funcallstat) state)) (cons (getFirst (createFuncLayer (getThird (lookupfunc (getSecond funcallstat) state)) (getAfterSecond funcallstat) (addlayer '() state))) state) return whileReturn throwReturn breakReturn)))))
    

(define returnit (lambda(v) v))
;defining a function that returns a state after a statement
(define M_state
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((null? stmt) state)
      ((eq? (getFirst stmt) 'class) (M_state_Declaration_class (classClosure stmt state return whileReturn throwReturn breakReturn) state))
      ((eq? (getFirst stmt) 'var) (M_state_declaration stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) '=) (M_state_assignment stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) 'return) (return (M_state_return stmt state return whileReturn throwReturn breakReturn)))
      ((eq? (getFirst stmt) 'throw)  (if (null? throwReturn) (error "Error: throw not in try block") (throwReturn (M_state_throw stmt state return whileReturn throwReturn breakReturn))))
      ((eq? (getFirst stmt) 'if) (M_state_if stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) 'while) (returnit (call/cc (lambda (breakReturn) (M_state_while stmt (M_state_Declaration_updateBinding (bind 'gotype 0) state) return whileReturn throwReturn breakReturn)))))
      ((eq? (getFirst stmt) 'begin)  (poplayer (call/cc (lambda (whileReturn) (run (getAfterFirst stmt) (addlayer emptyLayer state) return whileReturn throwReturn breakReturn)))))
      ((eq? (getFirst stmt) 'continue) (whileReturn (M_state_Assignment_updateBinding (bind 'gotype 'continue) state)))
      ((eq? (getFirst stmt) 'break) (breakReturn (poplayer (M_state_Assignment_updateBinding (bind 'gotype 'break) state))))
      ((eq? (getFirst stmt) 'try) (M_state_try stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) 'function) (M_state_function stmt state))
      ((and (eq? (getFirst stmt) 'funcall) (eq? (getSecond stmt) 'main)) (M_state_funcall stmt state return whileReturn throwReturn breakReturn))
      ((eq? (getFirst stmt) 'funcall) (call/cc (lambda (funcreturn) (M_state_funcall stmt state funcreturn whileReturn throwReturn breakReturn))))
      ((eq? (getFirst stmt) 'static-function) (M_state_function (getAfterFirst stmt) state))
      (else (error "Invalid statements")))))

; Creates a proper layer of the function call
(define createFuncLayer
  (lambda (paramlis inputlis state)
    (cond
      ((paramMismatch paramlis inputlis) (error "Arity mismatch!"))
      ((null? paramlis) state)
      ((null? (getAfterFirst paramlis)) (M_state_Declaration_updateBinding (createBinding (getFirst paramlis) (getFirst inputlis) state) state))
      (else (createFuncLayer (getAfterFirst paramlis) (getAfterFirst inputlis) (M_state_Declaration_updateBinding (createBinding (getFirst paramlis) (getFirst inputlis) state) state))))))

; Check if the parameter list and input list mismatch
(define paramMismatch
  (lambda (l1 l2)
    (cond
      ((not (eq? (listLength l1) (listLength l2))) #t)
      (else #f))))

; Returns the length of a given list (number of elemnets)
(define listLength
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (listLength (cdr l))))))

; Creates a binding for each parameter with respective input value
(define createBinding
  (lambda (param input state)
    (cond
      ((eq? input 'true) (list param (box 'true)))
      ((eq? input 'false) (list param (box 'false)))
      (else (list param (box (M_value input (poplayer state) '() '() '() '())))))))
    
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
                (M_state_Declaration_updateBinding (bind (catchVar stmt) (lookupvar 'throw state)) (cdr state))
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
                   (run (tryBody stmt) (M_state_Declaration_updateBinding (bind 'throw (box 'none)) state) return whileReturn throwReturn breakReturn)))
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
      ((eq? (key binding) 'M_state_return) (cons (addBinding binding (topLayer state)) (getAfterFirst state)))
      ((assq (key binding) (topLayer state)) (error "Variable already declared"))
      (else (cons (addBinding binding (topLayer state)) (getAfterFirst state))))))

; defining a function that declares functions
(define M_state_Declaration_function
  (lambda (function state)
    (add (add function (topLayer state)) (getAfterFirst state))))

; defining a class that decalres classes
(define M_state_Declaration_class
  (lambda (class global)
    (append (add class (topLayer global)) (getAfterFirst global))))

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
         (if (box? (getSecond (findvar var state)))
             (unbox (getSecond (findvar var state)))
             (getSecond (findvar var state)))
         ((error "Variable not declared!")))))

; defining a function that adds a binding if the top layer does not contain it and return error if it is already declared in the top layer
(define declareLocalVar
  (lambda (binding state)
    (cond 
      ((assq (key binding) (topLayer state)) (error "Local variable already declared!"))
      (else (M_state_Declaration_updateBinding binding state)))))

(define getClassName caar)
(define getClassClosure cadar)
(define getTailClasses cdr)
; defining a function that lookup a class and return the class closure
(define lookupclass
  (lambda (name state)
    (cond
      ((null? state) (error "Class not defined!"))
      ((eq? (getClassName state) name) (getClassClosure state))
      (else (lookupclass name (getTailClasses state))))))

; defining a function that returns a function if defined or an error msg if not
(define lookupfunc
  (lambda (name state)
    (cond
      ((null? state) (error "Function not defined!"))
      ((and (assq 'function (topLayer state)) (equal? (getSecond (assq 'function (topLayer state))) name))
       (assq 'function (topLayer state)))
      ((assq 'function (topLayer state)) (lookupfunc name (list (getAfterFirst (topLayer state)))))
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

;---------------------------part4----------------------------------
;------------------------------------------------------------------

(define className getSecond)
(define superClass caaddr)
(define classBody getFourth)
    
    
;define a function that returns the closure of a given class
(define classClosure
  (lambda (stmt state return whileReturn throwReturn breakReturn)
    (cond
      ((and (not (null? (getThird stmt)))(eq? (getFirst (getThird stmt)) 'extends)) (list (className stmt) (superClass stmt) (run (classBody stmt) state return whileReturn throwReturn breakReturn)))
      (else (list (className stmt) (run (classBody stmt) state return whileReturn throwReturn breakReturn))))))

;defien a function that returns the closure of a given function


;define a function that returns the closure of a given instance of a class




