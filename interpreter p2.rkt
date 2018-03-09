(require "simpleParser.scm")

; definign a function that takes an input file to be executed and returns a value
(define interpret
  (lambda (filename)
    (cond
      ((not (string? filename)) (error "File name must be a string!"))
      (else (lookupvar 'M_state_return (run (parser filename) (M_state_nullState)))))))


;--------------------------------------------------------------------------
;------------------------Interpreter---------------------------------------

; abstraction
(define getfirst car)
(define getsecond cadr)
(define getthird caddr)
(define getforth cadddr)
(define getsecond* cdr)
(define getthird* cddr)
(define getforth* cdddr)

; defining a function that returns a state after a declaration statement
(define M_state_declare
  (lambda (stmt state)
    (cond
      ((null? (getthird* stmt)) (M_state_declareBinding (cons (getsecond stmt) null) state))
      (else (M_state_declareBinding (cons (getsecond stmt) (M_value (getthird stmt) state)) state)))))

; defining a function that returns a state after an assignment statement
(define M_state_assign
  (lambda (stmt state)
    (M_state_updateBinding (cons (getsecond stmt) (M_value (getthird stmt) state)) state)))
      
; defining a function that returns the value of an expression
(define M_value
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((eq? exp '#t) 'true)
      ((eq? exp '#f) 'false)
      ((symbol? exp) (M_value_var exp state))
      ((and (null? (second* exp)) (eq? (getfirst exp) '-)) (- 0 (M_value (getsecond exp) state)))
      ((eq? (getfirst exp) '+) (+ (M_value (getsecond exp) state) (M_value (getthird exp) state)))
      ((eq? (getfirst exp) '-) (- (M_value (getsecond exp) state) (M_value (getthird exp) state)))
      ((eq? (getfirst exp) '*) (* (M_value (getsecond exp) state) (M_value (getthird exp) state)))
      ((eq? (getfirst exp) '/) (quotient (M_value (getsecond exp) state) (M_value (getthird exp) state)))
      ((eq? (getfirst exp) '%) (modulo (M_value (getsecond exp) state) (M_value (getthird exp) state)))
      ((or (eq? (getfirst exp) '==)
           (or (eq? (getfirst exp) '<)
               (or (eq? (getfirst exp) '>)
                   (or (eq? (getfirst exp) '<=)
                       (or (eq? (getfirst exp) '>=)
                           (or (eq? (getfirst exp) '!=)
                               (or (eq? (getfirst exp) '&&)
                                   (or (eq? (getfirst exp) '||)
                                       (or (eq? (getfirst exp) '!)))))))))) (M_value (M_bool exp state) state))
      (else (error "unknown operator")))))

; defining a function that returns a boolean based on the input statement
(define M_bool
  (lambda (stmt state)
    (cond
      ((null? stmt) (error "Conditional statement needed!"))
      ((eq? stmt 'true) '#t)
      ((eq? stmt 'false) '#f)
      ((symbol? stmt) (M_bool (M_value_var stmt state) state))
      ((eq? (getfirst stmt) '==) (= (M_value (getsecond stmt) state) (M_value (getthird stmt) state)))
      ((eq? (getfirst stmt) '<) (< (M_value (getsecond stmt) state) (M_value (getthird stmt) state)))
      ((eq? (getfirst stmt) '>) (> (M_value (getsecond stmt) state) (M_value (getthird stmt) state)))
      ((eq? (getfirst stmt) '>=) (>= (M_value (getsecond stmt) state) (M_value (getthird stmt) state)))
      ((eq? (getfirst stmt) '<=) (<= (M_value (getsecond stmt) state) (M_value (getthird stmt) state)))
      ((eq? (getfirst stmt) '!=) (not (= (M_value (getsecond stmt) state) (M_value (getthird stmt) state))))
      ((eq? (getfirst stmt) '&&) (and (M_bool (getsecond stmt) state) (M_bool (getthird stmt) state)))
      ((eq? (getfirst stmt) '||) (or (M_bool (getsecond stmt) state) (M_bool (getthird stmt) state)))
      ((eq? (getfirst stmt) '!) (not (M_bool (getsecond stmt) state)))
      (else (error "Invalid conditional statement!")))))

;----------------------------------Need To Do--------------------------------------------

;1. M_state_block

;2. try-catch-finally
;abstraction
(define tryblock cadr)
(define secondblock caddr)
(define thirdblock cadddr)
(define blockidentifier car)
(define exception caadr)
(define catchblock caddr)
(define finallyblock cadr)
(define lastelement cdddr)
(define ex? car)
(define trystate cadr)
(define exval caddr)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------try-catch-finally--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define M_state_trycatchfinally
  (lambda (stmt state)
    (cond
      ;exception thrown, try-catch-finally
      ((and (not (null? (lastelement stmt)))                               
            (ex? (M_state_try (tryblock stmt) (M_state_addLayer state))))  ;add layer to state - execute tryblock(remove top layer upone exit)
                                                                           ;add layer to state - declare exception with value - execute catch(remove top layer upone exit)
                                                                           ;add layer to state - execute finally(remove top layer upon exit)
                                                                           (M_state_finally (finallyblock (thirdblock stmt))
                                                                                            (M_state_addLayer (M_state_catch (catchblock (secondblock stmt)) (M_state_declareBinding (cons (exception (secondblock stmt)) (exval (M_state_try (tryblock stmt) (M_state_addLayer state))))
                                                                                                                                                     (M_state_addLayer (trystate (M_state_try (tryblock stmt) (M_state_addLayer state)))))))))
      ;no exception thrown, try-catch-finally   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      ((not (null? (lastelement stmt)))       ;add layer to state - execute tryblock(remove top layer upon exit)
                                              ;add layer to state - execute finally(remove top layer upon exit)
                                              (M_state_finally (finallyblock (thirdblock stmt)) (M_state_addLayer
                                                                                            (trystate (M_state_try (tryblock stmt) (M_state_addLayer state))))))
      ;exception thrown, try-catch     --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      ((and (ex? (M_state_try (tryblock stmt) (M_state_addLayer state)))
            (eq? (blockidentifier (secondblock stmt)) 'catch))           ;add layer to state - execute tryblock(remove top layer upon exit)
                                                                         ;add layer to state - declare exception with value - execute catch(remove top layer upon exit)
                                                                         (M_state_catch (catchblock (secondblock stmt)) (M_state_delareBinding (cons (exception (secondblock stmt)) (exval (M_state_try (tryblock stmt) (M_state_addLayer state))))
                                                                                                (M_state_addLayer (trystate (M_state_try (tryblock stmt) (M_state_addLayer state)))))))
      ;no exception thrown, try-catch  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      ((eq? (blockidentifier (secondblock stmt)) 'catch) ;add layer to state - execute tryblock(remove top layer upon exit)
                                                         (trystate (M_state_try (tryblock stmt) (M_state_addLayer state))))
      ;try-finally   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      ((eq? (blockidentifier (secondblock stmt)) 'finally) ;add layer to state - execute tryblock(remove top layer upon exit)
                                                           ;add layer to state - execute finally(remove top layer upon exit)
                                                           (M_state_finally (finallyblock (secondblock stmt)) (M_state_addLayer
                                                                                                               (trystate (M_state_try (tryblock stmt) (M_state_addLayer state))))))
      ;other form would be unacceptable
      (else (error "invalid try statement")))))
                                                           
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                         
;cps function that executes the try block
(define M_state_try-cps
  (lambda (stmtlis state return);the state passed in already has an empty top layer added in trycatchfinally
    (cond
      ;stmtlis is null, no throw statement is executed, return (#f, a state after removing the top layer)
      ((null? stmtlis) (return (cons #f (list (M_state_removeLayer state)))))
      ;throw a list of (#t, a state after removing the top layer, exception value)
      ((eq? (getfirst (getfirst stmtlis)) 'throw) (return (cons #t (cons (M_state_removeLayer state) (list (M_value (getsecond (getfirst stmtlis)) state))))))
      ;if no throw statement is executed return (#f, a state after removing the top layer)
      ((null? (getsecond* stmtlis)) (return (cons #f (list (M_state_removeLayer (M_state (getfirst stmtlis) state))))))
      ;recurse
      (else (return (M_state_try-cps  (getsecond* stmtlis) (M_state (getfirst stmtlis) state) return))))))

; wrapper function for try block
(define M_state_try
  (lambda (stmtlis state)
    (M_state_try-cps stmtlis state (lambda (v) v))))

;(define M_state_catch)
(define M_state_catch-cps
  (lambda (stmtlis state return) ;the state passed in should already have an extra layer for this block, and "var e = ex" should already have been declared
    (cond
      ;return a state with top layer removed if the stmtlis is null
      ((null? stmtlis) (return (M_state_removeLayer state)))
      ;if stmtlis is not null, declare "var e = ex" in the top layer, execute stmtlis, and remove the top layer
      ((null? (getsecond* stmtlis)) (return (M_state_removeLayer (M_state (getfirst stmtlis) state))))
      (else (return (M_state_catch-cps (getsecond* stmtlis) (M_state (getfirst stmtlis) state) return))))))

;wrapper function for catch block
(define M_state_catch
  (lambda (stmtlis state)
    (M_state_catch-cps stmtlis state (lambda (v) v))))


;cps function that executes the finally block
(define M_state_finally-cps
  (lambda (stmtlis state return);the state passed in should've already been added a new layer
    (cond
      ((null? stmtlis) (return (M_state_removeLayer state)))
      ((null? (getsecond* stmtlis)) (return (M_state_removeLayer (M_state (getfirst stmtlis) state))))
      (else (return (M_state_finally-cps (getsecond* stmtlis) (M_state (getfirst stmtlis) state) return))))))

;wrapper function for finally block
(define M_state_finally
  (lambda (stmtlis state)
    (M_state_finally-cps stmtlis state (lambda (v) v))))



;3. break

;4. throw

;5. the following code may need changes


;-----------------------------------Need To Change---------------------------------------

; defining a function for the return statement that returns the value of the expression being returned
(define M_state_return-old
  (lambda (stmt state); return)
    (cond
      ((null? (cadr stmt)) (return (error "Nothing to return")))
      ;(else 'M_state_return_cps (M_value (cadr stmt) state)
      (else (append state (list (cons 'M_state_return (M_value (cadr stmt) state))))))))


;cps return
(define M_state_return-cps
  (lambda (stmt state return)
    (cond
      ((null? (cadr stmt)) (return (error "Nothing to Return")))
      (else (return (M_value (cadr stmt) state))))))

;return wrapper
(define M_state_return
  (lambda (stmt state)
    (M_state_return-cps stmt state (lambda (v) v))))

; defining a function that returns a state after an if statement
(define M_state_if
  (lambda (stmt state)
    (cond
      ((M_bool (getsecond stmt) state) (M_state (getthird stmt) state))
      ((null? (getforth* stmt)) state)
      (else (M_state (getforth stmt) state)))))

; defining a function that takes an initial state and a list of statements and returns the final state after runing the statements in the list
(define run-cps
  (lambda (stmtlis state return)
    (cond
      ((null? stmtlis) (return state))
      ((null? (getsecond* stmtlis)) (return (M_state (getfirst stmtlis) state)))
      (else (return (run (getsecond* stmtlis) (M_state (getfirst stmtlis) state) return))))))

;wrapper for the above function
(define run
  (lambda (stmtlis state)
    (run-cps stmtlis state (lambda (v) v))))

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
      ((null? (second* body)) state)
      (else (M_state_removeLayer (run (second* body) (M_state_addLayer state)))))))


;defining a function that returns a state after a statement
(define M_state
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((eq? (getfirst stmt) 'var) (M_state_declaration stmt state))
      ((eq? (getfirst stmt) '=) (M_state_assignment stmt state))
      ((eq? (getfirst stmt) 'return) (M_state_return stmt state))
      ((eq? (getfirst stmt) 'if) (M_state_if stmt state))
      ((eq? (getfirst stmt) 'while) (M_state_while stmt state))
      ((eq? (getfirst stmt) 'begin) (M_state_block stmt state))
      ((eq? (getfirst stmt) 'try) (M_state_trycatchfinally stmt state))
      (else (error "Invalid statements")))))



;--------------------------------------------------------------------------



;--------------------------------------------------------------------------
;------------------------State Implementation------------------------------

; The state is a list of layers, and layers are lists of pairs, and each pair contains
;a variable and a value

;The following are functions written to manipulate the state and its elements

; a function that takes no input and returns a state with an empty layer
(define M_state_nullState
  (lambda ()
    '(()) ))

; a function that adds an empty layer to a given state
(define M_state_addLayer
  (lambda (state)
    (cons '() state)))

; a function that removes the top layer of a given state
(define M_state_removeLayer
  (lambda (state)
    (cond
      ((null? state) (error "invalid state"))
      ((null? (cdr state)) state)
      (else (cdr state)))))

; a function that returns the top layer of a given state
(define topLayer
  (lambda (state)
    (car state)))

; a function that returns the list of layers excluding the toplayer of a given state
(define bottomLayers
  (lambda (state)
    (cond
      ((null? state) (error "invalid state"))
      ((null? (cdr state)) '())
      (else (cdr state)))))

; a function that returns the first binding in a given layer
(define firstBinding
  (lambda (layer)
    (cond
      ((null? layer) null)
      (else (car layer)))))

; a function that returns the list of all but the first bindings of a given layer
(define otherBindings
  (lambda (layer)
    (cond
      ((null? layer) null)
      (else (cdr layer)))))

; a function that returns the key(variable name) of a binding
(define bindingKey
  (lambda (binding)
    (cond
      ((null? binding) null)
      (else (car binding)))))

; a function that returns the value of a binding
(define bindingVal
  (lambda (binding)
    (cond
      ((null? binding) null)
      ((null? (cdr binding)) null)
      (else (cdr binding)))))
      
; cps function that tries to find a binding in a given layer
(define M_value_varInLayer-cps
  (lambda (var layer return)
    (cond
      ((null? layer) (return '(#f #f)))
      ((and (eq? var (bindingKey (firstBinding layer)))
            (not (null? (bindingVal (firstBinding layer))))) (return (cons #t (firstBinding layer))))
      ((eq? var (bindingKey (firstBinding layer))) (return (cons #t null)))
      ((null? (otherBindings layer)) (return '(#f #f)))
      (else (return (M_value_varInLayer-cps var (otherBindings layer) return))))))

; wrapper function for the above function
(define M_value_varInLayer
  (lambda (var layer)
    (M_value_varInLayer-cps var layer (lambda (v) v))))

;cps function that looks up a variable in the state
(define M_value_var-cps
  (lambda (var state return)
    (cond
      ((and (car (M_value_varInLayer var (topLayer state)))
            (not (null? (getsecond* (M_value_varInLayer var (topLayer state)))))) (return (bindingVal (cdr (M_value_varInLayer var (topLayer state))))))
      ((car (M_value_varInLayer var (topLayer state))) (return (error "Variable not initialized")))
      ((null? (bottomLayers state)) (return (error "Variable not diclared!")))
      (else (return (M_value_var-cps var (bottomLayers state) return))))))

; wrapper for the above function
(define M_value_var
  (lambda (var state)
    (M_value_var-cps var state (lambda (v) v))))

; a function that adds a binding to a layer
(define addBinding
  (lambda (binding layer)
    (cons binding layer)))

; cps function that adds a binding to the top layer when a var is declared
(define M_state_declareBinding-cps
  (lambda (binding state layers return)
    (cond
      ((null? state) (return (list (addBinding binding '()))))
      ((eq? layers (M_state_nullState)) (return (cons (addBinding binding (topLayer state)) (bottomLayers state))))
      ((car (M_value_varInLayer (bindingKey binding) (topLayer layers))) (return (error "Variable already declared")))
      ((null? (bottomLayers state)) (return (cons (addBinding binding (topLayer state)) (bottomLayers state))))
      (else (return (M_state_declareBinding-cps binding state (bottomLayers layers) return))))))

; wrapper for the above function
(define M_state_declareBinding
  (lambda (binding state)
    (M_state_declareBinding-cps binding state state (lambda (v) v))))

;a cps function that updates a value of a binding if it is in a given layer
(define M_state_updateBindingLayer-cps
  (lambda (binding layer return)
    (cond
      ((null? layer) (return null))
      ((eq? (bindingKey binding) (bindingKey (firstBinding layer))) (return (cons binding (otherBindings layer))))
      ((null? (otherBindings layer)) (return layer))
      (else (return (M_state_updateBindingLayer-cps binding (otherBindings layer) (lambda (v) (return (cons (firstBinding layer) v)))))))))

; a wrapper for the above function
(define M_state_updateBindingLayer
  (lambda (binding layer)
    (M_state_updateBindingLayer-cps binding layer (lambda (v) v))))


; a cps function that update the value of a variable if it is declared
(define M_state_updateBinding-cps
  (lambda (binding state return)
    (cond
      ((null? state) (return (error "variable not declared yet")))
      ((eq? state (M_state_nullState)) (return (error "variable not declared yet")))
      ((car (M_value_varInLayer (bindingKey binding) (topLayer state))) (return (cons (M_state_updateBindingLayer binding (topLayer state)) (bottomLayers state))))
      ((null? (bottomLayers state)) (return (error "variable not declared yet")))
      (else (return (M_state_updateBinding-cps binding (bottomLayers state) (lambda (v) (return (cons (topLayer state) v)))))))))

; a wrapper for the above function
(define M_state_updateBinding
  (lambda (binding state)
    (M_state_updateBinding-cps binding state (lambda (v) v))))