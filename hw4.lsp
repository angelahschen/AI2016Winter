
(load "parse_cnf.lsp")

;checkhelp takes 3 argument: a clause in a CNF (clause), the most current list of variables we're testing(test), the number of variables we've assumed a value in test(iter)
;check recursively check if the first element OR anything in the rest of the clause is satisfied
(defun checkhelp (clause iter test)
	(cond 
		((not clause) nil) ;;nil
		((>  (abs (car clause)) iter ) t)
		((= (nth  (abs (car clause)) test) (car clause) ) t)
		(t (checkhelp (cdr clause) iter test))
	)
)

;check takes 3 arguments: a CNF (delta), the most current list of variables we're testing(test), the number of variables we've assumed a value in test(iter)
;recursively check if the first element is satisfied and if the rest of the elements are
;use checkhelp to check the first element

(defun check (delta iter test)
	(cond
		((not delta) t) ;all clauses satisfied
		((not (checkhelp (car delta) iter test)) NIL)
		(t (check (cdr delta) iter test ))
	)
)

;backtrack takes 4 arguments: a CNF (delta), number of how many variables (n), 
;the most current list of variables we're testing(test), the number of variables we've assumed a value in test(iter),
;use the function check to see if 'the current assigned variables of test' satisfies 'delta'
;if not, return nil, if satisfied, proceed to check if assuming the next variable as + and - would satisfy delta
(defun backtrack (delta n iter test)
	;do check
	;if okay proceed
	(cond
		((not(check delta iter test)) nil) ;or butlast???
		( (= n iter) test )
		((setq res (backtrack delta n (+ 1 iter) (append test (cons(+ 1 iter)nil) )) ) res) ; if not nil, return this
		((setq res (backtrack delta n (+ 1 iter) (append test (cons(- (+ 1 iter))nil)  ))) res) ; if not nil, return this
		(t nil)
	)
)

;sat? takes 2 arguments: a CNF (delta) and the number of variables in it (n)
(defun sat? (n delta)
	(setq test (cons 0 nil))
	(setq result (backtrack delta n 0 '(0)))
	(cdr result)
)