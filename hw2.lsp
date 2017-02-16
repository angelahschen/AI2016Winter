

;hw2.lsp

;dfs takes argument tree and does a DFS search
;basically, it does dfs on the first element of the tree, then append on the dfs result of the other elements

(defun dfs(TREE)
	(cond
		((numberp TREE) (cons TREE NIL))
		((= 1 (list-length TREE) ) (dfs (first TREE)))
		(t (append (dfs (car TREE))  (dfs (cdr TREE))))
	)
)


;check takes argument TREE
;if TREE is a number, it return a list containing that number
;else it doesn't return anything

;SUB-DFID takes argument TREE and DEPTH, and does DFS on tree to the depth of argument 'DEPTH'
;basically, it's the combination of doing SUB-DFID on first element with DEPTH-1, and doing SUB-DFID on (cdr TREE)

;DFID calls SUB-DFID iteratively, doing SUB-DFID with DEPTH from 1 to MAX-DEPTH

(defun check(TREE)
	(cond
		((numberp TREE) (cons TREE NIL))
		(t NIL )
	)
)


(defun SUB-DFID(TREE DEPTH)
	(cond
		( (= 0 DEPTH) (check TREE))
		( (numberp TREE) (check TREE))
		((= 1 (list-length TREE) ) (SUB-DFID (first TREE) (- DEPTH 1)))
		(t (append  (SUB-DFID (first TREE) (- DEPTH 1) ) (SUB-DFID (cdr TREE) DEPTH)))
	)
)

(defun DFID(TREE MAX-DEPTH)
	(cond
		((= MAX-DEPTH 0) (cons NIL NIL) )
		((= MAX-DEPTH 1) (SUB-DFID TREE MAX-DEPTH) )
		(t  (append (DFID TREE (- MAX-DEPTH 1) ) (SUB-DFID TREE MAX-DEPTH)))
	)
)

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond
		((not(and(= 3 (first s))(= 3 (second s)))) NIL)
		( t  (not (third TREE) ) )
	)
 )

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(setq sol NIL)
	(cond
		(
			(and(and (= m 1) (= c 1) ) (and (= (first s) (second s) ) (and (>= (first s) 1) (<= (first s) 3))) )
			;(setq sol (append sol (cons(cons (not (third s)) (cons (- 4 (first s))  (cons (- 4 (second s)) NIL))) NIL)))
			(setq sol (append sol (cons(cons (- 4 (first s)) (cons (- 4 (second s))  (cons(not (third s)) NIL))) NIL)))
		)
		(
			(and(and (= m 2) (= c 0) ) (or(and (= (first s) 2)(= (second s) 2)) (and(= (first s) 3)(= (second s) 1))))
			;(setq sol (append sol (cons(cons (not (third s)) (cons (- 5 (first s))  (cons (- 3 (second s)) NIL))) NIL)))
			(setq sol (append sol (cons(cons (- 5 (first s)) (cons(- 3 (second s))  (cons (not (third s)) NIL))) NIL)))
		)
		(
			(and(and (= m 0) (= c 2) ) (and(= (second s) 2)(= (first s) 3) ) )
			;(setq sol (append sol (cons(cons (not (third s)) (cons (- 3 m)  (cons (- 5 c) NIL)))NIL)))
			(setq sol (append sol (cons(cons (- 3 (first s) ) (cons (- 5 (second s) )  (cons (not (third s)) NIL)))NIL)))
		)
		(
			(and(and (= m 1) (= c 0) ) (or (and (= (first s) 3)(= (second s) 2)) (and (= (first s) 1)(= (second s) 1))) )
			;(setq sol (append sol (cons(cons(not (third s)) (cons (- 4 (first s))  (cons (- 3 (second s)) NIL)))NIL)))
			(setq sol (append sol (cons(cons(- 4 (first s)) (cons (- 3 (second s))  (cons (not (third s)) NIL)))NIL)))
		)
		(
			(and(and (= m 0) (= c 1) ) (or (and (= (first s) 0) (and (<= 1 (second s))(>= 3 (second s)))) (and(= (first s) 3)(or(= 1 (second s))(= 2 (second s))))) )
			;(setq sol(append sol (cons(cons(not (third s))(cons (- 3 (first s)) (cons (- 4 (second s)) NIL)))NIL)))
			(setq sol(append sol (cons(cons(- 3 (first s))(cons (- 4 (second s)) (cons (not (third s)) NIL)))NIL)))
		)
	)
	sol
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
	(setq sol NIL)
	(cond
		( (and (= (first s) (second s)) (and (>= (first s) 1) (<= (first s) 3))) (setq sol (append sol (cons(cons (- 4 (first s)) (cons (- 4 (second s))  (cons (not (third s)) NIL))) NIL))))
	);1m 1c cross
	(cond
		( (or(and (= (first s) 2)(= (second s) 2)) (and(= (first s) 3)(= (second s) 1)))(setq sol (append sol (cons(cons (- 5 (first s)) (cons (- 3 (second s))  (cons (not (third s)) NIL))) NIL))))
	); 2 m cross
	(cond
		((or  (and(= (second s) 2)(= (first s) 3))(and(= (second s) 3)(= (first s) 3))) (setq sol (append sol (cons(cons (- 3 (first s)) (cons (- 5 (second s))  (cons (not (third s)) NIL)))NIL))))
	);2 c cross
	(cond
		((or (and (= (first s) 3)(= (second s) 2)) (and (= (first s) 1)(= (second s) 1)))(setq sol (append sol (cons(cons(- 4 (first s)) (cons (- 3 (second s)) (cons (not (third s)) NIL)))NIL))))
	);1 m cross
	(cond
		((or (and (= (first s) 0) (and (<= 1 (second s))(>= 3 (second s)))) (and(= (first s) 3)(or(<= 1 (second s))(>= 3 (second s)))))(setq sol(append sol (cons(cons(- 3 (first s))(cons (- 4 (second s)) (cons (not (third s)) NIL)))NIL))))
	);1 c cross
	
	sol
)

; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.
(defun mult-dfs (states path depth)
	(cond
		((and(= 0 depth)(final-state(first states))) (append path (cons(first states)NIL)) )
		((= 0 depth) NIL)
		(t
			(or
				(or NIL (mult-dfs (succ-fn(first states)) (append path (cons(first states)NIL)) (- depth 1)))
				(mult-dfs (cdr states) path depth)
			)
		)
	)
)

; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.



(defun single-dfs (s path depth)
	(mult-dfs (succ-fn s) (append path (cons s NIL)) (- depth 1) )
)

; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.
(defun id-dfs (s depth)
	(or (or NIL (single-dfs s NIL depth)) (id-dfs s (+ 1 depth))  )
)

