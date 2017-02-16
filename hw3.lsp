;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;getpositionstate takes argument state(s) and a position label (col row)
;returns the number on tha particular position in the state
(defun getpositionstate (s col row)
	(nth col (nth row s))
)
  
;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

;containbox takes argument row and checks if a row has any boxes not on goal
;if yes, return true, else return nil
(defun containbox(row)
	(cond
		((= 2 (first row)) t)
		((= 1 (list-length row)) NIL)
		(t (containbox (cdr row)))
	)
)

;goal-state takes argument s and checks if there are any boxes not on goal in the state
;if there aren't any, return true, else return nil
(defun goal-test(s)
  (cond
	((containbox (first s)) NIL)
	((= 1 (list-length s)) t)
	(t (goal-test (cdr s)))
  )
);end defun

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;

;clone takes argument state(s)
;make a copy of a completely same state s and returns it
(defun clone (s)
	(cond
		((= 1 (list-length s)) (cons (append (car s) nil) nil))
		(t (append (cons(append (car s) nil)nil) (clone (cdr s) ) ))
	)
)

;try-move takes argument state(s) and direction(dir)
;outputs the state after s move in direction dir 

(defun try-move(s dir) ;up 1 down 3 right 2 left 4

	(setq x (first(getKeeperPosition s 0)))
	(setq y (second(getKeeperPosition s 0)))
	(setq state (clone s) )
	;keeper at col=x, row=y
	;getpositionstate (state col row)
	(cond
		;can't move out of frame
		( (and(= dir 1)(<= y 0)) (setq state NIL) )
		( (and(= dir 2)(>= x (-(list-length (car state)) 1))) (setq state NIL))
		( (and(= dir 3)(>= y (-(list-length state) 1))) (setq state NIL))
		( (and(= dir 4)(<= x 0)) (setq state NIL) )
		;can't move into wall
		( (and (= dir 1) (= 1 (getpositionstate state x (- y 1)))) (setq state NIL))
		( (and (= dir 2) (= 1 (getpositionstate state (+ x 1) y))) (setq state NIL))
		( (and (= dir 3) (= 1 (getpositionstate state x (+ y 1)))) (setq state NIL))
		( (and (= dir 4) (= 1 (getpositionstate state (- x 1) y))) (setq state NIL))
		;can't push box out of boundary
		((and (and(= dir 1)(= y 1))(= 2 (getpositionstate state x (- y 1))) ) (setq state NIL))
		((and (and(= dir 2)(= x (-(list-length (car state)) 2)))(= 2 (getpositionstate state (-(list-length (car state)) 1) y)) ) (setq state NIL))
		((and (and(= dir 3)(= y (-(list-length state) 2)))(= 2 (getpositionstate state x (-(list-length state) 1))) ) (setq state NIL))
		((and (and(= dir 4)(= x 1))(= 2 (getpositionstate state (- x 1) y)) ) (setq state NIL))
		
		((and (and(= dir 1)(= y 1))(= 5 (getpositionstate state x (- y 1))) ) (setq state NIL))
		((and (and(= dir 2)(= x (-(list-length (car state)) 2)))(= 5 (getpositionstate state (-(list-length (car state)) 1) y)) ) (setq state NIL))
		((and (and(= dir 3)(= y (-(list-length state) 2)))(= 5 (getpositionstate state x (-(list-length state) 1))) ) (setq state NIL))
		((and (and(= dir 4)(= x 1))(= 5 (getpositionstate state (- x 1) y)) ) (setq state NIL))
		
		;can't push box into wall or into another box
		((and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 1 (getpositionstate state x (- y 2))))  )(setq state NIL) )
		((and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 2 (getpositionstate state x (- y 2))))  )(setq state NIL) )
		((and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 5 (getpositionstate state x (- y 2))))  )(setq state NIL) )
		
		((and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ 1 x) y)) ( = 1 (getpositionstate state (+ 2 x) y)))  )(setq state NIL) )
		((and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ 1 x) y)) ( = 2 (getpositionstate state (+ 2 x) y)))  )(setq state NIL) )
		((and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ 1 x) y)) ( = 5 (getpositionstate state (+ 2 x) y)))  )(setq state NIL) )
		
		
		((and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ 1 y) )) ( = 1 (getpositionstate state x (+ 2 y) )))  )(setq state NIL) )
		((and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ 1 y) )) ( = 2 (getpositionstate state x (+ 2 y) )))  )(setq state NIL) )
		((and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ 1 y) )) ( = 5 (getpositionstate state x (+ 2 y) )))  )(setq state NIL) )
		
		((and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 1 (getpositionstate state (- x 2) y)))  )(setq state NIL) )
		((and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 2 (getpositionstate state (- x 2) y)))  )(setq state NIL) )
		((and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 5 (getpositionstate state (- x 2) y)))  )(setq state NIL) )
		
		((and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 1 (getpositionstate state x (- y 2))))  )(setq state NIL) )
		((and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 2 (getpositionstate state x (- y 2))))  )(setq state NIL) )
		((and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 5 (getpositionstate state x (- y 2))))  )(setq state NIL) )
		
		((and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ 1 x) y)) ( = 1 (getpositionstate state (+ 2 x) y)))  )(setq state NIL) )
		((and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ 1 x) y)) ( = 2 (getpositionstate state (+ 2 x) y)))  )(setq state NIL) )
		((and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ 1 x) y)) ( = 5 (getpositionstate state (+ 2 x) y)))  )(setq state NIL) )
		
		
		((and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ 1 y) )) ( = 1 (getpositionstate state x (+ 2 y) )))  )(setq state NIL) )
		((and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ 1 y) )) ( = 2 (getpositionstate state x (+ 2 y) )))  )(setq state NIL) )
		((and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ 1 y) )) ( = 5 (getpositionstate state x (+ 2 y) )))  )(setq state NIL) )
		
		((and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 1 (getpositionstate state (- x 2) y)))  )(setq state NIL) )
		((and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 2 (getpositionstate state (- x 2) y)))  )(setq state NIL) )
		((and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 5 (getpositionstate state (- x 2) y)))  )(setq state NIL) )
		;moving into a blank
		((and (= dir 1) (= 0 (getpositionstate state x (- y 1)))) (progn(setf (nth x (nth y state) ) 0)(setf (nth x (nth (- y 1) state) ) 3) )  )		
		((and (= dir 2) (= 0 (getpositionstate state (+ x 1) y))) (progn(setf (nth x (nth y state) ) 0)(setf (nth (+ 1 x) (nth y state) ) 3) )  )
		((and (= dir 3) (= 0 (getpositionstate state x (+ y 1)))) (progn(setf (nth x (nth y state) ) 0)(setf (nth x (nth (+ y 1) state) ) 3) )  )
		((and (= dir 4) (= 0 (getpositionstate state (- x 1) y))) (progn(setf (nth x (nth y state) ) 0)(setf (nth (- x 1) (nth y state) ) 3) )  )
		;stepping onto a goal
		((and(and (= dir 1) (= 4 (getpositionstate state x (- y 1))))(= 3 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 0)(setf (nth x (nth (- y 1) state) ) 6) )  )		
		((and(and (= dir 2) (= 4 (getpositionstate state (+ x 1) y)))(= 3 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 0)(setf (nth (+ x 1) (nth y state) ) 6) )  )
		((and(and (= dir 3) (= 4 (getpositionstate state x (+ y 1))))(= 3 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 0)(setf (nth x (nth (+ y 1) state) ) 6) )  )
		((and(and (= dir 4) (= 4 (getpositionstate state (- x 1) y)))(= 3 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 0)(setf (nth (- x 1) (nth y state) ) 6) )  )
		
		((and(and (= dir 1) (= 4 (getpositionstate state x (- y 1))))(= 6 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 4)(setf (nth x (nth (- y 1) state) ) 6) )  )		
		((and(and (= dir 2) (= 4 (getpositionstate state (+ x 1) y)))(= 6 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 4)(setf (nth (+ x 1) (nth y state) ) 6) )  )
		((and(and (= dir 3) (= 4 (getpositionstate state x (+ y 1))))(= 6 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 4)(setf (nth x (nth (+ y 1) state) ) 6) )  )
		((and(and (= dir 4) (= 4 (getpositionstate state (- x 1) y)))(= 6 (getpositionstate state x y) ))(progn(setf (nth x (nth y state) ) 4)(setf (nth (- x 1) (nth y state) ) 6) )  )
		; k->box->blank & k->box->goal
		((and(and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 0 (getpositionstate state x (- y 2))))) (= 3 (getpositionstate state x y)) )     (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 2)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ x 1) y)) (= 0 (getpositionstate state (+ x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 2)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ y 1))) (= 0 (getpositionstate state x (+ y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 2)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 0 (getpositionstate state (- x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 2)) ) )
		
		
		((and(and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 0 (getpositionstate state x (- y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 2)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ x 1) y)) (= 0 (getpositionstate state (+ x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 2)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ y 1))) (= 0 (getpositionstate state x (+ y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 2)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 0 (getpositionstate state (- x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 2)) ) )
		
		
		((and(and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 4 (getpositionstate state x (- y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 5)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ x 1) y)) (= 4 (getpositionstate state (+ x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 5)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ y 1))) (= 4 (getpositionstate state x (+ y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 5)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 4 (getpositionstate state (- x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 5)) ) )
		
		
		((and(and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 4 (getpositionstate state x (- y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 5)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ x 1) y)) (= 4 (getpositionstate state (+ x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 5)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ y 1))) (= 4 (getpositionstate state x (+ y 2))))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 5)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 4 (getpositionstate state (- x 2) y)))) (= 3 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 0 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 5)) ) )
	
		
		;;;;;
		
		((and(and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 0 (getpositionstate state x (- y 2))))) (= 6 (getpositionstate state x y)) )     (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 2)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ x 1) y)) (= 0 (getpositionstate state (+ x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 2)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ y 1))) (= 0 (getpositionstate state x (+ y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 2)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 0 (getpositionstate state (- x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 2)) ) )
		
		
		((and(and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 0 (getpositionstate state x (- y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 2)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ x 1) y)) (= 0 (getpositionstate state (+ x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 2)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ y 1))) (= 0 (getpositionstate state x (+ y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 2)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 0 (getpositionstate state (- x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 2)) ) )
		
		
		((and(and (and(= dir 1)(>= y 2))(and( = 2 (getpositionstate state x (- y 1))) (= 4 (getpositionstate state x (- y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 5)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 2 (getpositionstate state (+ x 1) y)) (= 4 (getpositionstate state (+ x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 5)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 2 (getpositionstate state x (+ y 1))) (= 4 (getpositionstate state x (+ y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 5)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 2 (getpositionstate state (- x 1) y)) (= 4 (getpositionstate state (- x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 5)) ) )
		
		
		((and(and (and(= dir 1)(>= y 2))(and( = 5 (getpositionstate state x (- y 1))) (= 4 (getpositionstate state x (- y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (- y 1) state) ) (+ (getpositionstate state x (- y 1) ) 1)) (setf (nth x (nth (- y 2) state) ) 5)) ) )
		((and(and (and(= dir 2)(<= x (-(list-length (car state)) 3)))(and( = 5 (getpositionstate state (+ x 1) y)) (= 4 (getpositionstate state (+ x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (+ x 1) (nth y state) ) (+ (getpositionstate state (+ 1 x) y) 1)) (setf (nth (+ x 2) (nth y state) ) 5)) ) )
		((and(and (and(= dir 3)(<= y (-(list-length state) 3)))(and( = 5 (getpositionstate state x (+ y 1))) (= 4 (getpositionstate state x (+ y 2))))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth x (nth (+ y 1) state) ) (+ (getpositionstate state x (+ y 1) ) 1)) (setf (nth x (nth (+ y 2) state) ) 5)) ) )
		((and(and (and(= dir 4)(>= x 2))(and( = 5 (getpositionstate state (- x 1) y)) (= 4 (getpositionstate state (- x 2) y)))) (= 6 (getpositionstate state x y)) )    (progn(setf  (nth x (nth y state) ) 4 )(progn (setf (nth (- x 1) (nth y state) ) (+ (getpositionstate state (- x 1) y) 1)) (setf (nth (- x 2) (nth y state) ) 5)) ) )
	)
	state
)


;next-states takes argument state(s)
;returns all possible next states in a list
;use try-move to try in 4 different directions
(defun next-states (s)
	(cleanUpList (cons (try-move s 1) (cons (try-move s 2) (cons (try-move s 3) (cons (try-move s 4) NIL) )  ) ))
 );

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;

;countboxes takes argument row, computes the number of misplaced boxes in the row
;it's a helper function of h1
(defun countbox(row)
	(cond
		((and(= 1 (list-length row))(= 2 (car row))) 1)
		((= 1 (list-length row)) 0)
		( (= 2 (car row)) (+ (countbox (cdr row)) 1) )
		( t  (countbox (cdr row)) )
	)
)
;h1 takes the argument state(s)
;then computes the number of misplaced boxes in s, with the help of countbox
(defun h1 (s)
	(cond
		((= 1 (list-length s)) (countbox (car s) ) )
		(t (+ (h1 (cdr s) ) (countbox (car s) ) ))
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

;list-of-boxes-and-goals-inrow takes arguments:
;a row(s), an iterator over the row(col), the label of the row(rownum), list of boxes up to now(boxes), and list of goals up to now(goals)
;return ((boxes) (goals)). boxes and goals would contain the respective coordinates of the boxes and rows in the rownum-th row
(defun list-of-boxes-and-goals-inrow (s col rownum boxes goals)
	(cond
		((not s) (cons boxes (cons goals nil)) ) ;( (list of boxes) (list of goals))
		(
			(isBox (car s))
			(list-of-boxes-and-goals-inrow (cdr s) (+ 1 col) rownum (append boxes (cons (cons col (cons rownum nil)) nil)) goals)
		)	
		(
			(isStar (car s))
			(list-of-boxes-and-goals-inrow (cdr s) (+ 1 col) rownum boxes (append goals (cons (cons col (cons rownum nil)) nil)))
		)	
		(t (list-of-boxes-and-goals-inrow (cdr s) (+ 1 col) rownum boxes goals ) )
	)
)

;list-of-boxes-and-goals-h is helper function to list-of-boxes-and-goals
;takes argument state(s), an iterator over the rows in the state(rownum), list of boxes up to now(boxes), and list of goals up to now(goals)
;return ((boxes) (goals)). boxes and goals would contain the respective coordinates of the boxes and rows in the state
(defun list-of-boxes-and-goals-h (s rownum boxes goals)
	(cond
		((= 1 (list-length s) ) (list-of-boxes-and-goals-inrow (car s) 0 rownum boxes goals)  )
		(t
			(cons
				(append (first (list-of-boxes-and-goals-inrow (car s) 0 rownum boxes goals)  ) (first (list-of-boxes-and-goals-h (cdr s) (+ rownum 1) boxes goals) ) )
				(cons (append (second (list-of-boxes-and-goals-inrow (car s) 0 rownum boxes goals) ) (second (list-of-boxes-and-goals-h (cdr s) (+ rownum 1) boxes goals) ) ) nil)
			)	
		)
	)
)

;list-of-boxes-and-goals takes argument state(s)
;returns ((boxes) (goals) ). boxes and goals would contain the respective coordinates of the boxes and rows in the state
(defun list-of-boxes-and-goals (s)
	
	(list-of-boxes-and-goals-h s 0 '() '() )
)

;distance takes to positions point(x1 y1) and point2(x2 y2)
;returns |x1-x2| + |y1-y2|
(defun distance (point1 point2)
	(cond 
		((> (first point1) (first point2)) (setq xdist (- (first point1) (first point2))))
		(t (setq xdist (- (first point2) (first point1))) )
	)
	(cond 
		((> (second point1) (second point2)) (setq ydist (- (second point1) (second point2))))
		(t (setq ydist (- (second point2) (second point1))) )
	)
	(+ xdist ydist)
)

;nearest-goal-dist takes a position of a box(box), and a list of positions of goals(goals)
;find the smallest distance of a box to a goal in the list
(defun nearest-goal-dist (box goals)
	(cond 
		((= 1 (list-length goals) ) (distance box (car goals) )  )
		(( >= (nearest-goal-dist box (cdr goals)) (distance box (car goals) ) ) (distance box (car goals) ) )
		(t (nearest-goal-dist box (cdr goals)) )
	)
)

;sum-of-nearest-goal-dist takes argument a list of boxes' positions (boxes), and a list of goals' postions (goals)
;return the sum of (nearest-goal-dist box goals) for all box in boxes
(defun sum-of-nearest-goal-dist (boxes goals)
	(cond
		((= 1 (list-length boxes)) (nearest-goal-dist (car boxes) goals ) )
		(t (+ (nearest-goal-dist (car boxes) goals) (sum-of-nearest-goal-dist (cdr boxes) goals )))
	)
)

;nearest-keeper-to-box-dist takes the position of keeper(keeper) and a list of positions of boxes(boxes)
;return the smallest distance between the keeper and any box in the boxes list
(defun nearest-keeper-to-box-dist (keeper boxes)
	(cond 
		((= 1 (list-length boxes) ) (distance keeper (car boxes) )  )
		(( >= (nearest-keeper-to-box-dist keeper (cdr boxes)) (distance keeper (car boxes) ) ) (distance keeper (car boxes) ) )
		(t (nearest-keeper-to-box-dist keeper (cdr boxes)) )
	)
)

;takes argument state(s) and box
;return true if box is stuck, else return 0
(defun stuckbox (s box)
	(cond
		((= 0 (list-length box)) nil)
		((and(= 0 (first box)) (= (- (list-length s) 1) (second box))) t)
		((and(= 0 (first box)) (= 0 (second box))) t)
		((and(= (- (list-length (car s) ) 1) (first box)) (= 0 (second box))) t)
		((and(= (- (list-length (car s) ) 1) (first box)) (= (- (list-length s) 1) (second box))) t)
		((and(or (= 0 (first box))  (= (- (list-length (car s) ) 1) (first box))) (or (= 1 (getpositionstate s (first box) (- (second box) 1))) (= 2 (getpositionstate s (first box) (- (second box) 1)))))t)
		((and(or (= 0 (first box))  (= (- (list-length (car s) ) 1) (first box))) (or (= 1 (getpositionstate s (first box) (+ (second box) 1))) (= 2 (getpositionstate s (first box) (+ (second box) 1)))))t)
	
		((and(or (= 0 (first box))  (= (- (list-length (car s) ) 1) (first box))) (or (= 5 (getpositionstate s (first box) (+ (second box) 1))) (= 5 (getpositionstate s (first box) (- (second box) 1)))))t)
	
		((and(or (= 0 (second box))  (= (- (list-length s ) 1) (second box))) (or (= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (- (first box) 1) (second box) ))))t)
		((and(or (= 0 (second box))  (= (- (list-length s ) 1) (second box))) (or (= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (+ (first box) 1) (second box) ))))t)
		
		((and(or (= 0 (second box))  (= (- (list-length s ) 1) (second box))) (or (= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (- (first box) 1) (second box) ))))t)
		((or (= 0 (first box))  (= (- (list-length (car s) ) 1) (first box))) nil)
		((or (= 0 (second box))  (= (- (list-length s ) 1) (second box))) nil)
		((and(= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (+ (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		
		((and(= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (+ (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (+ (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		
		((and(= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (- (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (- (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (- (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (- (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (+ (second box) 1) ) ) ) t)
		
		((and(= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 1 (getpositionstate s (- (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (- (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 2 (getpositionstate s (- (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (- (first box) 1) (second box) )) (= 1 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (- (first box) 1) (second box) )) (= 2 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		((and(= 5 (getpositionstate s (- (first box) 1) (second box) )) (= 5 (getpositionstate s (first box) (- (second box) 1) ) ) ) t)
		(t nil)
	)
)

;stuckstate takes argument state(s) and a list of coordinates of boxes(boxes)
;return true if at least one of the box is stuck
;else return nil
(defun stuckstate (s boxes)
	(cond		
		((stuckbox s (car boxes)) t)
		((= 1 (list-length boxes)) nil)
		(t (stuckstate s (cdr boxes) ) )
	)
)



;h304758638 takes a state 
;and check if the state s is stuck or not. If stuck, return 2000, if not stuck,
;return the sum of (sum-of-nearest-goal-dist boxlist goallist) and (nearest-keeper-to-box-dist (getKeeperPosition s 0) boxlist)
(defun h304758638 (s)
	;(boxes): (first (list-of-boxes-and-goals s))
	;(goals): (second (list-of-boxes-and-goals s))
	(setq boxlist (first (list-of-boxes-and-goals s)) )
	(setq goallist (second(list-of-boxes-and-goals s)) )
	(cond
		( (goal-test s) 0 )
		((stuckstate s boxlist) 2000)
		(t (+ (sum-of-nearest-goal-dist boxlist goallist) (nearest-keeper-to-box-dist (getKeeperPosition s 0) boxlist)) )
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#
 
 
;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
