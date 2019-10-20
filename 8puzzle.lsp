#|
        ***** 8puzzle.LSP *****

8-Puzzle: slide tiles around the board until the board has the form:

		1 2 3
		8 0 4
		7 6 5

The zero denotes a blank space that any number can slide into

Board representation:	( 1 2 3 4 5 6 7 8 0)

This file contains functions for storing and handling 8_board puzzles

Author: Zachary A Christy, Jeremy Goens
Class:  CSC447/547 Artificial Intelligence
Date:   Spring 2018

|#

;8-puzzle functions and heuristics
(load "8puzzlehelpers.lsp")
(load "searches.lsp")

;global limit for number of generated nodes.
;lets us move on to more efficient searches if less efficient ones wont work.
(setf *NODE-LIMIT* 200000)


(defun main (args) 
	"main function, parses command line args and calls search functions"

	;reset globals to ensure predictable behavior on repeated runs
	(setf *puzzle* nil)	

	(let ((infile nil)
		  (inval "Y")
		  (newheur nil)) 

	;get filename if available.
	(cond ((car args) (setf infile (car args)) (setf args (cdr args))) 
		  (t 
			;prompt for 8 number input file
			(read8puzzlein)	
		   ))

	;set puzzle size to nearest perfect square, or 9 if no argument is specified
	(cond ((car args) (setf *N* (* (isqrt (1+ (parse-integer (car args)))) (isqrt (1+ (parse-integer (car args))))))) 
		(t (setf *N* 9)))

	;saves computation of square root for many functions
	(setf *rootN* (isqrt *N*))

	(cond (infile (setf *puzzle* (readpuzzle infile))))

	;set goal state based on N
	(setgoalstate)

	;output puzzle and goal state if we're using unconventional dimensions
	(cond ((not (= *N* 9)) (format t "N = ~D~CPuzzle read from file:" *N* #\newline) (printboard *puzzle*)
		(format t "~C~C" #\newline #\newline)
		(format t "Goal state:")
		(printboard *goal-state*)
		(format t "~C~C" #\newline #\newline))
	)
	

	;ensure puzzle is solvable:
	(cond ( (and (not (solvable *puzzle*)) (= 9 *N*)) (format t "Puzzle not solvable! Terminating...") (return-from main nil) ))

	;Solve puzzle via A* algorithm given an inadmissible hueristic
	;This heuristic almost always result in a faster solution on harder problems,
	;so we use it first. The following 2 can solve the worst puzzle quickly:
	;heur-6: (Exponentiating sum of manhattan distances)
	;heur-8: (Exponentiating Nilsson score)
	;(boolean variable denotes admissibility)
	(astar_8puz *puzzle* 'heur-6)

	;Solve puzzle via A* algorithm given an admissible hueristic
	;heur-4: (Sum of manhattan distances of tiles from their desired spaces)
	(astar_8puz *puzzle* 'heur-4)

	;Solve puzzle via Depth-First iterated deepening search
	(dfid_8puz *puzzle*)

	;Solve puzzle via Breadth-First search
	(bfs_8puz *puzzle*)


	(do () ((not (equal inval "Y")))
		(format t "Test additional heuristic? [Y/N]: ")
		(setf inval (string (read)))
		(when (equal inval "Y") (format t "~%Enter desired heuristic (1-8): ")
			(setf newheur (read))
			(cond ((eq newheur 1) (astar_8puz *puzzle* 'heur-1))
				  ((eq newheur 2) (astar_8puz *puzzle* 'heur-2))
				  ((eq newheur 3) (astar_8puz *puzzle* 'heur-3))
				  ((eq newheur 4) (astar_8puz *puzzle* 'heur-4))
				  ((eq newheur 5) (astar_8puz *puzzle* 'heur-5))
				  ((eq newheur 6) (astar_8puz *puzzle* 'heur-6))
				  ((eq newheur 7) (astar_8puz *puzzle* 'heur-7))
				  ((eq newheur 8) (astar_8puz *puzzle* 'heur-8))
				  (t (format t "Invalid heuristic choice.~%"))
			)
		)

	)
	)

)

; Member-state looks for a node on the node-list with the same state.
(defun member-state (state node-list)
    "(member-state state node-list) - searches node-list for state, and returns node"
    ;would normally use 'member' but function searches based on state, not node.
    ;could inpmement the equality check here as a special comparison function, 
    ;but not really worth it since we only need it once.
    (dolist (node node-list)
            (when (equal state (node-state node)) (return node))
        )
)

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (node node-list)
    "(build-solution node node-list) - returns solution path"
    (do
        ((path (list (node-state node))))        ; local loop var
        ((null (node-parent node)) path)         ; termination condition

        ; find the parent of the current node
        (setf node (member-state (node-parent node) node-list))

        ; add it to the path
        (setf path (cons (node-state node) path))
    )
)
;main function call after all other function definitions
(main *args*)
