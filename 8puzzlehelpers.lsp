#|
        ***** 8puzzlehelpers.LSP *****

8-Puzzle: slide tiles around the board until the board has the form:

		1 2 3
		8 0 4
		7 6 5

The zero denotes a blank space that any number can slide into

Board representation:	( 1 2 3 4 5 6 7 8 0)

This file contains functions for storing and handling 8_board puzzles

Author: Zachary A Christy
Class:  CSC447/547 Artificial Intelligence
Date:   Spring 2018

|#
 
(load "solvable.lsp") 			;Load functions for solvability
(load "heuristics.lsp")

;node stores the board state, 
(defstruct node state cost parent depth)

;node stores the board state for aStar
(defstruct heurNode state parent fn gn hn)

;size of board, passed in via command line arguments. default 9
(defvar *N* 9)

;root of N, stored so we dont have to re-compute every time
(defvar *rootN* 3)

;indexes denoting limits of blank tile motion (i.e. up, down, right)
;left is always zero so we dont have to calculate
(defvar *downlimit* 6)
(defvar *uplimit* 2)
(defvar *rightlimit* 2)

;goal state of board
(defvar *goal-state* '(1 2 3 8 0 4 7 6 5))


;input puzzle
(defvar *puzzle* nil)


(defun equal-states (node1 node2)
    "(equal-states n1 n2) - return T if node1 and node2 have the same state"
    (equal (node-state node1) (node-state node2))
)


(defun getsuccessors (x) 	
	"returns a list containing x's succesors"
	( let ( (zeropos (position 0 x) )
			(templist ()))

		;Check if we can move DOWN
		(cond ((< zeropos *downlimit*) 
			;add DOWN successor to succlist			
			(setf templist (append templist (list (swapzerobyindex x (+ zeropos *rootN*)))))))

		;Check if we can move 
		(cond ((> zeropos *uplimit*) 
			;add up successor to list	
			(setf templist (append templist (list (swapzerobyindex x (- zeropos *rootN*)))))))

		;Check if we can move RIGHT
		(cond ((< (mod zeropos *rootN*) *rightlimit*) 
			;add right successor to list
			(setf templist (append templist (list (swapzerobyindex x (+ zeropos 1)))))))

		;Check if we can move LEFT
		(cond ((> (mod zeropos *rootN*) 0) 
			;add left successor to list
			(setf templist (append templist (list (swapzerobyindex x (- zeropos 1)))))))


		;return a copy of the list
		(copy-list templist)

	)
)

(defun printboard (board)
	"prints a puzzle out to the terminal"
	(format t "~C" #\newline)
		
	(dotimes (i *rootN*) 
		(format t "~%~C" #\tab)
		(dotimes (j *rootN*)
			( cond
				;use tabs if we have 2 digit numbers
				((> *N* 9) (format t "~D~C" (nth (+ j (* i *rootN*)) board) #\tab))

				;otherwise use spaces
				(t (format t "~D " (nth (+ j (* i *rootN*)) board)))
			)
		)
	)
	
)


(defun swaptilesbyval ( board t1 t2)	
	"returns a list with the 2 specified tiles swapped"
   ( let ((newlist (copy-list board)))		;copy list to preserve input argument after swap
	(rotatef (car (member t1 newlist)) (car (member t2 newlist)) ) 
	newlist		;Return the new list
	)
)

( defun swapzerobyindex (board index)
	"returns a list with the blank moved to 'index'"
	( let ( (newlist (copy-list board)) )			;copy list to preserve input argument after swap

	(rotatef (car (member 0 newlist)) (nth index newlist) ) 
	newlist		;Return the new list
	)
)

(defun goal-state? (state)
    "(goal-state? state) - returns T if state is goal (i.e. solved 8-tile puzzle)"
     (equal state *goal-state*)
)

(defun readpuzzle (filename)
	"read puzzle from specified file"
	( let( (readval nil) (temppuzzle nil) (filestream (open filename)) ) 

	(setf temppuzzle nil)

	;ensure file stream opened properly
	(cond ((not filestream) (format t "failed to open file!") (close filestream) (return-from readpuzzle)))

	;read N elements from file
	(dotimes (i *N*) 
		
		;get next character in file
        (setf readval (read filestream))

		;if the value we read was not nil
		(cond ((not readval) (format t "input file did not contain enough data.") (close filestream) (return-from readpuzzle)))


		;append the read value to the puzzle
		(setf temppuzzle (append temppuzzle (list readval)) )

	)
	;safely close the file
	(close filestream)

	;return the puzzle list
	temppuzzle
	)
)

( defun read8puzzlein ()
	"reads in 8-puzzle values"

	(let ((readval ()))
		(format t "Enter 9 digit puzzle configuration: ")

		(dotimes (i *N*)
			(setf readval (read))
			;only add character if  it's not already in the puzzle and is between 0 and 9
			( cond ((or (member readval *puzzle*) (or (> readval 9) (< readval 0))) 
				(format t "invalid input!") (return-from read8puzzlein))
			)
			(setf *puzzle* (append *puzzle* (list readval)))	
		)
	)
)

;sets the goal state based on the value of *N*
(defun setgoalstate ()
	"puzzles are assumed to be solved when their numbers 'spiral' around their centers"

	;create and initialize local variables
	;iterator - array index of space being assigned
	;direction - direction spiral is going
	(let* ((tempgoal (make-array *N*) ) (iterator 0) (direction 0))

	;re-initialize goal state as an empty list.
	(setf *goal-state* ())

	;ensure rootN is correct value
	(setf *rootN* (isqrt *N*))

	;set limits to reflect new board
	(setf *downlimit* (- *N* *rootN*))
	(setf *rightlimit*(1- *rootN*))
	(setf *uplimit*   (1- *rootN*))

	;move the iterator in a straight line until it hits an edge or a filled space
	;if it hits one, check if we've filled all spaces, and drop out
	(dotimes (i *N*)
		;check if we need to change direction
		(cond 
			;if we're going right and hit an edge or a number
			(( and (= direction 0) (or (= iterator (1- *rootN*)) ( aref tempgoal (+ iterator 1)) )) (setf direction 1))
			
			;if we're going down and hit an edge or number
			(( and (= direction 1) (or (= iterator (- *N* 1)) ( aref tempgoal (+ iterator *rootN* )) )) (setf direction 2))

			;if we're going left  and hit an edge or a number
			(( and (= direction 2) (or (= iterator (- (+ 1 *rootN*))) ( aref tempgoal (- iterator 1)) )) (setf direction 3))

			;if we're going up and hit an edge or a number
			(( and (= direction 3) (or (= iterator 0) ( aref tempgoal (- iterator *rootN*)) )) (setf direction 0))
		)

		;set array at iterator to proper value
		(setf (aref tempgoal iterator) (mod (+ 1 i) *N*))

		;increment the iterator
		(cond
			;iterator should move right
			( (= direction 0) (setf iterator (+ iterator 1)) )
		
			;iterator should move down
			( (= direction 1) (setf iterator (+ iterator *rootN*)) )

			;iterator should move left
			( (= direction 2)  (setf iterator (- iterator 1)) )

			;iterator should move up
			( (= direction 3)  (setf iterator (- iterator *rootN*)) )
		)
	)
	
	;copy the tempgoal array into the global "puzzle"
	(dotimes (i *N*) 
		(
			setf *goal-state* (append *goal-state* (list (aref tempgoal i)))
		)
	)

	*goal-state*
	)
)



(defun print-solution (SOLUTION-PATH)
	"Function used to print out a solution path in a neat form"
	(let ((boardsperrow  (floor 40 (1+ *N*)))	;how many boards on each row
		 (spacespertile (ceiling (log *N* 10) 1));max number of spaces needed for any space
		 (tileval 0)
		 (currentboard 0))

	(format t "~C~C" #\newline #\newline)

	;iterate through rows of solution output
	(dotimes (i (ceiling (length SOLUTION-PATH) boardsperrow))

		;iterate through rows in boards
		(dotimes (j *rootN*)

			;print an indent for each new row of boards
			(format t "~C" #\tab)

			;iterate through each board
			(dotimes (k boardsperrow)

				;set board value
				(setf currentboard (+ k (* i boardsperrow)) )

				;dont try to print boards that dont exist
				(unless (> (+ k (* i boardsperrow )) (- (length SOLUTION-PATH) 1))
					;print the elements of the jth row of the (k + i*boardsperrow)th board
					(dotimes (l *rootN*)
						(setf tileval (nth (+ l (* *rootN* j)) (nth (+ k (* i boardsperrow)) SOLUTION-PATH)))

						;print tile value
						(cond ((= tileval 0) (format t " "))
							  (t (format t "~D"  tileval))
							  )

						print extra spaces
						(cond ((> tileval 0) (dotimes (m (- spacespertile (log tileval 10) )) (format t " ")))
							  (t (dotimes (m (- spacespertile 0)) (format t " ")))
						)
					)

					;spaces between rows contain either an arrow or spaces
					(cond ((and (not (= currentboard (-(length SOLUTION-PATH) 1))) (= (floor *rootN* 2) j)) (format t "-> "))
						  (t (format t "   "))
				    )
				)
			)
			(format t "~C" #\newline))
		(format t "~C" #\newline))
	)
)



(defun puzzlecmp(arg1 arg2)
	"comparison function used for sorting"
	(< (node-cost arg1) (node-cost arg2))
)