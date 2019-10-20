#|
        ***** heuristics.LSP *****

8-Puzzle: This file contains heuristics used by A* search 
	 	  for 8-tile puzzles. 

Author: Zachary A Christy
Class:  CSC447/547 Artificial Intelligence
Date:   Spring 2018

|#


(defun factorial (x)
	(cond ((= x 0) 1)
		  (t (* x (factorial (1- x)) ))
	)
)

(defun heurdata (heur)
	"prints data about given heuristic"
	(cond 
		((equal heur 'heur-1) (format t "Zero heuristic, Admissible"))
		((equal heur 'heur-2) (format t "Out of place tiles, Admissible"))
		((equal heur 'heur-3) (format t "Square of out of place tiles, Inadmissible"))
		((equal heur 'heur-4) (format t "Sum of Manhattan distances, Admissible"))
		((equal heur 'heur-5) (format t "Factorial of Manhattan distances, Inadmissible"))
		((equal heur 'heur-6) (format t "Exponential of Manhattan distances, Inadmissible"))
		((equal heur 'heur-7) (format t "Nilsson Score, Inadmissible"))
		((equal heur 'heur-8) (format t "Exponential of Nilsson score, Inadmissible"))
		(t (format t "Unknown heuristic."))
	)
)

;HEURISTIC FUNCTIONS

(defun heur-1 (state) 
	"Simplest heuristic call. Returns Zero. Admissible"
	0
)

(defun heur-2 (state) 
	"Returns number of out-of-place tiles. Admissible"
	(let ((numtiles 0))
		(dotimes (i *N*)
			(unless (= (nth i state)(nth i *goal-state*)) (setf  numtiles (1+ numtiles)))
		)

		numtiles
	)
)

(defun heur-3 (state)
	"functionally identical to heur-2 but uses square of values. Inadmissible"
	(expt (heur-2 state) 2)
)

(defun heur-4 (state)
	"calculates distance based on manhattan distance of current tile positions from goal. Admissible"
	 (let ((score 0) 
	 	   (rowpos_g 0)
	 	   (columnpos_g 0)
	 	   (rowpos_t 0)
	 	   (columnpos_t 0))

	 	(dolist (tile state)
	 		;row position found with integer division
	 		(setf rowpos_t (floor (position tile state :test #'=) *rootN*))
	 		(setf rowpos_g (floor (position tile *goal-state* :test #'=) *rootN*))

	 		;column position found with modular division
	 		(setf columnpos_t (mod (position tile state :test #'=) *rootN*))
	 		(setf columnpos_g (mod (position tile *goal-state* :test #'=) *rootN*))

	 		(setf score (+ score (abs (- rowpos_g rowpos_t)) (abs (- columnpos_g columnpos_t))))
	 	)

	 	score
	 )
)

(defun heur-5 (state)
	"Factorial of Manhattan distance. Inadmissible"
	 (factorial (heur-4 state))
)

(defun heur-6 (state)
	"Exponential of manhattan distance. Inadmissible"
	(exp (heur-4 state))
)

(defun heur-7 (state)
	"computes the Nilsson score of the 8 puzzle. Inadmissible"
	(let ((sequencescore 0)
		  (prev (car state))	;previous value is 1st space
		  (iterator 1) 			;start at the 2nd space
		  (direction 0))		;start moving right

	(dotimes (i (- (* *rootN* 3) 4))
		;check if we need to change direction
		(cond 
			;if we're going right and hit an edge
			(( and (= direction 0)  (= iterator (1- *rootN*)) ) (setf direction 1))
			
			;if we're going down and hit an edge
			(( and (= direction 1) (= iterator (- *N* 1))) (setf direction 2))

			;if we're going left  and hit an edge
			(( and (= direction 2) (= iterator (- (1+ *rootN*)))) (setf direction 3))
		)

		;increment score if value is not in sequence
		(unless (and (= (nth iterator state) (1+ prev)) (not(= prev 0))) (setf sequencescore (+ sequencescore 2)))

		;set new previous value
		(setf prev (nth iterator state))

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
	;if the empty space is not at the center, add 1
	(unless (= 0 (nth (floor *N* 2) state)) (setf sequencescore (1+ sequencescore)))

	;add sequence score to manhattan distances to get Nilsson score
	(+ sequencescore (heur-4 state))
	)
)

(defun heur-8 (state)
	"Expoinential of Nilsson distance. Inadmissible"
	(exp (heur-7 state))
)
