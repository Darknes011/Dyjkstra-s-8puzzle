#|
        ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given 8-puzzle position is solvable, NIL otherwise.

Usage:    (solvable L)
          where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
             A.P.Domoryad, Macmillan, 1964.

Author: John M. Weiss, PhD
Class:  CSC447/547 Artificial Intelligence
Date:   Spring 2018

Modifications:
|#

; global *flag*
(defvar *flag*)

(defun solvable (L)
    "(solvable L) - returns T if list L contains a solvable 8-puzzle position"
    (setf *flag* nil)
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)

(defun disorder (elem L)
    "(disorder elem L) - helper function for solvable routine"
    (cond
        ((eq (car L) elem))
        ((> (car L) elem)
            (setf *flag* (not *flag*))
            (disorder elem (cdr L))
        )
        (t (disorder elem (cdr L)))
    )
)
