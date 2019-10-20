#|
        ***** searches.LSP *****


Description: This file contains functions for sstate space searches

Searches are fundamentally based on OPEN and CLOSED lists.
These lists are built out of hash table entries. This allows
constant time searches of the open and closed lists which
greatly increases search time efficiency without significant 
additional space requirements:

Example:

N# = Node #
S# = State #


LIST      | N1 | -> | N2 | -> | N3 | -> | N4 | -> | N5 |
            ^         ^         ^         ^         ^
            |         |         |         |         |
HASHTABLE   S1        S2        S3        S4        S5 

In this way, the list maintains the node ordinality, but
the board states can be used as keys to access the nodes 
within the lists in constant time. (i.e. No linear search
via 'member' is required.)  

Author: Zachary A Christy, Jeremy Goens
Class:  CSC447/547 Artificial Intelligence
Date:   Spring 2018

|#  

(load "heuristics.lsp")

(defun astar_8puz (start heuristic)
    "searches for puzzle solution using A* algorithm"
    ;initialize local variables
    (let* (
            (SOLUTION-PATH nil) 
            (curNode (make-node :state start :cost 0 :parent nil :depth 0)) 
            (CLOSED nil) 
            (OPEN (list curNode))
            (distinct 0)
            (generated 0)
            (memopen nil)
            (memclosed nil)
            (opentable (make-hash-table :test #'equal))     ;keeping a hash table of open and closed states
            (closedtable (make-hash-table :test #'equal))   ;means we dont have to call 'member' every time
          ) 
    

    (format t "~CA* graph search (heuristic: " #\newline)
    (heurdata heuristic)
    (format t ")~%--------------------~%")

    (do* () ;No need for iteration variables as we're using local function variables

        ;check if we've reached our goal, build solution if we have.
        ((goal-state? (node-state curNode)) (setf SOLUTION-PATH (build-solution curNode CLOSED)))

        (when (null OPEN) 
            (format t "No solution path found via A*.~C" #\newline) 
            (return-from astar_8puz nil)
        )             

        (when (>  generated *NODE-LIMIT*)
            (format t "Memory Limit exceeded after generating ~D Nodes.~C" generated #\newline) 
            (return-from astar_8puz nil)
        )   ; too many paths, terminate early

        ;next node is at front of list
        (setf curNode (car OPEN))
        
        ;remove node from open list and table
        (setf OPEN (cdr OPEN))
        (remhash (node-state curNode) opentable) 

        ;add node to closedtable
        (setf (gethash (node-state curNode) closedtable) curNode)   

        ;add hash table values to list
        (setf CLOSED (cons (gethash (node-state curNode) closedtable) CLOSED))

    ;iterate through list of successors
    (dolist (successor (getsuccessors(node-state curNode)))

            ;keep track of all generated nodes
            (setf generated (+ 1 generated))

            ;new cost is (previous cost + 1) + (heuristic)
            (setf child (make-node  :state successor 
                                    :parent (node-state curNode) 
                                    :cost (+ 1 (node-depth curNode) (funcall heuristic successor )) 
                                    :depth (1+ (node-depth curNode))) ;depth may factor into certain heuristics, or depth bounds
            )   

                    ;use the state value to see if the child is in the open or closed list.
                    (setf memopen   (gethash (node-state child) opentable))
                    (setf memclosed (gethash (node-state child) closedtable))

                    (cond (memopen  ;test if child is on the open list
                            ;if the child contains a less expensive route to this state
                            (when (< (node-cost child) (node-cost memopen)) 
                                
                                (setf (gethash successor opentable) child)                          ;re-assign new value in table
                                (sort OPEN #'puzzlecmp)
                            )                                           
                          )

                          (memclosed    ;test if child is on the closed list
                            ;test if the child contains a less expensive route to this state
                            (when (< (node-cost child) (node-cost memclosed)) 
                                (remove (gethash successor closedtable) CLOSED)                     ;remove old node from list
                                (remhash successor closedtable)                                     ;remove old node from table

                                ;adding the hash table contents to list means list works 
                                ;exactly the same, but list gains constant time access by value.
                                (setf (gethash successor opentable) child)                          ;assign new value in table
                                (setf OPEN (cons (gethash successor opentable) OPEN))               ;add child to open list
                                (sort OPEN #'puzzlecmp)
                            )
                          )
                          
                          (t ;if child is not a member of open or closed table  

                            ; add it to the OPEN table
                            (setf (gethash successor opentable) child)

                            ;Build the open list out of entries in hash table                     
                            (setf OPEN (cons (gethash successor opentable) OPEN))

                            ;sort list
                            (sort OPEN #'puzzlecmp)
                            (setf distinct (+ 1 distinct))          ;keep track of distinct nodes
                            )

                    )

        )

    )

    (format t "~%Solution found in ~D moves.~C" (- (length SOLUTION-PATH) 1) #\newline)

    (format t "~D Nodes generated. (~D distinct nodes), ~D nodes expanded." 

        generated                                       ;open and closed contain all generated nodes
        distinct                                        ;number of distinct moves found
        (length CLOSED)                                 ;expanded nodes are on closed list
    )

    ;print out boards in solution path
    (print-solution SOLUTION-PATH)
    )

    ;return true to keep error message from appearing
    t
)

(defun bfs_8puz (start) 
    "searches for puzzle solution using breadth-first search"

    ;initialize local variables
    (let* (
            (SOLUTION-PATH nil) 
            (curNode (make-node :state start :cost 0 :parent nil :depth 0)) 
            (CLOSED nil) 
            (OPEN (list curNode))
            (distinct 0)
            (generated 0)
            (opentable (make-hash-table :test #'equal))     ;keeping a hash table of open and closed states
            (closedtable (make-hash-table :test #'equal))   ;means we dont have to call 'member' every time
            (currentdepth 1)
          ) 
    

    (format t "~CBFS graph search~%--------------------~%Current Depth:  " #\newline)

    (setf (gethash (node-state curNode) opentable) t)

    (do* () ;No need for iteration variables as we're using local function variables
        ;check if we've reached our goal, build solution if we have.
        ((goal-state? (node-state curNode)) (setf SOLUTION-PATH (build-solution curNode CLOSED)))

        (when (null OPEN) (format t "~%No solution found via BFS~C" #\newline)(return-from bfs_8puz nil)) 

    (when (> generated *NODE-LIMIT*)
        (format t "Memory Limit exceeded after generating ~D Nodes.~C" generated #\newline) 
        (return-from bfs_8puz nil)) ; too many paths, terminate early

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (remhash (node-state curNode) opentable)    ;remove node from opentable

        (setf (gethash (node-state curNode) closedtable) t) ;add node to closedtable
        (setf CLOSED (cons curNode CLOSED))
        

    ;iterate through list of successors
    (dolist (successor (getsuccessors(node-state curNode)))
            ;keep track of all generated nodes
            (setf generated (+ 1 generated))

            ; for each child node, set parent to current node, set state to a successor, set cost to parent cost + 1
            (setf child (make-node :state successor :parent (node-state curNode) :cost (+ 1 (node-cost curNode)) :depth (1+ (node-depth curNode))))

            ;if node is not open or closed, evaluate it.
            (when (and (not (gethash successor opentable)) (not (gethash successor closedtable)))
                ; add it to the OPEN list
                (setf OPEN (append OPEN (list child)))  ;add child to open list
                (setf distinct (+ 1 distinct))          ;keep track of distinct nodes
                (setf (gethash successor opentable) t)  ;note that the state is in the open list
                (when (> (node-depth child) currentdepth) 
                       ;live depth limit readout helps monitor progress on harder puzzles
                       (dotimes (i (log currentdepth 10)) (format t "~C" #\backspace))
                       (format t "~D" currentdepth))
                       (setf currentdepth (node-depth child))
            )

            
        )

    )

    (format t "~%Solution found in ~D moves.~C" (- (length SOLUTION-PATH) 1) #\newline)

    (format t "~D Nodes generated. (~D distinct nodes), ~D nodes expanded." 

        generated                                       ;number of generated nodes
        distinct                                        ;number of distinct moves found
        (length CLOSED)                                 ;expanded nodes are on closed list
    )

    ;print out boards in solution path
    (print-solution SOLUTION-PATH)
    )

    ;return true
    t
)

(defun dfid_8puz (start) 

    ;initialize local variables
    (let* (
            (SOLUTION-PATH nil) 
            (curNode (make-node :state start :cost 0 :parent nil :depth 0)) 
            (CLOSED nil) 
            (OPEN (list curNode))
            (distinct 0)
            (depthlimit 1)
            (lastdepth -1)
            (currentdeepest 0)
            (generated 0)
            (memopen nil)
            (memclosed nil)
            (opentable (make-hash-table :test #'equal))     ;keeping a hash table of open and closed states
            (closedtable (make-hash-table :test #'equal))   ;means we dont have to call 'member' every time
          ) 

        (format t "~CDFID graph search~%--------------------~%Current Depth Limit:  " #\newline)

        (setf (gethash start opentable) curNode)        

        (do* () ;No need for iteration variables as we're using local function variables
            ;check if we've reached our goal, build solution if we have.
            ((goal-state? (node-state curNode)) (setf SOLUTION-PATH (build-solution curNode CLOSED)))

            ;if there are no available paths, and incrementing the depth limit didnt result in new nodes, no solution can be found
            ;otherwise, increment the depth limit and reset local variables
            (cond ((and (null OPEN) (= currentdeepest lastdepth)) (format t "No solution path found via DFID") (return-from dfid_8puz nil)) )


            ;when we have exhausted our paths, reset local variables
            (when (null OPEN) 
                       (setf depthlimit (1+ depthlimit))                    ;set new depth limit

                       ;live depth limit readout helps monitor progress on harder puzzles
                       (dotimes (i (log depthlimit 10)) (format t "~C" #\backspace))
                       
                       (format t "~D" depthlimit)
                       (setf curNode (make-node :state start :cost 0 :parent nil :depth 0))
                       (setf distinct 0)
                       (setf OPEN (list curNode))
                       (setf CLOSED nil)
                       (setf lastdepth currentdeepest)
                       (setf generated 0)
                       (setf opentable (make-hash-table :test #'equal))     ;re-initialize hash tables
                       (setf closedtable (make-hash-table :test #'equal))
                       (setf (gethash start opentable) curNode) ;put new node back in hash table
            )

            ; get current node from OPEN, update OPEN and CLOSED

            (setf curNode (car OPEN))
            
            (setf OPEN (cdr OPEN))
            (remhash (node-state curNode) opentable)                                ;remove node from opentable


            (setf (gethash (node-state curNode) closedtable) curNode)               ;add node to closedtable
            (setf CLOSED (cons (gethash (node-state curNode) closedtable) CLOSED))  ;add table entry to closed list

            (when (> generated *NODE-LIMIT*) 
                (format t "Memory Limit exceeded after generating ~D Nodes.~C" generated #\newline) 
                (return-from dfid_8puz nil)) ; too many paths, terminate early


            ;keep track of the depth of the deepest node so we know if we've hit a dead end. 
            ;(used to check if increasing depth limit added new nodes)
            (cond ( (> (node-depth curNode) currentdeepest) (setf currentdeepest (node-depth curNode))))
                        
            ;generate successors if current node is within depth limit
            (cond ((> depthlimit (node-depth curNode))
        
                (dolist (successor (getsuccessors (node-state curNode)))

                    ;keep track of all generated nodes
                    (setf generated (1+ generated))

                    ;for each child node, set parent to current node, set state to a successor, set cost to parent cost + 1
                    (setf child (make-node  :state successor 
                                            :parent (node-state curNode) 
                                            :cost (1+ (node-cost curNode)) 
                                            :depth (1+ (node-depth curNode))))

                    (setf memopen   (gethash successor opentable))
                    (setf memclosed (gethash successor closedtable))

                    (cond (memopen  ;test if child is on the open list
                            ;if the child contains a less expensive route to this state
                            (when (< (node-cost child) (node-cost memopen)) 

                                (setf (gethash successor opentable) child)                          ;re-assign new value in table
                                
                            )                                           
                          )

                          (memclosed    ;test if child is on the closed list
                            ;if the child contains a less expensive route to this state
                            (when (< (node-cost child) (node-cost memclosed)) 

                                (remove (gethash successor closedtable) CLOSED)                     ;remove old node from list
                                (remhash successor closedtable)                                     ;remove old node from table

                                ;adding the hash table contents to list means list works 
                                ;exactly the same, but list gains constant time access by value.
                                (setf (gethash successor opentable) child)                          ;assign new value in table
                                (setf OPEN (cons (gethash successor opentable) OPEN))               ;add child to open list
                            )
                          )
                          
                          (t ;if child is not a in open or closed lists
                            (setf (gethash successor opentable) child)              ;take note in hash table                                
                            (setf OPEN (cons (gethash successor opentable) OPEN))   ;add child to open list
                            (setf distinct (1+ distinct))                           ;keep track of distinct nodes
                            )

                    )
                        
                    )

                )
            )
        )

    (format t "~%Solution found in ~D moves.~C" (- (length SOLUTION-PATH) 1) #\newline)

    (format t "~D Nodes generated. (~D distinct nodes), ~D nodes expanded." 

        generated                                       ;open and closed contain all generated nodes
        distinct                                        ;number of distinct moves found
        (length CLOSED)                                 ;expanded nodes are on closed list
    )

    ;print out boards in solution path
    (print-solution SOLUTION-PATH)
    )

    ;return true to keep error message from appearing
    t
)