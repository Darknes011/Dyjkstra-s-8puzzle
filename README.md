Programming Assignment 2

usage: clisp 8puzzle.lsp <puzzle file name (optional) > <puzzlesize (optional) >

*---Input parameters-------------------------------------*

puzzle file name - The input file containing the name of the 
		   puzzle, name is of the form: "name.puz"
		   including quotation marks. file should 
		   contain row-major puzzle data with each 
		   tile separated by one or more spaces.

puzzlesize	 - Tile puzzle can be of arbitrary size N
		   where N is equal to the number of number
		   tiles in the puzzle (e.g. 8, 15, 24...)




*---Early Termination:----------------------------------*

	 Search algorithms are limited to generating 200000 nodes per search. This results in each search algorithm taking around 2 minutes before terminating and displaying an error message. The exception to this rule is DFID which may take longer as it repeats its searches with new depth limits

	To change this limit, one can re-assign the *NODE-LIMIT* variable to a greater value for extra time allowance, or a lesser value for a lower time allowance.

	A* will run once with an admissible heuristic, and then again with an inadmissible heuristic. This is because some inadmissible heuristics can find solutions much more efficiently on hard problems (e.g. tough.puz, worst.puz) where admissable heuristics result in the memory limit being reached.

*---List of heuristics for A*----------------------------*

	heur-1: zero (makes A* into djikstra's algorithm, essentially BFS for this problem since all tile moves cost 1)(admissible)
	heur-2: number of out-of-place tiles. (admissible)
	heur-3: square of heur-2 (inadmissable)
	heur-4: sum of manhattan distances of tiles from goal positions (admissible)
	heur-5: factorial of heur-4 (inadmissible)
	heur-6: exponential of heur-4 (inadmissible)
	heur-7: Nilsson score of state (heur-4 + number of out-of sequence tiles) (inadmissible)
	heur-8: Exponential of heur-7 (inadmissible)

*---Findings---------------------------------------------*

	Inadmissible heuristics for A* can sometimes be made 
more efficient by exponentiating the result. These improvements 
are usually at great cost to optimality on difficult problems, 
but result in very quick solutions.
	
	Using hash tables to eliminate the need to call 
'member' on every successor greatly reduces computation time.
The lists are stored normally but the contents are mirrored
in a paralell hash table.

*---Output Modifications---------------------------------*

	Added a "Current depth limit" readout for DFID. 
While this may not match the desired output exactly, it's 
easily removed and is useful for monitoring progress on very 
difficult searches.

	A* will run twice, once with an admissible heuristic, 
and once with an inadmissile heuristic. Since the 
Inadmissible heuristic runs very quickly, this shouldn't add
any significant computation time.

*---Partners---------------------------------------------*
    
    Jeremy Goens
    Zachary Christy
