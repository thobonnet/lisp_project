

;;les strategies sont des fonctions
(defun strat-premier-possible (sudoku)
  ;;pour chaque cell
  ;;    pour chaque val-possible de cell
  ;;        joue premier val-possible qu'il rencontre
  (let ((played 0))
  (do ((i 0 (1+ i))) ((= i 9))
    (do ((j 0 (1+ j))) ((= j 9))
      (if (zerop (cell-current-val (aref (sudoku-grid sudoku) i j)))
	  (do ((k 0 (1+ k))) ((= k 9))
	    (if (= 1 (aref (cell-val-possible(aref (sudoku-grid sudoku) i j)) k))
		(progn 
		       (setf played 1)
		       (return-from strat-premier-possible (values i j k played))
		       ))) ))
      )
    (values 0 0 0 played)))


  
(defun strat-aleatoire (sudoku)
  (let ((i (random 8))
	(j (random 8))
	(val (random 8))
	(played 0))
    (if (zerop (cell-current-val (aref (sudoku-grid sudoku) i j)))
	(progn
	 (if (= 1 (aref (cell-val-possible(aref (sudoku-grid sudoku) i j)) val))
	     (progn 
	       (setf played 1)
	       (values i j val played)
	       )
	     (values 0 0 0 played)))
	 (values 0 0 0 played))))
	     
	   
(defun strat-singleton (sudoku)
  (let ((played 0))
    (do ((i 0 (1+ i))) ((= i 9))
      (do ((j 0 (1+ j))) ((= j 9))
	(if (zerop (cell-current-val (aref (sudoku-grid sudoku) i j)))
	    (if (= 1 (cell-sum (aref (sudoku-grid sudoku) i j)))
		(do ((k 0 (1+ k))) ((= k 9))
		  (if (= 1 (aref (cell-val-possible(aref (sudoku-grid sudoku) i j)) k))
		      (progn
			(setf played 1)
			(return-from strat-singleton (values i j k played)))
		      ))) ))
    (values 0 0 0 played))))
  
  
