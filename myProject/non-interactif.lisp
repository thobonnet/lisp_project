



(defparameter *sudoku-game* NIL)


(defun init-standalone(grid)
  (setf *sudoku-game* (make-instance 'sudoku))
  (new-game *sudoku-game* grid)
  (display-game *sudoku-game*)
  )

(defun main-standalone()
  (let ((strategie #'strat-singleton))
    ;;chooses which strategy to apply
    ;;set variable strategie to chosen strategy
    (multiple-value-bind (line col val played) (apply-strategy *sudoku-game* strategie)
      (display-game *sudoku-game*)
      (if (zerop played)
	  (return-from main-standalone)
	  (values (1+ line) (code-char (+ 65 col)) (1+ val))))))

(defun affi(i j)
  (print (cell-sum (aref (sudoku-grid *sudoku-game*) i j)))
  (print (cell-val-possible (aref (sudoku-grid *sudoku-game*) i j))))

(defun affi-sum(i j)
  (print (cell-sum (aref (sudoku-grid *sudoku-game*) i j)))
  (print (cell-sum (aref (sudoku-grid *sudoku-game*) i j))))

;;func to see hw far strategy goes
(defun test-limit(grid)
  (init-standalone grid)
  (loop
     (main-standalone)
     ))
       
       
(defun test-strat ()
  (strat-aleatoire *sudoku-game*))
