
    
;;change input-grid to grid
(defun sudoku(input-grid)
  (let ((sudoku-game (make-instance 'sudoku)))
    (new-game sudoku-game input-grid)
    (display-game sudoku-game)
    (play sudoku-game)))
  


  
