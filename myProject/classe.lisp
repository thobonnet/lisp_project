
(defun upper-bound(x y)
	   (multiple-value-bind (upper) (ceiling x y)
	     upper))

;; 0 1 2 -> 1
;; 3 4 5 -> 2
;; 6 7 8 -> 3
;;input: coordinate :square for that coordinate (1 side)
(defun calc-sq (x)
  (upper-bound (1+ x) 3))


;;IMPLEMENT EFFECTIVE FORMULA ALLOW FOR EXTENSIBILITY
;; 1 -> (0 1 2)
;; 2 -> (3 4 5)
;; 3 -> (6 7 8)
;;input: square number output: range to interate over (one coordinate)
(defun calc-range (x)
  (case x
    (1 '(0 1 2))
    (2 '(3 4 5))
    (3 '(6 7 8))))

;;TURN POSSIBLE VAL TO 0 AND 1
;;ADD FIELD SUM_ SUM OF POSSIBLE_VAL ARRAY
;;ADD FIELD DONE i.e current-val assigned, all other val-possible false???
;;!!!ATTENTION!!! do necessary if person changes value they have assigned
;;i.e turn val-possible back to t

(defclass cell()
  ((current-val
    :initarg :current-val ;if 0 cell empty
    :initform 0
    :accessor cell-current-val)
   (line
    :initarg :line
    :initform 0
    :accessor cell-line)
   (col
    :initarg :col
    :initform 0
    :accessor cell-col)
   (carre
    :initarg :carre
    :initform 0
    :accessor cell-carre)
   (original
    :initarg :original
    :initform nil
    :accessor cell-original)
   (val-possible
    :initform (make-array '9 :initial-element 1)
    :accessor cell-val-possible)
   (sum ;;nb de val possible
    :initarg :sum
    :initform 0
    :accessor cell-sum)))

;;ADD SIZE ARRAY FOR EXTENSIBILITY OF MATRIX
(defclass sudoku()
  ((grid
    :initform (make-array '(9 9))
    :accessor sudoku-grid)
   (empty-cells
    :initarg :empty-cells
    :initform 81
    :accessor sudoku-empty-cells)))


(defgeneric update-group (sudoku cell val)
  (:documentation "update group of cells concerned when val is added to cell"))


(defmethod update-group ((sudoku sudoku) (cell cell) val)
  (let ((temp 0))
  ;;update line
    (do ((j 0 (1+ j))) ((= j 9))
      (setf (aref (cell-val-possible
		   (aref (sudoku-grid sudoku) (cell-line cell) j)) (1- val)) 0)
      (setf temp 0)
      (do ((k 0 (1+ k))) ((= k 9))
	(setf temp (+ temp (aref (cell-val-possible
		       (aref (sudoku-grid sudoku) (cell-line cell) j)) k)))) 
      
    
    (setf (cell-sum (aref (sudoku-grid sudoku) (cell-line cell) j)) temp))
    
  ;;update coloumn
  (do ((i 0 (1+ i))) ((= i 9))
    (setf (aref (cell-val-possible
		 (aref (sudoku-grid sudoku) i (cell-col cell))) (1- val)) 0)
    (setf temp 0)
    (do ((k 0 (1+ k))) ((= k 9))
      (setf temp (+ temp (aref (cell-val-possible
		     (aref (sudoku-grid sudoku) i (cell-col cell))) k))))
    
     (setf (cell-sum (aref (sudoku-grid sudoku) i (cell-col cell))) temp))
  
    ;;update carre
    (loop for x in (calc-range (calc-sq (cell-line cell)))
	  do (loop for y in (calc-range (calc-sq (cell-col cell)))
	       do (setf (aref (cell-val-possible
			       (aref (sudoku-grid sudoku) x y)) (1- val)) 0)
		  (setf temp 0)
		  (do ((k 0 (1+ k))) ((= k 9))
		    (setf temp (+ temp (aref (cell-val-possible
				   (aref (sudoku-grid sudoku) x y)) k))))
		  (setf (cell-sum (aref (sudoku-grid sudoku) x y)) temp)))
      
	      
  ;;(decf (cell-sum (aref (sudoku-grid sudoku) (cell-line cell) (cell-col cell)))))
  ))

(defgeneric val-valid (cell val)
  (:documentation "can val be placed in given cell?"))

(defmethod val-valid((cell cell) val)
  (and (= 1 (aref (cell-val-possible cell) (1- val)))
       (not (cell-original cell))))
    
      

(defgeneric new-game (sudoku input-grid)
  (:documentation "create new sudoku game from grid of original values"))

(defmethod new-game ((sudoku sudoku) input-grid)

  ;;remplissage du sudoku avec des cells
  (do ((i 0 (1+ i))) ((= i 9))
    (do ((j 0 (1+ j))) ((= j 9))
      (setf (aref (sudoku-grid sudoku) i j) (make-instance 'cell :line i :col j))))

  ;;remplissage du sudoku avec des valeurs initial
  (do ((i 0 (1+ i))) ((= i 9))
    (do ((j 0 (1+ j))) ((= j 9))
      (cond ((not (zerop (aref input-grid i j)))
	     (setf (cell-current-val
		    (aref (sudoku-grid sudoku) i j)) (aref input-grid i j))
	     (setf (cell-original
		    (aref (sudoku-grid sudoku) i j)) t)
	  (setf (sudoku-empty-cells sudoku) (1- (sudoku-empty-cells sudoku)))
	     (update-group sudoku (aref (sudoku-grid sudoku) i j)
			   (aref input-grid i j)))))))


(defgeneric play (sudoku)
  (:documentation "asks user input, verifys input, displays game"))

(defmethod play ((sudoku sudoku))
  ;;SORT OUT ERROR MESSAGES
  (let ((c 1)
	(l 1)
	(input-c 1)
	(val 1)
	(continue 'yes)
	(end 'GAME-OVER))
    (loop
       (format t "C?")
      (setq input-c (read))
      (setq c (- (char-code (aref (write-to-string input-c) 0)) 65))
       ;;verify c valid
       (format t "L?")
      (setq l (1- (read)))      
       ;;VERIFY L VALID
       ;;CHECK IF CELL IS ORIGINAL OR NOT
       (format t "val?")
       (setq val (read))
       ;;VERIFY VAL VALID
       (cond ((val-valid (aref (sudoku-grid sudoku) l c) val)
	      (setf (cell-current-val (aref (sudoku-grid sudoku) l c)) val)
	       (update-group sudoku (aref (sudoku-grid sudoku) l c) val)
	       (display-game sudoku))

	     (t
	       (format t "original val or exists in line, coloumn or square~%")
	       (format t "continue game?~%")
	       (setf continue (read))) )
       
       (when (or (eql continue 'no)
		 (= (sudoku-empty-cells sudoku) 0)) (return end)))))
	   


  
(defgeneric display-game (sudoku)
  (:documentation "display sudoku game"))

;;get rid of NIL returned
(defmethod display-game ((sudoku sudoku))
  (let ((s1 "-|-------+-------+-------|-")
	(row 0)
	(val 0))
    (format t " | A B C | D E F | G H I |~%")
    (format t s1)
    (do ((i 0 (1+ i))) ((= i 9))
      (format t "~%")
      (setq row (1+ row))
      (format t (write-to-string row))
      (format t "| ")
      (do ((j 0 (1+ j))) ((= j 9))
	(cond ((zerop (cell-current-val (aref (sudoku-grid sudoku) i j)))
		(format t "  "))
	       (t
		(setq val (cell-current-val (aref (sudoku-grid sudoku) i j)))
		(format t (write-to-string val))
		(format t " ")) )
	(if (or (= j 2) (= j 5) (= j 8))
	    (format t "| ")))
      (if (or (= i 2) (= i 5) (= i 8))
	  (progn
	    (format t "~%")
	    (format t s1))))
    (format t "~%")
    (format t (write-to-string (sudoku-empty-cells sudoku)))
    (format t "~%")))
	       
  
(defgeneric apply-strategy (sudoku strategy)
  (:documentation "applies strategy to sudoku"))

(defmethod apply-strategy ((sudoku sudoku) strategy)
  (multiple-value-bind (line col val played) (funcall strategy sudoku)
    (if (zerop played)
	(return-from apply-strategy (values 0 0 0 0)))
    
    (setf (cell-current-val (aref (sudoku-grid sudoku) line col)) (1+ val))
    (setf (sudoku-empty-cells sudoku) (1- (sudoku-empty-cells sudoku)))
    (update-group sudoku (aref (sudoku-grid sudoku) line col) (1+ val))
   
    (values line col val played)))
