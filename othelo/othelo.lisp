;;constants to be used
;;--------------------------------------------------------------------------
;; empty squares
(defconstant empty 0 )

;; black pieces
(defconstant black 1 )

;; white pieces
(defconstant white 2 )

;; squares out of the board limit
(defconstant edges 3 )

;;values to calculate next square 
;; -1 = up 1 = down
;; -10 = left 10 = right
;; -11 = diagonal left up 11 = diagonal right down
;; -9 = diagonal left down 9 = diagonal right up
(defconstant directions '(-11 -10 -9 -1 1 9 10 11))



;;define board heuristic number for ai moves
(defparameter *weights*
  '#(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))

;;define the simbol for a piece
(defun piece_simbol (piece) (char ".BW?" piece))

;;type definitions
(deftype piece () `(integer ,empty ,edges))
(deftype board () '(simple-array piece (100)))


;; oponent color definition
(defun opponent (player) (if (eql player black) white black))

;;define all playble isqueres of the board
(defconstant playble_squares
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

;; set a piece to the correspondent square of the board
(defun bref (board square) (aref board square))

;; define setf behavior
(defsetf bref (board square) (val) 
  `(setf (aref ,board ,square) ,val))


(defun start_board ()
  (let ((board (make-array 100 :element-type 'piece
                           :initial-element edges)))
    (dolist (square playble_squares)
      (setf (bref board square) empty))
    (setf (bref board 44) white   (bref board 45) black
          (bref board 54) black   (bref board 55) white)
    board))

;;define the variable *board* with the initial board
(defvar *board* (start_board))


(defun print-board (&optional (board *board*))  
  ;; First print the header and the current score
  (format t "~2&    a b c d e f g h   [~c=~2a ~c=~2a (~@d)]"
          (piece_simbol black) (count black board)
          (piece_simbol white) (count white board)
          (count-difference black board))          
  ;; print line numbers and the lines 
  (loop for line from 1 to 8 do
        (format t "~&  ~d " line)
        (loop for colune from 1 to 8
              for piece = (bref board (+ colune (* 10 line)))
              do (format t "~c " (piece_simbol piece)))))


(defun count-difference (player board)
  (- (count player board)
     (count (opponent player) board)))

;;just return if the move is inside of the board
(defun valid-place (move)
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))


;; return if a move is legal
(defun legal-place (move player board)
  (and (eql (bref board move) empty)
       (some #'(lambda (dir) (does_flip move player board dir))
             directions)))

;; return if a move flip any piece 
(defun does_flip (move player board dir)
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

;;find next piece for a given direction
(defun find-bracketing-piece (square player board dir)
   (cond ((eql (bref board square) player) square)
        ((eql (bref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (t nil)))

;;return a boolean if a player has moves
(defun have_moves (player board)
  (some #'(lambda (move) (legal-place move player board))
        playble_squares))

;; return all possible moves for a given player
(defun possible-moves (player board)
  (loop for move in playble_squares
  when (legal-place move player board) collect move))

;;do the changes on the table after a move
(defun m-move (move player board)
  (setf (bref board move) player)
  (dolist (dir directions)
    (m-flip move player board dir))
  board)


;;flip the pieces after move
(defun m-flip (move player board dir)
  (let ((bracketer (does_flip move player board dir)))
    (when bracketer
      (if (not(eql move bracketer))
        (progn
          (setf (bref board (+ move dir)) player)
          (m-flip (+ move dir) player board dir))
          nil ))))

(defvar st 1)
(defun start (st)
  (loop while(= st 1)do
   (princ (format t "~A ~%~A ~%~A~%" "for play versus CPU enter 1"
    "for 2 player enter        2"
    "=>")) 
   (read)(setf st 0)))

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

;; names of each place on the board
 (let ((square-names 
        (cross-product #'symbol
                       '(? a b c d e f g h ?)
                       '(? 1 2 3 4 5 6 7 8 ?))))

  (defun h8->88 (str)
    "Convert from alphanumeric to numeric square notation."
    (or (position (string str) square-names :test #'string-equal)
        str))

  (defun 88->h8 (num)
    "Convert from numeric to alphanumeric square notation."
    (if (valid-place num)
        (elt square-names num)
        num)))

(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))


(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))