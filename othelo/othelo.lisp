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
(defvar *move-number* 1 "The number of the move to be played")


;;---------------------------------------------------------------------------------------------

;;some definitions

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
(defconstant in-board-squares
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

;; define setf behavior
(defsetf bref (board square) (val) 
  `(setf (aref ,board ,square) ,val))



;;-------------------------------------------------------------------------------

(defun concatenate-symbol (&rest args)
  (intern (format nil "~{~a~}" args)))


(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))


;;Sum of the weights of player's squares minus opponent's

(defun weighted-squares (player board)
  (let ((opp (opponent player)))
    (loop for i in in-board-squares
          when (eql (bref board i) player) 
          sum (aref *weights* i)
          when (eql (bref board i) opp)
          sum (- (aref *weights* i)))))


;; set a piece to the correspondent square of the board
(defun bref (board square) (aref board square))



;;creates the board and place the initial pieces

(defun start_board ()
  (let ((board (make-array 100 :element-type 'piece
                           :initial-element edges)))
    (dolist (square in-board-squares)
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
        in-board-squares))

;; return all possible moves for a given player
(defun possible-moves (player board)
  (loop for move in in-board-squares
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


(defun select-strategy (player)
  (print (format t "chose the strategy for the ~a ~% 1 HUMAN ~% 2 RANDOM ~% 3 ALPHA-BETA " player))
  (strategy-choice (read) player))



(defun strategy-choice (choice player)
	(if (= choice 1)
	    (princ (format t "you chosed HUMAM for the player ~a" player))
	    (if (= choice 2)
		(princ (format t "you chosed RANDOM for the player ~a" player))
		(if (= choice 3)
		    (princ (format t "you chosed ALPHA-BETA for the player ~a" player))
		    (princ "wrong choice please try again ")))))




(defun choice (choice)
  (print choice)
  (if (= choice 1)
      (progn
        (princ "you chosed BLACK") 
        t)
      (if (= choice 2)
       (progn 
        (princ "you choosed WHITE")
        t)
       (progn
        (princ "wrong choice please try again BLACK = 1 WHITE = 2       =>")
        nil))))


(defun othello ()
  (let* ((board (start_board)))
	(catch 'game-over
	  (loop for *move-number* from 1
	       for player = black then (next-to-play board player t)
	       for strategy = #'human
	       until (null player)
	       do (get-move strategy player board))
	  (when t
	    (format t "~&The game is over. Final result:")
	    (print-board board)))))

(defun next-to-play (board previous-player print)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opp (opponent previous-player)))
    (cond ((have_moves opp board) opp)
          ((have_moves previous-player board) 
           (when print
             (format t "~&~c has no moves and must pass."
                     (piece_simbol opp)))
           previous-player)
          (t nil))))



(defun get-move (strategy player board)
  (when t (print-board board))
  (let*  ((move (funcall strategy player (replace *board* board))))
    (cond
    ((eq move 'resign)
       (throw 'game-over (if (eql player black) -64 64)))
    ((and (valid-place move) (legal-place move player board))
         (format t "~&~c moves to ~a." 
                 (piece_simbol player) (88->h8 move))
       (m-move move player board)))))



(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

;; names of each place on the board
 (let ((square-names 
        (cross-product #'concatenate-symbol
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

 (defun human (player board)
  (format t "~&~c to move ~a: " (piece_simbol player)
          (mapcar #'88->h8 (possible-moves player board)))
  (h8->88 (read)))
