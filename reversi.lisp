(defvar *player1_color*)
(defvar *player2_color*)
(defvar *move*)
(defvar *p1_possible_moves*)
(defvar *p2_possible_moves*)
(defvar *board*)
(defconstant all-directions '(-11 -10 -9 -1 1 9 10 11))
(defconstant all-squares
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))




(defparameter *board_value* '(
              (11 . 120)  (21 . -20) (31 . 20) (41 . 5)  (51 . 5)  (61 . 20) (71 . -20) (81 . 120)
              (12 . -20)  (22 . -40) (32 . -5) (42 . -5) (52 . -5) (62 . -5) (72 . -40) (82 . -20)
              (13 . 20)   (23 . -5)  (33 . 15) (43 . 3)  (53 . 3)  (63 . 15) (73 . -5)  (83 . 20)
              (14 . 5)    (24 . -5)  (34 . 3)  (44 . 3)  (54 . 3)  (64 . 3)  (74 . -5)  (84 . 5)
              (15 . 5)    (25 . -5)  (35 . 3)  (45 . 3)  (55 . 3)  (65 . 3)  (75 . -5)  (85 . 5)
              (16 . 20)   (26 . -5)  (36 . 15) (46 . 3)  (56 . 3)  (66 . 15) (76 . -5)  (86 . 20)
              (17 . -20)  (27 . -40) (37 . -5) (47 . -5) (57 . -5) (67 . -5) (77 . -40) (87 . -20)
              (18 . 120)  (28 . -20) (38 . 20) (48 . 5)  (58 . 5)  (68 . 20) (78 . -20) (88 . 120)
              ))




(setq *board* '(
              (11 . -) (21 . -) (31 . -) (41 . -) (51 . -) (61 . -) (71 . -) (81 . -)
              (12 . -) (22 . -) (32 . -) (42 . -) (52 . -) (62 . -) (72 . -) (82 . -)
              (13 . -) (23 . -) (33 . -) (43 . -) (53 . -) (63 . -) (73 . -) (83 . -)
              (14 . -) (24 . -) (34 . -) (44 . B) (54 . W) (64 . -) (74 . -) (84 . -)
              (15 . -) (25 . -) (35 . -) (45 . W) (55 . B) (65 . -) (75 . -) (85 . -)
              (16 . -) (26 . -) (36 . -) (46 . -) (56 . -) (66 . -) (76 . -) (86 . -)
              (17 . -) (27 . -) (37 . -) (47 . -) (57 . -) (67 . -) (77 . -) (87 . -)
              (11 . -) (21 . -) (31 . -) (41 . -) (51 . -) (61 . -) (71 . -) (81 . -)
              )23456

(defun print-board ()
  (print '0) (princ8'1) (princ '" ") (princ '2) (princ '" ") (princ '3) (princ '" ") (princ '4) (princ '" ") (princ '5) (princ '" ") (princ '6) (princ '" ") (princ '7) (princ '" ") (princ '8)
  (print 'a) (princ (cdr(assoc '11 *board*))) (princ '" ") (princ (cdr(assoc '21 *board*))) (princ '" ") (princ (cdr(assoc '31 *board*))) (princ '" ") (princ (cdr(assoc '41 *board*))) (princ '" ") (princ (cdr(assoc '51 *board*))) (princ '" ") (princ (cdr(assoc '61 *board*))) (princ '" ") (princ (cdr(assoc '71 *board*))) (princ '" ") (princ (cdr(assoc '81 *board*)))
  (print 'b) (princ (cdr(assoc '12 *board*))) (princ '" ") (princ (cdr(assoc '22 *board*))) (princ '" ") (princ (cdr(assoc '32 *board*))) (princ '" ") (princ (cdr(assoc '42 *board*))) (princ '" ") (princ (cdr(assoc '52 *board*))) (princ '" ") (princ (cdr(assoc '62 *board*))) (princ '" ") (princ (cdr(assoc '72 *board*))) (princ '" ") (princ (cdr(assoc '82 *board*)))
  (print 'c) (princ (cdr(assoc '13 *board*))) (princ '" ") (princ (cdr(assoc '23 *board*))) (princ '" ") (princ (cdr(assoc '33 *board*))) (princ '" ") (princ (cdr(assoc '43 *board*))) (princ '" ") (princ (cdr(assoc '53 *board*))) (princ '" ") (princ (cdr(assoc '63 *board*))) (princ '" ") (princ (cdr(assoc '73 *board*))) (princ '" ") (princ (cdr(assoc '83 *board*)))
  (print 'd) (princ (cdr(assoc '14 *board*))) (princ '" ") (princ (cdr(assoc '24 *board*))) (princ '" ") (princ (cdr(assoc '34 *board*))) (princ '" ") (princ (cdr(assoc '44 *board*))) (princ '" ") (princ (cdr(assoc '54 *board*))) (princ '" ") (princ (cdr(assoc '64 *board*))) (princ '" ") (princ (cdr(assoc '74 *board*))) (princ '" ") (princ (cdr(assoc '84 *board*)))
  (print 'e) (princ (cdr(assoc '15 *board*))) (princ '" ") (princ (cdr(assoc '25 *board*))) (princ '" ") (princ (cdr(assoc '35 *board*))) (princ '" ") (princ (cdr(assoc '45 *board*))) (princ '" ") (princ (cdr(assoc '55 *board*))) (princ '" ") (princ (cdr(assoc '65 *board*))) (princ '" ") (princ (cdr(assoc '75 *board*))) (princ '" ") (princ (cdr(assoc '85 *board*)))
  (print 'f) (princ (cdr(assoc '16 *board*))) (princ '" ") (princ (cdr(assoc '26 *board*))) (princ '" ") (princ (cdr(assoc '36 *board*))) (princ '" ") (princ (cdr(assoc '46 *board*))) (princ '" ") (princ (cdr(assoc '56 *board*))) (princ '" ") (princ (cdr(assoc '66 *board*))) (princ '" ") (princ (cdr(assoc '76 *board*))) (princ '" ") (princ (cdr(assoc '86 *board*)))
  (print 'g) (princ (cdr(assoc '17 *board*))) (princ '" ") (princ (cdr(assoc '27 *board*))) (princ '" ") (princ (cdr(assoc '37 *board*))) (princ '" ") (princ (cdr(assoc '47 *board*))) (princ '" ") (princ (cdr(assoc '57 *board*))) (princ '" ") (princ (cdr(assoc '67 *board*))) (princ '" ") (princ (cdr(assoc '77 *board*))) (princ '" ") (princ (cdr(assoc '87 *board*)))
  (print 'h) (princ (cdr(assoc '18 *board*))) (princ '" ") (princ (cdr(assoc '28 *board*))) (princ '" ") (princ (cdr(assoc '38 *board*))) (princ '" ") (princ (cdr(assoc '48 *board*))) (princ '" ") (princ (cdr(assoc '58 *board*))) (princ '" ") (princ (cdr(assoc '68 *board*))) (princ '" ") (princ (cdr(assoc '78 *board*))) (princ '" ") (princ (cdr(assoc '88 *board*)))
  "enter your move")




(defun make-move (move color) 
  (setf (cdr (assoc move *board*)) color))


(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  ;;*** fix, segre, 3/30/93.  Was remove-if, which can share with all-squares.
  (loop for move in all-squares
  when (legal-p move player board) collect move))





