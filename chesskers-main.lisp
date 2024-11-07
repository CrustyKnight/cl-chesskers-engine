(defpackage :chesskers-main
  (:import-from :chesskers-engine #:find-best-move)
  (:import-from :chesskers #:game #:make-game #:game-color #:game-turn #:game-board #:piece
                #:game-step-p #:game-jump-p #:make-game-step #:make-game-jump
                #:game-promotion-step-p #:game-promotion-jump-p
                #:make-game-promotion-jump #:make-game-promotion-step
                #:game-move #:game-jump-start #:game-jump-land #:game-jump-taken
                #:game-step-end #:game-step-start)
  (:import-from :arrow-macros #:-<>> #:-<> #:<>)
  (:use :common-lisp :defstar)
  (:export #:play-game #:for-python-oneshot #:main))
(in-package :chesskers-main)

(declaim (optimize (speed 0) (safety 3) (compilation-speed 3)))

(defun piece->string (piece)
  "Converts a piece to a string."
  (declare (type piece piece))
  (case piece
    (-6 "k") (-5 "q") (-4 "r") (-3 "b") (-2 "n") (-1 "p")
    (0 ".")
    (1 "P") (2 "N") (3 "B") (4 "R") (5 "Q") (6 "K")))

(defun string->piece (string)
  "Converts a string to a piece.
The string should be a single character."
  (case (aref string 0)
    (#\k -6) (#\q -5) (#\r -4) (#\b -3) (#\n -2) (#\p -1)
    (#\. 0)
    (#\P 1) (#\N 2) (#\B 3) (#\R 4) (#\Q 5) (#\K 6)))


(defun print-board (game)
  "Prints a board out in a semi human-readable format that is also easily readable by a computer."
  (let ((board (loop for r below 8
                     collecting (loop for c below 8 collecting (piece->string (aref (game-board game) r c))))))
    (concatenate 'string
                 (format nil "C,C,None,~a,~a~%" (game-turn game) (game-color game))
                 (format nil "~{~{~a~^ ~}~^~%~}" board))))
(defun read-board (string)
  "Reads a board like one printed by [[print-board]]."
  (let* ((split-string (remove-if #'(lambda (str) (string= "" str)) (uiop:split-string string :separator '(#\Newline #\ ))))
         (chars (cdr split-string))
         (data (uiop:split-string (car split-string) :separator ","))
         (turn (parse-integer (fourth data)))
         (color (parse-integer (fifth data)))
         (board (-<>> chars
                  (mapcar #'string->piece) ; chars -> flat list of piece values
                  (make-array 64 :initial-contents <> :element-type 'piece) ; flat list -> flat array
                  (make-array '(8 8) :element-type 'piece :displaced-to <>))))
    (make-game :board board :turn turn :color color)))

(defun square->string (square)
  "Converts a square to a string in UCN format for a square."
  ;; For use in move->ucn
  (format nil "~c~c"
          (aref "abcdefgh" (cdr square))   ; row
          (aref "87654321" (car square)))) ;row
(defun string->square (string)
  "Converts a string to a square.
The string should be in UCN format for a square."
  (cons (- 8  (digit-char-p (aref string 1)))
        (case (aref string 0)
          (#\a 0)
          (#\b 1)
          (#\c 2)
          (#\d 3)
          (#\e 4)
          (#\f 5)
          (#\g 6)
          (#\h 7))))
;; TODO add ucn support for promotion
(defun move->ucn (move)
  "Converts an internal move object to its UCN string representation."
  (declare (type game-move move))
  (if (game-step-p move)
      (format nil "~a~a" (square->string (game-step-start move)) (square->string (game-step-end move)))
      (format nil "~{~a~^|~}" (mapcar #'(lambda (jmp) (format nil "~a~at~a"
                                                              (square->string (game-jump-start jmp))
                                                              (square->string (game-jump-taken jmp))
                                                              (square->string (game-jump-land jmp))))
                                      move))))
(defun ucn->move (ucn)
  "Converts a UCN string representation of a move into the internal representation"
  (if (find #\t ucn)
      (let ((ucn-jmps (uiop:split-string ucn :separator "|")))
        (mapcar #'(lambda (ucn-jmp)
                    (make-game-jump :start (string->square (subseq ucn-jmp 0 2))
                                    :taken (string->square (subseq ucn-jmp 2 4))
                                    :land  (string->square (subseq ucn-jmp 5 7))))
                ucn-jmps))
      (make-game-step :start (string->square (subseq ucn 0 2)) :end (string->square (subseq ucn 2 4)))))


(defun read-board-and-evaluate-for-humans ()
  (format t "~@{~a~%~}"
          "Enter in the board in the plain string format line by line."
          "After finishing, enter an empty line"
          "Then, enter the depth to search at and hit enter")
  (let ((game
          (-<>> (loop for line = (read-line)
                      while (not (string= "" line))
                      collecting line)
            (format nil "~{~a~^~%~}" <>)
            read-board))
        (depth (parse-integer (read-line))))
    (format t "Best Move for ~:[black~;white~]:~%~a" (= (game-color game) 1)
            (move->ucn
             (find-best-move game depth)))))

(defun play-game ()
  "Game loop function for playing a game in the terminal."
  (let ((color nil)
        (game (make-game))
        (depth 3)
        (bot-move nil))
    (format t "Would you like to play white (enter 1 for white, -1 for black)")
    (finish-output)
    (setf color (parse-integer (read-line)))
    (format t "You are playing ~:[black~;white~]. What depth would you like to play against? " (= color 1))
    (finish-output)
    (setf depth (parse-integer (read-line)))
    (when (= -1 color)
      (setf bot-move (find-best-move game depth))
      (format t "I'll start with: ~a~%" (move->ucn bot-move))
      (finish-output)
      (game-move game bot-move))
    (loop (progn
            (format t "~a~%" (print-board game))
            (format t "Make your move: ")
            (finish-output)
            (-<> (read-line)
              ucn->move
              (game-move game <>))
            (format t "~a~%" (print-board game))
            (setf bot-move (find-best-move game depth))
            (format t "I'll go ~a~%" (move->ucn bot-move))
            (game-move game bot-move)))))

(defun read-board-and-evaluate-for-machines ()
  (let ((game
          (-<>> (loop for line = (read-line)
                      if (string= "quit" line)
                        do (return-from read-board-and-evaluate-for-machines 0)
                      while (not (string= "" line))
                      collecting line)
            (format nil "~{~a~^~%~}" <>)
            read-board))
        (depth (parse-integer (read-line))))
    (format t "~a" (move->ucn (find-best-move game depth)))))
(defun for-python-oneshot ()
  (read-board-and-evaluate-for-machines))

(defun show-help ()
  (format t "~@{~a~%~}"
          "A Chesskers app written in Common Lisp"
          "  Use the \"play\" option to play against the bot."
          "  If you are trying to use this as part of a program, use the \"eval\" option."
          "    This will read a board and a depth from input and return the best move, then exit."
          "  If you want to use this to evaluate a board in a more human friendly way, use the \"eval-human\" option."))

(defun main ()
  (cond
    ((member "play" (uiop:command-line-arguments) :test #'string-equal) (play-game))
    ((member "eval" (uiop:command-line-arguments) :test #'string-equal) (for-python-oneshot))
    ((member "eval-human" (uiop:command-line-arguments) :test #'string-equal) (read-board-and-evaluate-for-humans))
    (t (show-help))))
