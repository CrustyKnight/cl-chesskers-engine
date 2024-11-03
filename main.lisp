(ql:quickload :arrow-macros)
(ql:quickload :alexandria)
(defpackage :chesskers
  (:use :common-lisp :arrow-macros)
  (:export #:calc-moves #:game #:at #:put-at #:find-best-move #:game-move #:game-step #:game-jump #:ucn->move #:move->ucn #:main))

(in-package :chesskers)


                                        ;(declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (speed 3) (safety 0)))
(defun squarep (thing)
  (and (consp thing)
       (numberp (car thing))
       (numberp (cdr thing))))
(deftype square () '(satisfies squarep))
(deftype piece () '(integer -6 6))


(declaim (type (simple-array (SIGNED-BYTE 8) (8 8)) START-BOARD))
(defparameter START-BOARD (make-array '(8 8) :element-type 'piece
                                             :initial-contents
                                             '((-4 -2 -3 -5 -6 -3 -2 -4)
                                               (-1 -1 -1 -1 -1 -1 -1 -1)
                                               (0 0 0 0 0 0 0 0)
                                               (0 0 0 0 0 0 0 0)
                                               (0 0 0 0 0 0 0 0)
                                               (0 0 0 0 0 0 0 0)
                                               (1 1 1 1 1 1 1 1)
                                               (4 2 3 5 6 3 2 4))))

(defstruct (game (:copier nil))
  (board (alexandria:copy-array START-BOARD) :type (SIMPLE-ARRAY PIECE (8 8))) (turn 0 :type INTEGER) (color 1 :type (INTEGER -1 1)))
(defun copy-game (game)
  (make-game :board (alexandria:copy-array (game-board game)) :turn (game-turn game) :color (game-color game)))
(defstruct game-step
  (start (error "start is required") :type SQUARE) (end (error "end is required") :type SQUARE))
(defstruct game-jump
  (start (error "start is required") :type SQUARE) (taken (error "taken is required") :type SQUARE) (land (error "land is required") :type SQUARE))
(defun list-of-jumps (list)
  (and (listp list)
       (every #'game-jump-p list)))
(deftype game-jump-list ()
  '(and list (satisfies list-of-jumps)))
(deftype game-move () '(or game-step game-jump-list))

(declaim (ftype (function (game square piece) piece) put-at))
(defun put-at (game square piece)
  (declare (type square square))
  (declare (type game game))
  (declare (type piece piece))
  (setf (aref (game-board game) (car square) (cdr square)) piece))

(declaim (ftype (function (game square) piece) at) (inline at))
(defun at (game square)
  (declare (type square square))
  (declare (type game game))
  (aref (game-board game) (car square) (cdr square)))


(defun game-do-jump (game jump)
  (let ((p (at game (game-jump-start jump))))
    (put-at game (game-jump-start jump) 0)
    (put-at game (game-jump-taken jump) 0)
    (put-at game (game-jump-land jump) p)))
(defun game-do-step (game step)
  (let ((p (at game (game-step-start step))))
    (put-at game (game-step-start step) 0)
    (put-at game (game-step-end step) p)))
(defun game-move (game move)
  (declare (type game game))
  (declare (type game-move move))
  (if (game-step-p move)
      (game-do-step game move)
      (loop for jump in move
            doing (game-do-jump game jump)))
  (incf (game-turn game))
  (setf (game-color game) (- (game-color game)))
  game)

(declaim (ftype (function (square) boolean) in-bounds) (inline in-bounds))
(defun in-bounds (square)
  (declare (type square square))
  (and (<= 0 (car square) 7) (<= 0 (cdr square) 7)))

(declaim (ftype (function (game square) boolean) emptyp) (inline emptyp))
(defun emptyp (game square)
  (declare (type square square))
  (declare (type game game))
  (= 0 (at game square)))
(declaim (ftype (function (game square) boolean) fullp) (inline fullp))
(defun fullp (game square)
  (not (emptyp game square)))

(declaim (ftype (function (square square) square) add-sq-dir) (inline add-sq-dir))
(defun add-sq-dir (square direction)
  (declare (type square square direction))
  (cons (+ (car square) (car direction)) (+ (cdr square) (cdr direction))))

(defun direxp-list (game square direction)
  (loop for sq = (add-sq-dir square direction) then (add-sq-dir sq direction)
        while (and (in-bounds sq) (emptyp game sq))
        collect sq))

(declaim (ftype (function (piece) (integer -1 1)) color) (inline color))
(defun color (piece)
  (declare (type piece piece))
  (cond
    ((plusp piece) 1)
    ((minusp piece) -1)
    (t 0)))
(defun sign (num)
  (declare (type integer num))
  (cond
    ((plusp num) 1)
    ((minusp num) -1)
    (t 0)))


(defmacro filter-proper-steps (steps)
  `(remove-if-not #'(lambda (mv) (and (in-bounds mv) (emptyp game mv))) ,steps))
(defun square-steps (game square)
  (declare (type game game))
  (declare (type square square))
  (let ((p (at game square)))
    (labels ((pawn ()
               (let* ((c (color p))
                      (d (- c))
                      (start-row (if (= c 1) 6 1)))
                 (remove-if-not #'(lambda (mv) (and (in-bounds mv) (emptyp game mv)))
                                (if (= (car square) start-row)
                                    (list (add-sq-dir square (cons d 0))
                                          (add-sq-dir square (cons (* 2 d) 0)))
                                    (list (add-sq-dir square (cons d 0)))))))
             (knight ()
               (let ((moves '((-1 . 2) (-1 . -2) (1 . 2) (1 . -2) (2 . 1) (2 . -1) (-2 . 1) (-2 . -1))))
                 (filter-proper-steps
                  (mapcar #'(lambda (mv) (add-sq-dir square mv)) moves))))
             (bishop ()
               (let ((directions '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1))))
                 (filter-proper-steps
                  (mapcan #'(lambda (d) (direxp-list game square d)) directions))))
             (rook ()
               (let ((directions '((1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                 (filter-proper-steps
                  (mapcan #'(lambda (d) (direxp-list game square d)) directions))))
             (queen ()
               (append (bishop) (rook)))
             (king ()
               (let ((moves '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1) (1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                 (filter-proper-steps
                  (mapcar #'(lambda (mv) (add-sq-dir square mv)) moves)))))
      (mapcar #'(lambda (mv) (make-game-step :start square :end mv))
              (case (abs p)
                (1 (pawn))
                (2 (knight))
                (3 (bishop))
                (4 (rook))
                (5 (queen))
                (6 (king)))))))

(defun direxp-land (game square direction)
  (loop for sq = (add-sq-dir square direction) then (add-sq-dir sq direction)
        while (and (in-bounds sq) (emptyp game sq))
        finally (return sq)))

(declaim (ftype (function (square square) square) jump-direction) (inline jump-direction))
(defun jump-direction (start taken)
  (cons (sign (- (car taken) (car start)))
        (sign (- (cdr taken) (cdr start)))))

;; destructive
(defun edge-effects (game square taken land)
  (let ((piece (at game square)))
    (if (in-bounds land) (return-from edge-effects (cons taken land)))
    (if (not (<= 0 (cdr land) 7))
        (setf (cdr land) (mod (cdr land) 8)))
    (cond
      ((and (plusp piece) (= 8 (car land))) (return-from edge-effects nil))
      ((and (plusp piece) (= -1 (car land)) (setf (car land) 7)))
      ((and (minusp piece) (= 8 (car land))) (setf (car land) 0))
      ((and (minusp piece) (= -1 (car land))) (return-from edge-effects nil)))
    (cons taken land)))

(defun execute-edge-effects (moves game square)
  (remove-if #'null (mapcar #'(lambda (mv) (edge-effects game square (car mv) (cdr mv))) moves)))
(defmacro filter-possible-jumps (mvs)
  `(remove-if-not #'(lambda (mv) (and (in-bounds mv) (fullp game mv))) ,mvs))
(defmacro filter-full-landings (mvs)
  `(remove-if-not #'(lambda (mv) (emptyp game (cdr mv))) ,mvs))

(defmacro piece-jumps (generate-takes generate-landings)
  `(-<>> ,generate-takes
     filter-possible-jumps
     ,generate-landings
     (execute-edge-effects <> game square)
     filter-full-landings))
(declaim (ftype (function (game square &optional t t) game-jump-list) square-jumps))
(defun square-jumps (game square &optional (start t) (qctx nil))
  (declare (type game game))
  (declare (type square square))
  (let ((p (at game square)))
    (labels ((single-square-jump (jmp-stub)
               (and (<= (abs (- (car square) (caar jmp-stub))) 1)
                    (<= (abs (- (cdr square) (cdar jmp-stub))) 1)))
             (pawn ()
               (let* ((c (color p))
                      (d (- c)))
                 (piece-jumps (list (add-sq-dir square (cons d 1))
                                    (add-sq-dir square (cons d -1)))
                              (mapcar #'(lambda (mv) (cons mv (add-sq-dir mv (jump-direction square mv))))))))
             (knight ()
               (let ((moves '((-1 . 2) (-1 . -2) (1 . 2) (1 . -2) (2 . 1) (2 . -1) (-2 . 1) (-2 . -1))))
                 (piece-jumps (mapcar #'(lambda (mv) (add-sq-dir square mv)) moves)
                              (mapcan #'(lambda (mv)
                                          (let* ((dir (jump-direction square mv))
                                                 (d (cons (cons (car dir) 0) (cons 0 (cdr dir)))))
                                            (list (cons mv (add-sq-dir mv (car d)))
                                                  (cons mv (add-sq-dir mv (cdr d))))))))))
             (bishop ()
               (let ((directions '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1))))

                 ;; the remove if not removes items that aren't single square jumps if it's not the start
                 (remove-if-not #'(lambda (jmp-stub) (or start (single-square-jump jmp-stub)))
                                (piece-jumps (mapcar #'(lambda (d) (direxp-land game square d)) directions)
                                             (mapcar #'(lambda (mv) (cons mv (add-sq-dir mv (jump-direction square mv)))))))))
             (rook ()
               (let ((directions '((1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                 (remove-if-not #'(lambda (jmp-stub) (or start (single-square-jump jmp-stub)))
                                (piece-jumps (mapcar #'(lambda (d) (direxp-land game square d)) directions)
                                             (mapcar #'(lambda (mv) (cons mv (add-sq-dir mv (jump-direction square mv)))))))))
             (queen ()
               (case qctx
                 ('nil (append (rook) (bishop)))
                 (diag (bishop))
                 (strt (rook))))
             (king () (let ((moves '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1) (1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                        (piece-jumps (mapcar #'(lambda (mv) (add-sq-dir square mv)) moves)
                                     (mapcar #'(lambda (mv) (cons mv (add-sq-dir mv (jump-direction square mv)))))))))
      (mapcar #'(lambda (mv) (make-game-jump :start square :taken (car mv) :land (cdr mv)))
              (case (abs p)
                (1 (pawn))
                (2 (knight))
                (3 (bishop))
                (4 (rook))
                (5 (queen))
                (6 (king)))))))

(defun new-queen-context (jump)
  (declare (type game-jump jump))
  (labels ((diagonalp (jump)
             (let ((d (jump-direction (game-jump-start jump) (game-jump-taken jump))))
               (= (abs (car d)) (abs (cdr d)) 1))))
    (cond ((diagonalp jump) 'diag)
          (t                'strt))))
(defun square-jumps-recursive (game square &optional (start t) (qctx nil))
  (let ((jumps (square-jumps game square start qctx)))
    (if (= 0 (length jumps))
        (return-from square-jumps-recursive '()))
    (labels ((next-level (jump)
               (declare (type game-jump jump))
               (let ((ng (copy-game game))
                     (qctx (new-queen-context jump)))
                 (game-do-jump ng jump)
                 (square-jumps-recursive ng (game-jump-land jump) nil qctx))))
      (loop for jump in jumps
            for next-jumps = (next-level jump)
            collect (list jump)
            when (< 0 (length next-jumps))
              append (mapcar #'(lambda (jmp) (cons jump jmp)) next-jumps)))))
(defun square-moves (game square)
  (append (square-jumps-recursive game square) (square-steps game square)))

(defun color-squares (game color)
  (loop for r from 0 to 7
        for sqs = (loop for c from 0 to 7
                        for sq = (cons r c)
                        when (= color (color (at game sq)))
                          collect sq)
        when sqs
          nconc sqs))
(defun game-moves (game &optional)
  (let ((squares (color-squares game (game-color game))))
    (loop for square in squares
          nconcing (square-moves game square))))

;; TODO do testing

;;; Bot Stuff

;; the most white can have is 1100 (1 king, 2 rooks, 2 bisops, 1 queen, 10 knights)
(declaim (ftype (function (piece) (integer -1000 1000)) piece->value))
(defun piece->value (piece)
  (declare (type piece piece))
  (* (sign piece)
     (case (abs piece)
       (6 1000)
       (5 6)
       (4 5)
       (3 4)
       (2 7)
       (1 1)
       (0 0))))
(declaim (ftype (function (game) (integer -1100 1100)) evaluate))
(defun evaluate (game)
  (reduce #'+ (map '(SIMPLE-VECTOR 64) #'piece->value (make-array 64 :element-type 'piece :displaced-to (game-board game)))))
(defun evaluate-move (game move)
  (let ((ng (copy-game game)))
    (game-move ng move)
    (evaluate ng)))
(defun order-moves (game moves)
  (declare (type game game))
  (sort moves #'(lambda (mv1 mv2)
                  (or (and (list-of-jumps mv1) (game-step-p mv2))
                      (and (list-of-jumps mv1)
                           (list-of-jumps mv2)
                           (> (length mv1) (length mv2)))))))
(defun alphabeta (game depth)
  (labels ((abmax (game depth alpha beta)
             (if (= depth 1) (return-from abmax (evaluate game)))
             (let ((moves (order-moves game (game-moves game))))
               (loop for move in moves
                     for temp-game = (let ((ng (copy-game game))) (game-move ng move) ng)
                     for val = (abmin temp-game (- depth 1) alpha beta)
                     when (>= val beta)
                       do (return-from abmax beta)
                     when (> val alpha)
                       do (setf alpha val)
                     finally (return alpha))))
           (abmin (game depth alpha beta)
             (if (= depth 1) (return-from abmin (evaluate game)))
             (let ((moves (order-moves game (game-moves game))))
               (loop for move in moves
                     for temp-game = (let ((ng (copy-game game))) (game-move ng move) ng)
                     for val = (abmax temp-game (- depth 1) alpha beta)
                     when (<= val alpha)
                       do (return-from abmin alpha)
                     when (< val beta)
                       do (setf beta val)
                     finally (return beta)))))
    (if (= (game-color game) 1)
        (abmax game depth -10000 10000)
        (abmin game depth -10000 10000))))




(defun print-jump (jump)
  (format nil "(~a, ~a, ~a), " (game-jump-start jump)
          (game-jump-taken jump)
          (game-jump-land jump)))
(defun fcons (cons)
  (format nil "(~a, ~a)" (car cons) (cdr cons)))
(defun print-jump-list (jump-list)
  (format t "[~{~a~^, ~}]~%" (mapcar #'(lambda (jmp) (format nil "(~a, ~a, ~a)"
                                                             (fcons (game-jump-start jmp))
                                                             (fcons (game-jump-taken jmp))
                                                             (fcons (game-jump-land jmp))))
                                     jump-list)))
(defun print-step (stp)
  (format t "(~a, ~a)~%"
          (fcons (game-step-start stp))
          (fcons (game-step-end stp))))

(defun minimized-move (game depth)
  (let ((moves (order-moves game (game-moves game))))
    (loop with (min-move . min-val) = (cons (first moves) 100000)
          for move in moves
          for val = (alphabeta (game-move (copy-game game) move) depth)
          if (< val min-val)
            do (setf min-val val min-move move)
          finally (return (cons min-val min-move)))))
(defun maximized-move (game depth)
  (let ((moves (order-moves game (game-moves game))))
    (loop with (max-move . max-val) = (cons (first moves) 100000)
          for move in moves
          for val = (alphabeta (game-move (copy-game game) move) depth)
          if (> val max-val)
            do (setf max-val val max-move move)
          finally (return (cons max-val max-move)))))
(declaim (ftype (function (game (integer 0)) game-move) find-best-move))
(defun find-best-move (game depth)
  ;;(declare (optimize (speed 3) (safety 0)))
  (cdr
   (if (= (game-color game) 1)
       (maximized-move game depth)
       (minimized-move game depth))))

;; TODO test stuff, & make a function to convert from the python program string rep to the common lisp rep of a board
;; Also, add promotion & en-passant
;; Also, add a function to output UCN moves (should be pretty easy w/ format)
;; Also, add castling

;; 4508 bytes for square-jumps
;; 3796 bytes with (declaim (optimize (speed 3) (safety 0)))
;; (find-best-move *english-dumb* 4) took 1.979 seconds with ^
;; down to 1.919 after declaming inline for some small oft used functions (with optimize)


;; TODO figure out how to use the profiler

;;; IO stuff

(declaim (ftype (function (piece) (simple-array character (1))) piece->string))
(defun piece->string (piece)
  (declare (type piece piece))
  (case piece
    (-6 "k") (-5 "q") (-4 "r") (-3 "b") (-2 "n") (-1 "p")
    (0 ".")
    (1 "P") (2 "N") (3 "B") (4 "R") (5 "Q") (6 "K")))

(declaim (ftype (function ((or (simple-array character (1)) (simple-base-string 1))) piece) string->piece))
(defun string->piece (string)
  (case (aref string 0)
    (#\k -6) (#\q -5) (#\r -4) (#\b -3) (#\n -2) (#\p -1)
    (#\. 0)
    (#\P 1) (#\N 2) (#\B 3) (#\R 4) (#\Q 5) (#\K 6)))

(defun print-board (game)
  (let ((board (loop for r below 8
                     collecting (loop for c below 8 collecting (piece->string (aref (game-board game) r c))))))
    (concatenate 'string
                 (format nil "C,C,None,~a,~a~%" (game-turn game) (game-color game))
                 (format nil "~{~{~a~^ ~}~^~%~}" board))))
(defun read-board (string)
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
  (format nil "~c~c"
          (aref "abcdefgh" (cdr square))   ; row
          (aref "87654321" (car square)))) ;row
(defun string->square (string)
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
(defun move->ucn (move)
  (declare (type game-move move))
  (if (game-step-p move)
      (format nil "~a~a" (square->string (game-step-start move)) (square->string (game-step-end move)))
      (format nil "~{~a~^|~}" (mapcar #'(lambda (jmp) (format nil "~a~at~a"
                                                              (square->string (game-jump-start jmp))
                                                              (square->string (game-jump-taken jmp))
                                                              (square->string (game-jump-land jmp))))
                                      move))))
(defun ucn->move (ucn)
  (if (find #\t ucn)
      (let ((ucn-jmps (uiop:split-string ucn :separator "|")))
        (mapcar #'(lambda (ucn-jmp)
                    (make-game-jump :start (string->square (subseq ucn-jmp 0 2))
                                    :taken (string->square (subseq ucn-jmp 2 4))
                                    :land  (string->square (subseq ucn-jmp 5 7))))
                ucn-jmps))
      (make-game-step :start (string->square (subseq ucn 0 2)) :end (string->square (subseq ucn 2 4)))))


(defun read-board-and-evaluate-for-humans ()
  (format t "Enter in the board in the plain string format line by line.~%After finishing, enter an empty line~%")
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

(defun main-human ()
  (loop (read-board-and-evaluate-for-humans)))

(defun play-game ()
  (let ((color nil)
        (game (make-game))
        (depth 3)
        (bot-move nil))
    (format t "Would you like to play white (enter 1 for white, -1 for black)")
    (setf color (parse-integer (read-line)))
    (format t "You are playing ~:[black~;white~]. What depth would you like to play against? " (= color 1))
    (setf depth (parse-integer (read-line)))
    (when (= -1 color)
      (setf bot-move (find-best-move game depth))
      (format t "I'll start with: ~a" (move->ucn bot-move))
      (game-move game bot-move))
    (loop (progn
            (format t "~a~%" (print-board game))
            (format t "Make your move: ")
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

;; (defun for-python-as-subprocess ()
;;   (loop (read-board-and-evaluate-for-machines)))
;; (defun for-python-as-subprocess ()
;;   (loop
;;     do (read-board-and-evaluate-for-machines)
;;     when (string= (read-line) "quit")
;;       return nil))

(defun for-python-as-subprocess ()
  (read-board-and-evaluate-for-machines))
