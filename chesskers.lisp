(defpackage :chesskers
  (:import-from :arrow-macros #:-<>> #:-<> #:<>)
  (:use :common-lisp :defstar)
  (:export #:game #:make-game #:game-move #:game-moves #:game-step #:game-jump))

(in-package :chesskers)

(declaim (optimize (speed 3) (safety 3)))


;;; Types/structs --------------------------------------------------------------------------
(deftype coord () '(integer -2 10)) ;; range coordinates will be in (not just 0 7 cause you have knights which initially look +-2 from where they are, could be -2 or 10 for those)
(deftype square () '(cons coord coord))
(defun squarep (thing)
  (typep thing 'square))
(deftype piece () '(signed-byte 8))
(defun list-of-squares-p (lst)
  (and
   (typep lst 'list)
   (every #'squarep lst)))
(deftype square-list ()
  `(and list (satisfies list-of-squares-p)))


(defparameter* (START-BOARD (simple-array (signed-byte 8) (8 8)))
    (make-array '(8 8) :element-type 'piece
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
  "A struct to represent a game."
  (board (alexandria:copy-array START-BOARD) :type (SIMPLE-ARRAY PIECE (8 8))) (turn 0 :type FIXNUM) (color 1 :type (INTEGER -1 1)))
(defun* (copy-game -> game) ((game game))
  (make-game :board (alexandria:copy-array (game-board game)) :turn (game-turn game) :color (game-color game)))
(defstruct game-step
  "A struct to represent a single step in a game."
  (start (error "start is required") :type SQUARE) (end (error "end is required") :type SQUARE))
(defstruct game-jump
  "A struct to represent a single jump in a game.

Note: this does not represent a full move."
  (start (error "start is required") :type SQUARE) (taken (error "taken is required") :type SQUARE) (land (error "land is required") :type SQUARE))
(defun* end-row-p ((square square))
  "Check if square is an end row."
  (or (= (car square) 0) (= (car square) 7)))
(deftype end-row () '(and square (satisfies end-row-p)))
(defstruct (game-promotion-step (:include game-step (end (error "start is required") :type end-row)))
  "A struct to represent steps that are also promotion."
  (promotion (error "promotion piece is required") :type piece))
(defstruct (game-promotion-jump (:include game-jump (land (error "start is required") :type end-row)))
  "A struct to represent jumps that are also promotion."
  (promotion (error "promotion piece is required") :type piece))
(defun list-of-jumps-p (list)
  (and (listp list)
       (every #'game-jump-p list)))
(deftype game-jump-list ()
  '(and list (satisfies list-of-jumps-p)))
(defun list-of-steps-p (lst)
  (and
   (typep lst 'list)
   (every #'game-step-p lst)))
(deftype game-step-list ()
  `(and list (satisfies list-of-steps-p)))
(deftype game-move () '(or game-step game-jump-list))
(defun list-of-moves-p (list)
  (and (listp list)
       (every #'(lambda (l) (typep l 'game-move)) list)))
(deftype game-move-list () '(and list (satisfies list-of-moves-p)))


;;; UTILS -----------------------------------------------------------------------------------

(defun* (sign -> (integer -1 1)) (num)
  "Give the sign of a number.

If you are trying to get the color of a piece, use the function color instead.
(It has smaller types & can therefore be more optimized)"
  (declare (optimize (speed 1)))
  (cond
    ((plusp num) 1)
    ((minusp num) -1)
    (t 0)))

(declaim (inline at))
(defun* (put-at -> piece)  ((game game) (square square) (piece piece))
  "Place a piece on a square in a game."
  (setf (aref (game-board game) (car square) (cdr square)) piece))
(defun* (at -> piece) ((game game) (square square))
  "Return the piece at a square in a game."
  (aref (game-board game) (car square) (cdr square)))


(defun* game-do-jump ((game game) (jump game-jump))
  "Execute a single jump on a game.

(The jump is not always a full move.)"
  (let ((p (if (game-promotion-jump-p jump) (game-promotion-jump-promotion jump)
               (at game (game-jump-start jump)))))
    (put-at game (game-jump-start jump) 0)
    (put-at game (game-jump-taken jump) 0)
    (put-at game (game-jump-land jump) p)))
(defun* game-do-step ((game game) (step game-step))
  "Execute a step on a game."
  (let ((p (if (game-promotion-step-p step) (game-promotion-step-promotion step)
               (at game (game-step-start step)))))
    (put-at game (game-step-start step) 0)
    (put-at game (game-step-end step) p)))
(defun* (game-move -> game) ((game game) (move game-move))
  "Execute a move on a game and return the modified game.

Flip the current player & increment turn as well as executing the move.
(Note: this function is destructive, and does modify the game it is passed.)"
  (if (game-step-p move)
      (game-do-step game move)
      (loop for jump in move
            doing (game-do-jump game jump)))
  (incf (game-turn game))
  (setf (game-color game) (- (game-color game)))
  game)

(declaim (inline in-bounds))
(defun* (in-bounds -> boolean) ((square square))
  "Check if SQUARE is in bounds"
  (*let ((row coord (car square))
         (col coord (cdr square)))
    (and (<= 0 row 7) (<= 0 col 7))))

(declaim (inline emptyp fullp color whitep blackp))
(defun* (emptyp -> boolean) ((game game) (square square))
  "Check if SQUARE in GAME is empty."
  (declare (type square square))
  (declare (type game game))
  (= 0 (at game square)))
(defun* (fullp -> boolean) ((game game)  (square square))
  "Check if SQUARE in GAME is full."
  (not (emptyp game square)))
(defun* (color -> (integer -1 1)) ((piece piece))
  (cond
    ((plusp piece) 1)
    ((minusp piece) -1)
    (t 0)))
(defun* (whitep -> boolean) ((piece piece)) (plusp piece))
(defun* (blackp -> boolean) ((piece piece)) (minusp piece))

(declaim (inline add-sq-dir))
(defun* (add-sq-dir -> square) ((square square) (direction square))
  "Add DIRECTION to SQUARE, returning the new square. Non destructive."
  (*let ((row1 coord (car square))
         (row2 coord (car direction))
         (col1 coord (cdr square))
         (col2 coord (cdr direction)))
    (cons (+ row1 row2) (+ col1 col2))))

(defmacro =one (item &rest rest)
  "Check if the number ITEM is equal to one of REST."
  `(or
    ,@(loop for r in rest collecting `(= ,item ,r))))


;;; Square Steps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* (direxp-list -> square-list) ((game game) (square square) (direction square))
  "Give all squares a piece could land on starting at SQUARE and traveling in DIRECTION in GAME."
  (loop for sq = (add-sq-dir square direction) then (add-sq-dir sq direction)
        while (and (in-bounds sq) (emptyp game sq))
        collect sq))

(defmacro make-game-promotion-steps (&key start end color)
  "Helper macro to construct all the possible promotions for a given step."
  `(list ,@(loop for piece from 2 to 5 collecting `(make-game-promotion-step :start ,start :end ,end :promotion (* ,color ,piece)))))
(defmacro filter-proper-steps (steps)
  "Filter STEPS so only 'proper' steps remain (e.g. steps that are in bounds and land on empty squares)."
  `(remove-if-not #'(lambda (mv) (and (in-bounds mv) (emptyp game mv))) ,steps))
(defun* (square-steps -> game-step-list) ((game game) (square square))
  "Give all the steps that the piece at SQUARE in GAME could do."
  (let ((p (at game square)))
    (labels ((squares->steps (mvs) (declare (type square-list mvs))  (mapcar #'(lambda (mv) (make-game-step :start square :end mv)) mvs))
             (pawn ()
               (let* ((c (color p))
                      (d (- c))
                      (start-row (if (= c 1) 6 1))
                      (promote-row (if (= c 1) 0 7)))
                 (-<>> (if (= (car square) start-row)
                           (list (add-sq-dir square (cons d 0))
                                 (add-sq-dir square (cons (* 2 d) 0)))
                           (list (add-sq-dir square (cons d 0))))
                   (remove-if-not #'(lambda (mv) (and (in-bounds mv) (emptyp game mv))))
                   (mapcan #'(lambda (mv) (if (= (the coord (car mv)) promote-row)
                                              (make-game-promotion-steps :start square :end mv :color c)
                                              (list (make-game-step :start square :end mv))))))))
             (knight ()
               (let ((moves '((-1 . 2) (-1 . -2) (1 . 2) (1 . -2) (2 . 1) (2 . -1) (-2 . 1) (-2 . -1))))
                 (squares->steps
                  (filter-proper-steps
                   (mapcar #'(lambda (mv) (add-sq-dir square mv)) moves)))))
             (bishop ()
               (let ((directions '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1))))
                 (squares->steps
                  (filter-proper-steps
                   (mapcan #'(lambda (d) (direxp-list game square d)) directions)))))
             (rook ()
               (let ((directions '((1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                 (squares->steps
                  (filter-proper-steps
                   (mapcan #'(lambda (d) (direxp-list game square d)) directions)))))
             (queen () ; bishop & rook already output steps
               (append (bishop) (rook)))
             (king ()
               (let ((moves '((-1 . -1) (-1 . 1) (1 . -1) (1 . 1) (1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                 (squares->steps
                  (filter-proper-steps
                   (mapcar #'(lambda (mv) (add-sq-dir square mv)) moves))))))
      (case (abs p)
        (1 (pawn))
        (2 (knight))
        (3 (bishop))
        (4 (rook))
        (5 (queen))
        (6 (king))))))

;;; Square Jumps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun* (direxp-land -> square) ((game game) (square square) (direction square))
  "Give the square a piece would stop right before if it traveled from SQUARE in DIRECTION in GAME as far as it could.

(or, give the square a piece would run into if it traveled as far as it could.)"
  (loop for sq = (add-sq-dir square direction) then (add-sq-dir sq direction)
        while (and (in-bounds sq) (emptyp game sq))
        finally (return sq)))

(declaim (inline jump-direction))
(defun* (jump-direction -> square) ((start square) (taken square))
  "Give the direction of a jump from START taking TAKEN."
  (cons (sign (- (car taken) (car start)))
        (sign (- (cdr taken) (cdr start)))))

;; destructive
(defun* edge-effects ((game game) (square square) (taken square) (land square))
  "Wrap a jump (given destructed as three squares) around the edge of the board if it is allowed to.
Otherwise, return nil.
(It the jump does not go over the edge boundaries, return it.)"

  (*let ((piece (at game square))
         (land-col (cdr land))
         (land-row (car land)))
    (if (in-bounds land) (return-from edge-effects (cons taken land)))
    (if (not (<= 0 land-col 7))
        (setf (cdr land) (mod land-col 8)))
    (cond
      ((and (whitep piece) (= 8 land-row)) (return-from edge-effects nil))
      ((and (whitep piece) (= -1 land-row) (setf (car land) 7)))
      ((and (blackp piece) (= 8 land-row)) (setf (car land) 0))
      ((and (blackp piece) (= -1 land-row)) (return-from edge-effects nil)))
    (cons taken land)))

(defun* execute-edge-effects (moves (game game) (square square))
  "Wrap all jumps in MOVES around the edeges of the board if allowed and or needed.
(Moves is a list of taken/landing pairs, not a list of game-jump structs.)"
  (remove-if #'null (mapcar #'(lambda (mv) (edge-effects game square (car mv) (cdr mv))) moves)))

(defmacro filter-possible-jumps (mvs)
  "Filter MVS down to jumps that are possible.

(Taking in bounds and taking a full square)"
  `(remove-if-not #'(lambda (mv) (and (in-bounds mv) (fullp game mv))) ,mvs))
(defmacro filter-full-landings (mvs)
  "Filter MVS to only jumps that don't land on pieces."
  `(remove-if-not #'(lambda (mv) (or (equal square (cdr mv)) (emptyp game (cdr mv)))) ,mvs))
(defmacro piece-jumps (generate-takes generate-landings &optional (custom-constructor nil ccp))
  "Macro to generate all the jumps a piece can make.

GENERATE-TAKES is a function that generates all the other pieces a piece can take.
GENERATE-LANDINGS is a function that generates (take . landing) pairs given a
  list of pieces that can be taken as the last argument.

CUSTOM-CONSTRUCTOR is a function that creates the actual jump structs given
  the (take . landing) pairs as the last argument."
  `(-<>> ,generate-takes
     filter-possible-jumps
     ,generate-landings
     (execute-edge-effects <> game square)
     filter-full-landings
     ,(if ccp custom-constructor
          `(mapcar #'(lambda (mv) (make-game-jump :start square :taken (car mv) :land (cdr mv)))))))
(defmacro make-promotion-jumps (&key start taken land color)
  "Helper macro to create all the possible promotions for a jump."
  `(list ,@(loop for piece from 2 to 5 collecting `(make-game-promotion-jump :start ,start :taken ,taken :land ,land :promotion (* ,piece ,color)))))
(defun* (square-jumps -> game-jump-list) ((game game) (square square) &optional (start t) (qctx nil))
  "Give all the single jumps that the piece at SQUARE in GAME could do."
  (let ((p (at game square)))
    (labels ((single-square-jump (jump)
               (and (<= (abs (- (car (game-jump-start jump)) (car (game-jump-taken jump)))) 1)
                    (<= (abs (- (cdr (game-jump-start jump)) (cdr (game-jump-taken jump)))) 1)))
             (pawn ()
               (let* ((c (color p))
                      (d (- c))
                      (promote-row (if (= c 1) 0 7)))
                 (piece-jumps (list (add-sq-dir square (cons d 1))
                                    (add-sq-dir square (cons d -1)))
                     (mapcar #'(lambda (mv) (cons mv (add-sq-dir mv (jump-direction square mv)))))
                     (mapcan #'(lambda (mv) (if (= (the coord (cadr mv)) promote-row)
                                                (make-promotion-jumps :start square :taken (car mv) :land (cdr mv) :color c)
                                                (list (make-game-jump :start square :taken (car mv) :land (cdr mv)))))))))
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
                 (remove-if-not #'(lambda (jump) (or start (single-square-jump jump)))
                                (piece-jumps (mapcar #'(lambda (d) (direxp-land game square d)) directions)
                                    (mapcar #'(lambda (mv) (cons mv (add-sq-dir mv (jump-direction square mv)))))))))
             (rook ()
               (let ((directions '((1 . 0) (-1 . 0) (0 . -1) (0 . 1))))
                 (remove-if-not #'(lambda (jump) (or start (single-square-jump jump)))
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
      (case (abs p)
        (1 (pawn))
        (2 (knight))
        (3 (bishop))
        (4 (rook))
        (5 (queen))
        (6 (king))))))


(defun* new-queen-context ((jump game-jump))
  "Create a new queen context from JUMP.

A queen context is either 'diag or 'strt, and helps square-jumps know how a
queen can move."
  (labels ((diagonalp (jump)
             (*let ((d square (jump-direction (game-jump-start jump) (game-jump-taken jump))))
               (= (abs (car d)) (abs (cdr d)) 1))))
    (cond ((diagonalp jump) 'diag)
          (t                'strt))))

(defun* edge-jump-p ((jump game-jump))
  "Check if JUMP is a jump over an edge."
  (let ((taken-col (cdr (game-jump-taken jump)))
        (taken-row (car (game-jump-taken jump)))
        (land-col  (cdr (game-jump-land jump)))
        (land-row  (car (game-jump-land jump))))
    (or
     (and (not (= taken-col land-col)) ;; col of taken and land are on different edges => jumped across side edge
          (=one taken-col 0 7)
          (=one land-col  0 7))
     (and (not (= taken-row land-row)) ;; row of taken and land are on different edges => jumped across back/front edge
          (=one taken-row 0 7)
          (=one land-row  0 7)))))

(defun* square-jumps-recursive ((game game) (square square) &optional (start t) (qctx nil))
  "Give all multi-jumps a piece at SQUARE in GAME can do.
START indicates if this is the first jump in a chain.
QCTX indicates how the queen can move (important for chains of queen jumps)
A multi-jump is a chain of jumps, which represents a single move."

  (let ((jumps (square-jumps game square start qctx)))
    (if (= 0 (length jumps))
        (return-from square-jumps-recursive '()))
    (labels ((next-level (jump)
               (declare (type game-jump jump))
               (if (or (edge-jump-p jump) (game-promotion-jump-p jump)) (return-from next-level '()))
               (let ((ng (copy-game game))
                     (qctx (new-queen-context jump)))
                 (game-do-jump ng jump)
                 (square-jumps-recursive ng (game-jump-land jump) nil qctx))))
      (loop for jump in jumps
            for next-jumps = (next-level jump)
            collect (list jump)
            when (< 0 (length (the list next-jumps)))
              append (mapcar #'(lambda (jmp) (cons jump jmp)) next-jumps)))))


;;; GAME-MOVES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* square-moves ((game game) (square square))
  "Give all the moves a piece at SQUARE at GAME can do."
  (append (square-jumps-recursive game square) (square-steps game square)))
(defun* (color-squares -> square-list) ((game game) (color (integer -1 1)))
  "Give all squares that have a piece of color COLOR in GAME."
  (loop for r from 0 to 7
        for sqs = (loop for c from 0 to 7
                        for sq = (cons r c)
                        when (= color (color (at game sq)))
                          collect sq)
        when sqs
          nconc sqs))
(defun game-moves (game)
  "Give all the moves the current color can do in GAME."
  (declare (type game game))
  (let ((squares (color-squares game (game-color game))))
    (loop for square in squares
          nconcing (square-moves game square))))
