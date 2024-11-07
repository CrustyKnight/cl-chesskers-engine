(defpackage :chesskers-engine
  (:import-from :chesskers #:piece #:game #:game-board #:game-color #:game-turn #:game-move #:game-moves #:game-move-list #:copy-game #:list-of-jumps-p #:game-step-p)
  (:use :common-lisp :defstar)
  (:export #:find-best-move #:evaluate #:evaluate-move))
(in-package :chesskers-engine)

(defun* (sign -> (integer -1 1)) (num)
  "Return the sign of a number."
  (declare (optimize (speed 1)))
  (cond
    ((plusp num) 1)
    ((minusp num) -1)
    (t 0)))
(defun* (piece->value -> (integer -1000 1000)) ((piece piece))
  "Return the value of a piece."
  (declare (type piece piece))
  (* (sign piece)
     (ecase (abs piece)
       (6 1000)
       (5 6)
       (4 5)
       (3 4)
       (2 7)
       (1 1)
       (0 0))))
;; NOTE: most-positive/most-negative evaluation are hand reasoned, and will need to be updated if evaluate is changed
(defvar* (most-positive-evaluation fixnum) 1100 "The highest possible value evaluate could return for a board.")
(defvar* (most-negative-evaluation fixnum) -1100)
(deftype evaulation-range () `(integer ,most-negative-evaluation ,most-positive-evaluation)) ;; this was supposed to be a type for alphabeta and stuff, for optimizations, but its not really used.
(defun* (evaluate -> (integer -1100 1100)) ((game game))
  "Evaluate a given board.

Currently this is a pretty basic function, just summing piece values.
Heuristics should probably be added to increase performance."
  (reduce #'+ (map '(SIMPLE-VECTOR 64) #'piece->value (make-array 64 :element-type 'piece :displaced-to (game-board game)))))

(defun evaluate-move (game move)
  "Evaluate a the outcome of a move without modifying game."
  (let ((ng (copy-game game)))
    (game-move ng move)
    (evaluate ng)))
(defun* (order-moves -> game-move-list) ((game game) (moves game-move-list))
  "Order moves to help pruning in alphabeta.

Currently this just places longer jumps at the front, and steps at the end."
  (sort moves #'(lambda (mv1 mv2)
                  (or (and (list-of-jumps-p mv1) (game-step-p mv2))
                      (and (list-of-jumps-p mv1)
                           (list-of-jumps-p mv2)
                           (> (length (the list mv1)) (length (the list mv2))))))))

(defun* (alphabeta -> integer) ((game game) (depth fixnum))
  "Alphabeta algorithm to search+prune the game tree down to a certain depth, and return and evaluation."
  (labels* (((abmax -> integer) ((game game) (depth fixnum) (alpha integer) (beta integer))
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
            ((abmin -> integer) ((game game) (depth fixnum) (alpha integer) (beta integer))
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
        (abmax game depth (- most-negative-evaluation 1) (+ 1 most-positive-evaluation))
        (abmin game depth (- most-negative-evaluation 1) (+ 1 most-positive-evaluation)))))

;; TODO using like lparallel or something parallelize this. (probably split it into a pmap that generates values, then a reduce that finds the max/min)
(defun minimized-move (game depth)
  "Find the move with the lowest evaluation, searching at the given depth."
  (let ((moves (order-moves game (game-moves game))))
    (loop with (min-move . min-val) = (cons (first moves) (+ 2 most-positive-evaluation)) ;; will be greater than anything alphabeta gives
          for move in moves
          for val = (alphabeta (game-move (copy-game game) move) depth)
          if (< val min-val)
            do (setf min-val val min-move move)
          finally (return (cons min-val min-move)))))
(defun maximized-move (game depth)
  "Find the move with the greatest evaluation, searching at the given depth."
  (let ((moves (order-moves game (game-moves game))))
    (loop with (max-move . max-val) = (cons (first moves) (- most-negative-evaluation 2)) ;; will be lesser than anything alphabeta gives
          for move in moves
          for val = (alphabeta (game-move (copy-game game) move) depth)
          if (> val max-val)
            do (setf max-val val max-move move)
          finally (return (cons max-val max-move)))))
(defun* (find-best-move -> game-move) ((game game) (depth fixnum))
  "Find the best move for the current player."
  (cdr
   (if (= (game-color game) 1)
       (maximized-move game depth)
       (minimized-move game depth))))
