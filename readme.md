# Common Lisp Chesskers Bot


A port of the python chesskers bot to common lisp, to be (possibly) used in the python implementation as the backend.

To get an executable, you can run build.sh (assuming you are on linux)

play-chesskers will put you into a more human friendly program which will allow you to play chesskers.

chesskers-computer-executable will expect a board representation and a depth entered (with an empty line separating them)
and will then print out the best move for the current player and exit.


# TODO
- Add support for castling, en-passant, and checkmate/end of game
