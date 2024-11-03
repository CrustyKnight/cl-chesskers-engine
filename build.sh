#!/usr/bin/env sh

sbcl --load main.lisp --eval "(sb-ext:save-lisp-and-die #P\"./play-chesskers\" :toplevel #'chesskers::play-game :executable t)"
sbcl --load main.lisp --eval "(sb-ext:save-lisp-and-die #P\"./chesskers-computer-executable\" :toplevel #'chesskers::for-python-as-subprocess :executable t)"
