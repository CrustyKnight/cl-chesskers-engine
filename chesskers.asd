(asdf:defsystem "chesskers"
  :depends-on ("arrow-macros" "alexandria" "defstar")
  :components ((:file "chesskers")
               (:file "chesskers-engine" :depends-on ("chesskers"))
               (:file "chesskers-main" :depends-on ("chesskers" "chesskers-engine")))
  :build-operation "program-op"
  :build-pathname "chesskers"
  :entry-point "chesskers-main:main")
