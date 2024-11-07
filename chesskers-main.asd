(asdf:defsystem "chesskers-main"
  :depends-on ("arrow-macros" "chesskers" "chesskers-engine")
  :components ((:file "chesskers-main"))
  :build-operation "program-op"
  :build-pathname "chesskers"
  :entry-point "chesskers-main:main")
