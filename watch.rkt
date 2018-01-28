#lang racket
(match-let (((list in out pid err f) (process "mupdf capstone.pdf")))
  (system (~a "ls *.scrbl | entr sh -c 'scribble --pdf capstone.scrbl && kill -s 1 " (add1 pid) "'"))
  (close-output-port out)
  (close-input-port in)
  (close-input-port err)
  )
