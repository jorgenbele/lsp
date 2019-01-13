;; This test program tests arithmetic
(println "== Primitives ==")
(println (+ 1 2 3 4 5)) ; expected = 15
(println (* 10 10))     ; expected = 100
(println (* 10 10 10))  ; expected = 1000
(println (- 10 10 3))   ; expected = -3

(println "== Composite ==")
(println (+ 10 (* 3 (- 10 4)))) ; expected = 28
