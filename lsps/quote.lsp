; tests of the quote function (shorthand ')
(println (quote (+ 1 2 3)))        ; (+ 1 2 3)
(println (eval (quote (+ 1 2 3)))) ; 6
(println '(+ 1 2 3))               ; (+ 1 2 3)
(println (eval '(+ 1 2 3)))        ; 6
