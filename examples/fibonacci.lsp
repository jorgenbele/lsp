;; Prints the largest fibonacci number storable in the lsp int
(defun fibo-max ()
 (progn
  (defun fibo-max-rec (x y c)
   (if (gt y 0)
    (progn
     ; (println x y c)
     (fibo-max-rec y (+ x y) (+ c 1)))
    (list x (- c 1))))
  (fibo-max-rec 0 1 0)))

;; print the highest fibonacci number one can store in
;; a 64-bit signed integer
(println (car (fibo-max))) ; 7540113804746346429 (the 92nd fibonacci number)
