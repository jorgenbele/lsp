
;; usage (fibo <number>) => n'th fibonacchi number
(defun fibo (n)
  (progn 
    (defun fibo-next (x y)
      (+ x y))
    (defun fibo-rec (x y c n)
        (if (lt c n)
            (fibo-rec y (fibo-next x y) (+ c 1) n)
          y))
    (fibo-rec 0 1 0 n)))

;; returns the biggest number storable in the lsp int
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
(println (car (fibo-max)))
; (println (fibo 3))
