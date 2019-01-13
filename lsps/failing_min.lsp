(defun function ()
 (let ((x 1))
    x
  ))

(defun function1 ()
 (let ((x 1))
    (return x)
  ))

 (println (function))
 (println (function1))
