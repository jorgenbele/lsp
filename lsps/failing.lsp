
; Exclude excl from the list lst
(defun recursive-function (lst acc excl)
  (if (> (len lst) 0)
      (let ((front (car lst)) (rest (cdr lst)) (accl acc))
        (if (! (= front excl))
            (setq accl (append front acc)))
        (return (recursive-function rest accl excl)))
        (return acc)))

;;(defvar *ints* (1 2 3 4))
;;(println "excl list" *ints* 2 "is" (recursive-function *ints* '() 2))

(defvar *ints* (1 2 3))
(println "excl list" *ints* 2 "is" (recursive-function *ints* '() 2))
