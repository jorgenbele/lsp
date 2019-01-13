;; Returns lst without excl
(defun list-excl (lst acc excl)
  (if (> (len lst) 0)
      (let ((front (car lst)) (rest (cdr lst)) (accl acc))
        (if (! (= front excl))
            (setq accl (append front acc)))
        (return (list-excl rest accl excl)))
        (return acc)))

(list-excl (1 2 3 4 5) '() 3) ; (1 2 4 5)
