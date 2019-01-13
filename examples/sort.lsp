(defun list-excl-helper (lst acc excl)
  (if (> (len lst) 0)
      (let ((front (car lst)) (rest (cdr lst)) (accl acc))
        (if (! (= front excl))
            (setq accl (append front acc)))
        (return (list-excl-helper rest accl excl)))
        (return acc)))

(defun list-excl (lst excl)
  (return (list-excl-helper lst '() excl)))

(defun list-min-helper (lst acc-min)
  (if (> (len lst) 0)
      (if (< (car lst) acc-min)
          (return (list-min-helper (cdr lst) (car lst)))
        (return (list-min-helper (cdr lst) acc-min)))
    (return acc-min)))

(defun list-min (lst)
  (list-min-helper lst 9223372036854775807))

(defun list-sort-helper (lst acc)
  (if (> (len lst) 0)
        (return (list-sort-helper (list-excl lst (list-min lst)) (append (list-min lst) (acc))))
    (return acc)))

(defun list-sort (lst)
  (return (list-sort-helper lst '())))

(println (list-sort (92839 7589 263695 123 5 71)))    ; (5 71 123 7589 92839 263695)
(println (list-sort (27371 958709721 -12312 751565))) ; (-12312 27371 751565 958709721)
