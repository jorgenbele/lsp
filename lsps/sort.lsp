;; sort
;; (defvar *objects* (24 199 283 49 -2 3 84))
(defvar *objects* (reverse ("abc" "def" "ghj" "klm")))

(defun min (x y)
  (if (< x y)
      x
    y))

(defun list/min-rec (lst cur)
  (if (> (len lst) 0)
      (list/min-rec (cdr lst) (min cur (car lst)))
    cur))

(defun list/min (lst)
  (list/min-rec (cdr lst) (car lst)))

(defun list/excl-rec (lst acc excl)
  (if (> (len lst) 0)
      (let ((front (car lst)) (rest (cdr lst)) (accl acc))
        ; (println "front" front "rest" rest "accl" accl)
        (if (! (= front excl))
            (setq accl (append front acc)))
        ; (println "accl" accl ", acc" acc)
        (return (list/excl-rec rest accl excl)))
    ;;(progn
    ;;    ; (println "done!" "front" front "rest" rest "acc" acc)
    ;;    (return acc)
    ;;  )
        (return acc)))


;(defun list/sort-rec (lst acc)
;  (if (> (len lst) 0)
;      (list/sort-rec (list/min lst) )
;  )

(defvar *ints* (1 2 3 4))
(println "excl list" *ints* 2 "is" (list/excl-rec *ints* '() 2))

(println "min of list" *objects* "is" (list/min *objects*))
