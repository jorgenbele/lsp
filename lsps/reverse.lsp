(defun gen-list (n) 
 (progn
  (defun gen-list-rec (n acc)
   (if (gt n 0)
    (gen-list-rec (- n 1) (append n acc))
    (acc)))
  (gen-list-rec n '())))

(defvar *long-list* '())
(setq *long-list* (gen-list 1000))
(println "*long-list*" *long-list*)

; reverse using the 'reverse' builtin
(defvar *reversed-list* '())
(setq *reversed-list* (reverse *long-list*))
(println "reversed using builtin:" *reversed-list*)

(defun reverse-list-rec (lst acc)
 (progn
  (if (> (len lst) 0)
   (progn
    (println "recursing" lst acc)
    (reverse-list-rec (cdr lst) (cons (car lst) acc)))
   (return acc))))

(defun reverse-list (lst)
  (progn
    (println "Starting recursive reverse!")
    (reverse-list-rec lst '())))

;; reverse using the 'reverse-list' function 
(setq *reversed-list-recursive* (reverse-list *long-list*))
(println "reversed using recursive function:" *reversed-list-recursive*)
