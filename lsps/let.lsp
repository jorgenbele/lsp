(defvar x 1)
(println "global x" x)
(let ((x 2))
    (println "local x" x))
(println "global" x)
(println "============")
(setq x 0)
(println "global" x)
(let ((x 1))
 (println "#1" x) 
 (let ((x 2))
  (println "#2" x)
  (let ((x 3))
   (println "#3" x))
  (println "#2" x))
 (println "#1" x))

(println "global" x)

(defvar y 1)
(println "global x" x)
(let ((x 2) (y 2))
    (println "local x:" x  "y:" y))
(println "global x:" x "y:" y)
(println "============")
(setq x 0)
(setq y 0)
(println "global x:" x "y:" y)
(let ((x 1) (y 1))
 (println "#1" x y)    ; #1 1 1
 (let ((x 2) (y 2))
  (println "#2" x y)   ; #2 2 2
  (let ((x 3) (y 3))
   (println "#3" x y)) ; #3 3 3
  (println "#2" x y))  ; #2 2 2
 (println "#1" x y))   ; #1 1 1

; Outputs
#1 1 1
#2 2 2
#3 3 3
#2 2 2
#1 1 1

(println "global" x)
