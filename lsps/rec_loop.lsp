;; loops using recursion

(defun loop-simple (n)
    (if (gt n 0)
     (progn
      (println n)
      (loop-simple (- n 1)))
      (progn
        (println "loop-simple completed!"))))

(defun loop-form (n form)
    (if (gt n 0)
     (progn
      (eval form)
      (loop-form (- n 1) form))
      (progn
        (println "loop-form completed!"))))


; (loop-acc-simple 10000 '()) takes 16 s
(defun loop-acc-simple (n acc)
 (if (gt n 0)
   (append 1 (loop-acc-simple (- n 1) acc))
  (progn
   (println "loop-acc-simple completed!")
   acc)))

; (loop-cc-simple2 10000 '()) takes 14 s
(defun loop-acc-simple2 (n acc)
 (if (gt n 0)
   (loop-acc-simple (- n 1) (append 1 acc))
  (progn
   (println "loop-acc-simple completed!")
   acc)))

; (loop-cc-simple3 10000 '()) takes 32 s
(defun loop-acc-simple3 (n)
 (progn
  (defun loop-acc-helper (n acc)
  (if (gt n 0)
   (loop-acc-helper (- n 1) (append 1 acc))
   (progn
    (println "loop-acc-simple completed!")
    acc)))
  (loop-acc-helper n '())))

; (defun loop-acc-helper (n form acc)
;  (if (gt n 0)
;   (progn
;    (loop-acc-helper (- n 1) 'form (append (eval form) acc)))
;   (progn
;    (println "loop-form completed!"))))

; (defun loop-acc (n form)
;     (loop-acc-helper n 'form '()))

; (loop-simple 100)
; (loop-form 100 '(println "test"))
; (loop-acc 100 '1)

; (println (append 1 '(1 2 3)))

;(println "===  simple  ===")
;(println (loop-acc-simple 1000 '()))
;(println "=== simple2 ===")
;(println (loop-acc-simple2 1000 '()))
;(println "=== simple3 ===")
;(println (loop-acc-simple3 10000))


(println (loop-acc-simple2 10000 '()))
