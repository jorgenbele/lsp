(setq *x* 99)
(setq *y* 88)

(if 
    (not (lt *x* *y*))
    (println *x* ">=" *y*)
    (println "!" *x* ">=" *y*))

