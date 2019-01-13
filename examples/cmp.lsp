; Operations
(gt x y)  ; x > y
(lt x y)  ; x < y
(eql x y) ; x == y
(not x)   ; C-style logical not; true if x is 0, false if x is non-zero

; Usage
(println *x* "==" *y* "=>" (if (eql *x* *y*) "true" "false"))      ; prints "true" if *x* == *y*
(println *x* ">" *y* "=>" (if (gt *x* *y*) "true" "false"))        ; prints "true" if *x* > *y*
(println *x* "<" *y* "=>" (if (lt *x* *y*) "true" "false"))        ; prints "true" if *x* < *y
(println *x* "<=" *y* "=>" (if (not (gt *x* *y*)) "true" "false")) ; prints "true" if *x* <= *y
(println *x* ">=" *y* "=>" (if (not (lt *x* *y*)) "true" "false")) ; prints "true" if *x* >= *y
