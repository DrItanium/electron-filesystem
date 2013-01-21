; Screen.clp - a clips representation of the screen
(defclass engine::Screen 
 "A high-level representation of a display"
 (is-a Object)
 (slot screen-number (type INTEGER) (range 0 ?VARIABLE))
 (slot width (type INTEGER) (range 0 ?VARIABLE))
 (slot height (type INTEGER) (range 0 ?VARIABLE)))

