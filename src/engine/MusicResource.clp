; MusicResource.clp - A resource representing a music resource 
; See LICENSE for license details
; Written by Joshua Scoggins
(defclass engine::MusicResource 
 "A class representing a music resource"
 (is-a AudioResource)
 (role concrete)
 (slot track-name (slot SYMBOL STRING))
 (slot track-number (slot INTEGER) (range 0 ?VARIABLE)))

