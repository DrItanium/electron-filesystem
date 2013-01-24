; Resource.clp - Contains the class definition for text resources 
; See LICENSE for license details
; Written by Joshua Scoggins

(defclass engine::TextResource
 "Base class for any text resources"
 (is-a DataResource)
 (role concrete)
 (slot text (type STRING)))
