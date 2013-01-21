; Object.clp - Base class for all objects in Pathways
; See LICENSE for licensing details
; Written by Joshua Scoggins

(defclass engine::Object 
 "Base class for all custom objects in pathways"
 (is-a USER)
 (slot id (type SYMBOL) (access initialize-only) (visibility public)))

(defmessage-handler engine::Object init around ()
 (call-next-handler)
 (bind ?self:id (instance-name-to-symbol (instance-name ?self))))


