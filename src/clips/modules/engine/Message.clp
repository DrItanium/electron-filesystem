; Message.clp - a basic template type that is used to pass messages between
; different modules
; Written by Joshua Scoggins
(deftemplate engine::message
				 "A standardized way to pass facts between modules"
				 (slot from)
				 (slot to)
				 (slot action)
				 (multislot arguments))

