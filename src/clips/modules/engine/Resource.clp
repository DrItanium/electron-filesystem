; Resource.clp - Contains the base classes for the resource types
; See LICENSE for license details
; Written by Joshua Scoggins
(defclass engine::Resource 
 "Base class for any resource used by the engine"
 (is-a Object)
 (role abstract)
 (slot resource-name (type SYMBOL))
 (slot path (type SYMBOL STRING)))
 
(defclass engine::AudioResource 
 "Base class for any audio resource"
 (is-a Resource) 
 (role abstract))

(defclass engine::VisualResource 
 "Base class for any visual resource"
 (is-a Resource) 
 (role abstract))

(defclass engine::DataResource 
 "Base class for any resource that isn't an audio or visual resource"
 (is-a Resource) 
 (role abstract))
