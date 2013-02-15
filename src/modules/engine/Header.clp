; Header.clp Describes the header for the engine module
; See LICENSE for license details
; Written by Joshua Scoggins

(defmodule engine 
 (import cortex ?ALL)
 (export ?ALL))

(load* "src/clips/modules/engine/Entity.clp")
(load* "src/clips/modules/engine/Resource.clp")
(load* "src/clips/modules/engine/TextResource.clp")
