; Header.clp Describes the header for the engine module
; See LICENSE for license details
; Written by Joshua Scoggins

(defmodule engine (export ?ALL))

(load* "src/clips/modules/engine/Object.clp")
(load* "src/clips/modules/engine/Entity.clp")
(load* "src/clips/modules/engine/Resource.clp")
(load* "src/clips/modules/engine/TextResource.clp")
(load* "src/clips/modules/engine/Message.clp")
