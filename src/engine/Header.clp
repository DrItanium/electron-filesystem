; Header.clp Describes the header for the engine module
; See LICENSE for license details
; Written by Joshua Scoggins

(defmodule engine (export ?ALL))

(load* "src/engine/Object.clp")
(load* "src/engine/Resource.clp")
(load* "src/engine/ResourceEntry.clp")
(load* "src/engine/MusicResource.clp")
(load* "src/engine/SoundResource.clp")
