;------------------------------------------------------------------------------
;theoretical-architecture
;Copyright (c) 2012-2013, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of theoretical-architecture nor the
;      names of its contributors may be used to endorse or promote products
;      derived from this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;-------------------------------------------------------------------------------
; SimulatorComponents .clp - Defines the classes and methods used by the 
;                            simulator
;-------------------------------------------------------------------------------
(defclass cell 
  "Represents a basic storage cell"
  (is-a USER)
  (role abstract)
  (slot cell-type
        (type SYMBOL)
        (storage shared)
        (access read-only)
        (default unknown))
  (slot address
        (type INTEGER)
        (visibility public)
        (storage local)
        (range 0 ?VARIABLE))
  (slot value
        (type INTEGER)
        (range 0 255)
        (visibility public)
        (default-dynamic 0)))
(defclass memory-cell
  "Represents a piece of storage for memory"
  (is-a cell)
  (role concrete)
  (pattern-match reactive)
  (slot cell-type
        (source composite)
        (default memory)))

(defclass cache-cell
  "Represents a cell for a cache"
  (is-a cell)
  (role concrete)
  (pattern-match reactive)
  (slot address 
        (source composite)
        (storage shared)
        (access initialize-only)
        (default 0))
  (slot cell-type
        (source composite)
        (default cache)))

(defclass register
  "Represents a storage location that is \"close\" to the procesor"
  (is-a USER)
  (slot offset
        (type INTEGER)
        (storage local)
        (default ?NONE))
  (slot value
        (type INTEGER)))

(defclass machine
  "Defines an instance of a given machine"
  (is-a USER)
  (multislot registers 
             (type INSTANCE)
             (allowed-classes register)))
(defgeneric next)
(defgeneric next-one)
(defgeneric next-two)
(defgeneric next-three)

(defmethod next
  "A method that makes the next method more understandable"
  ((?n NUMBER)
   (?cells SYMBOL (eq ?cells cells))
   (?from SYMBOL (eq ?from from))
   (?loc NUMBER))
  (next ?n ?loc 1))

(defmethod next
  ((?value NUMBER)
   (?count NUMBER (> ?count 3))
   (?inc NUMBER (!= ?inc 1)))
  (bind ?out (create$))
  (loop-for-count (?i 1 ?count) do
                  (bind ?out (create$ ?out (+ ?value (* ?i ?inc)))))
  (return ?out))

(defmethod next
  ((?value NUMBER)
   (?count NUMBER (> ?count 3))
   (?inc NUMBER (= ?inc 1)))
  (bind ?out (create$))
  (loop-for-count (?i 1 ?count) do
                  (bind ?out (create$ ?out (+ ?value ?i))))
  (return ?out))
(defmethod next
  ((?value NUMBER)
   (?count NUMBER (= ?count 1))
   (?inc NUMBER))
  (create$ (+ ?value ?inc)))
(defmethod next
  ((?value NUMBER)
   (?count NUMBER (= ?count 2))
   (?inc NUMBER (!= ?count 1)))
  (create$ (+ ?value ?inc)
           (+ ?value ?inc ?inc)))
(defmethod next
  ((?value NUMBER)
   (?count NUMBER (= ?count 2))
   (?inc NUMBER (= ?count 1)))
  (create$ (+ ?value 1)
           (+ ?value 2)))

(defmethod next
  ((?value NUMBER)
   (?count NUMBER (= ?count 3))
   (?inc NUMBER (!= ?count 1)))
  (create$ (+ ?value ?inc)
           (+ ?value ?inc ?inc)
           (+ ?value ?inc ?inc ?inc)))

(defmethod next 
  ((?value NUMBER)
   (?count NUMBER (= ?count 3))
   (?inc NUMBER (= ?count 1)))
  (create$ (+ ?value 1)
           (+ ?value 2)
           (+ ?value 3)))

(defmethod next
  ((?value NUMBER)
   (?count NUMBER))
  (next ?value ?count 1))


(defmethod next-one
  ((?value NUMBER)
   (?inc NUMBER))
  (next ?value 1 ?inc))

(defmethod next-one
  ((?value NUMBER))
  (next ?value 1 1))

(defmethod next-two
  ((?value NUMBER)
   (?inc NUMBER))
  (next ?value 2 ?inc))

(defmethod next-two
  ((?value NUMBER))
  (next ?value 2 1))

(defmethod next-three
  ((?value NUMBER)
   (?inc NUMBER))
  (next ?value 3 ?inc))

(defmethod next-three
  ((?value NUMBER))
  (next ?value 3 1))

(deffunction !! 
             "Used as an error function"
             ($?args)
             (printout werror "ERROR: instruction does not have a funcall!!! Halting!" crlf)
             (halt))
