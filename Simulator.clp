;------------------------------------------------------------------------------
;electron
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
;    * Neither the name of The Adventure Engine nor the
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
; Simulator.clp - Defines a simple computer to be executed through an expert
;                 system. This computer is unique in that instructions declare
;                 actions instead of perform actions directly.
;
;                 The Theoretical Architecture is an 8-bit declarative
;                 processor architecture. It has an 8-bit direct address space
;                 that can be extended through the use of jump instructions.
;-------------------------------------------------------------------------------
(defclass operation
 "Represents an instruction to execute"
 (is-a USER)
 (role abstract)
 (slot address
  (type INTEGER)
  (visibility public)
  (storage local)
  (range 0 ?VARIABLE)
  (default ?NONE))
 (slot name
  (type SYMBOL)
  (visibility public)
  (storage local)
  (default ?NONE)))
(defclass cell 
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
  (is-a cell)
  (role concrete)
  (slot cell-type
        (source composite)
        (default memory)))

(defclass cache-cell
  (is-a cell)
  (role concrete)
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
  (is-a USER)
  (multislot registers 
             (type INSTANCE)
             (allowed-classes register)))
;TODO: automate the generation of the register set
(defrule setup-machine
         (initial-fact)
         =>
         (bind ?registers (instance-name (make-instance pc of register (offset 256))))
         (loop-for-count (?i 0 255) do
                         (bind ?registers (create$ ?registers (instance-name (make-instance (sym-cat r ?i) of register (offset ?i))))))
         (make-instance proc of machine 
                        (registers ?registers))
         (assert (Machine setup)))
(defrule load-program-into-memory
         ?s <- (Machine setup)
         ?f <- (load ?program-path into memory)
         =>
         ; the encoding of this processor can change from time to time but my
         ; current idea is to have a 255 byte window that represents a set of
         ; actions to be performed by the machine itself.
         (retract ?s ?f)
         ;build the memory cells right now
         (if (open ?program-path file "r") then
           (bind ?this (get-char file))
           (bind ?i 0)
           (while (!= ?this -1) do
                  (make-instance of memory-cell
                                 (address ?i)
                                 (value ?this))
                  (bind ?this (get-char file))
                  (bind ?i (+ ?i 1)))
           (close ?file)
           (assert (stage decode execute restart))
           else
           (printout t "ERROR: Couldn't load program" crlf)
           (halt)))
(defrule next-stage
         (declare (salience -10000))
         ?f <- (stage ? $?rest)
         =>
         (retract ?f)
         (if (> (length$ ?rest) 0) then
           (assert (stage $?rest))))
(defrule decode-instruction
         (stage decode $?)
         (object (is-a register)
                 (name [pc])
                 (value ?location))
         (object (is-a memory-cell)
          (address ?location)
          (value ?operation))


