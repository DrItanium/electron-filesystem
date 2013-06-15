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
; Assembler.clp - An assembler for the theoretical processor.
;-------------------------------------------------------------------------------
(defglobal MAIN ; human readable form
           ?*symbol-terminate-instruction* = terminate
           ?*symbol-nop-instruction* = nop
           ?*symbol-add-instruction* = add
           ?*symbol-subtract-instruction* = sub
           ?*symbol-multiply-instruction* = mul
           ?*symbol-divide-instruction* = div
           ?*symbol-right-shift-instruction* = right-shift
           ?*symbol-left-shift-instruction* = left-shift
           ?*symbol-equal-instruction* = eq
           ?*symbol-not-equal-instruction* = neq
           ?*symbol-less-than-instruction* = lt
           ?*symbol-greater-than-instruction* = gt
           ?*symbol-and-instruction* = and
           ?*symbol-or-instruction* = or
           ?*symbol-not-instruction* = not
           ?*symbol-branch-instruction* = branch
           ?*symbol-load-instruction* = load
           ?*symbol-store-instruction* = store
           ?*symbol-set-instruction* = set
           ?*symbol-interrupt-instruction* = interrupt
           )
;-------------------------------------------------------------------------------
(deftemplate input-line 
             (slot line-number
                   (type INTEGER)
                   (default ?NONE))
             (multislot raw-input))

;-------------------------------------------------------------------------------
(deftemplate operation
             (slot line-number
                   (type INTEGER)
                   (default ?NONE))
             (slot operation
                   (type SYMBOL)
                   (default ?NONE))
             (slot destination 
                   (type SYMBOL)
                   (default ?NONE))
             (slot source0
                   (type NUMBER INSTANCE)
                   (default ?NONE))
             (slot source1
                   (type INSTANCE)))
;-------------------------------------------------------------------------------
(defgeneric to-char)

(defmethod to-char
  ((?i INTEGER (>= ?i 0)) ; no negative numbers
   (?stream SYMBOL))
  (format ?stream "%c" ?i))

(defmethod to-char
  ((?i INTEGER (>= ?i 0)))
  (to-char ?i nil))

; since we can't do "knowledge construction" in the standard sense we need to
; pull input from standard-in.
(defrule initialize-assembler
         (initial-fact)
         =>
         (assert (read-to-end 0)))

(defrule build-input-line
         ?f <- (read-to-end ?i)
         =>
         (retract ?f)
         (bind ?input (readline))
         (if (neq ?input EOF) then
           (assert (read-to-end (+ ?i 1))
                   (input-line (line-number ?i)
                               (raw-input (explode$ ?input))))))

(defrule throw-out-empty-lines
         "remove lines that are completely comments or just plain empty"
         (declare (salience 2)) 
         ?f <- (input-line (raw-input))
         =>
         (retract ?f))

(defrule process-input-line:two-input
         (declare (salience 1)) ; evaluate each line as it goes through
         ?f <- (input-line (raw-input ?operation ?dest ?source)
                           (line-number ?l))
         =>
         (retract ?f)
         (assert (operation (line-number ?l)
                            (operation ?operation)
                            (destination ?dest)
                            (source0 ?source))))

(defrule process-input-line:three-input
         (declare (salience 1)) ; evaluate each line as it goes through
         ?f <- (input-line (raw-input ?operation ?dest ?source0 ?source1)
                           (line-number ?l))
         =>
         (retract ?f)
         (assert (operation (line-number ?l)
                            (operation ?operation)
                            (destination ?dest)
                            (source0 ?source0)
                            (source1 ?source1))))
(defrule is-invalid-instruction
         ?f <- (operation (line-number ?l)
                          (operation ?operation&:(not (instruction-exists
                                                       ?operation))))
         =>
         (

;-------------------------------------------------------------------------------
; this becomes a script that reads from standard input
(run)
(exit)
