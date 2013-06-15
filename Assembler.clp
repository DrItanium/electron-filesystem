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
           ?*symbol-label-instruction* = label
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
           ?*symbol-interrupt-instruction* = interrupt)
;-------------------------------------------------------------------------------
(deftemplate input-line
             (slot line-number
                   (type INTEGER)
                   (default ?NONE))
             (slot original-input
                   (type STRING)
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
             (multislot arguments))
;-------------------------------------------------------------------------------
(deftemplate instruction
             (slot tag 
                   (type SYMBOL))
             (slot arg-count
                   (type INTEGER))
             (slot is-macro ; is it a convienience instruction/operation?
                   (type SYMBOL)
                   (allowed-symbols FALSE TRUE)))
;-------------------------------------------------------------------------------
(deffacts argument-conversion-counts
          (instruction (tag ?*symbol-terminate-instruction*)
                       (arg-count 0))
          (instruction (tag ?*symbol-nop-instruction*)
                       (arg-count 0))
          (instruction (tag ?*symbol-add-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-subtract-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-multiply-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-divide-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-right-shift-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-left-shift-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-equal-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-not-equal-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-less-than-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-greater-than-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-and-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-or-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-not-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-branch-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-load-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-store-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-set-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-interrupt-instruction*)
                       (arg-count 1))
          (instruction (tag ?*symbol-label-instruction*)
                       (arg-count 1)
                       (is-macro TRUE)))
;-------------------------------------------------------------------------------
(defgeneric to-char)
;-------------------------------------------------------------------------------
(defmethod to-char
  ((?i INTEGER (>= ?i 0)) ; no negative numbers
   (?stream SYMBOL))
  (format ?stream "%c" ?i))

(defmethod to-char
  ((?i INTEGER (>= ?i 0)))
  (to-char ?i nil))

;-------------------------------------------------------------------------------
; since we can't do "knowledge construction" in the standard sense we need to
; pull input from standard-in.
;-------------------------------------------------------------------------------
(defrule initialize-assembler
         (initial-fact)
         =>
         (assert (read-to-end 1)))

(defrule build-input-line
         ?f <- (read-to-end ?i)
         =>
         (retract ?f)
         (bind ?input (readline))
         (if (neq ?input EOF) then
           (assert (read-to-end (+ ?i 1))
                   (input-line (line-number ?i)
                               (original-input ?input)
                               (raw-input (explode$ ?input))))
           else
           (assert (translate-input))))

(defrule throw-out-empty-lines
         "remove lines that are completely comments or just plain empty"
         (declare (salience 2)) 
         ?f <- (input-line (raw-input))
         =>
         (retract ?f))
(defrule invalid-line-found
         (declare (salience 2))
         (input-line (line-number ?i)
                     (original-input ?line)
                     (raw-input $?input))
         (test (or (> (length$ $?input) 4)))
         =>
         (printout werror "ERROR: line " ?i " is invalid" crlf
                   "     " ?line crlf)
         (halt))

(defrule is-invalid-instruction
         (declare (salience 2)) 
         (input-line (raw-input ?op $?)
                     (line-number ?l)
                     (original-input ?input))
         (not (exists (instruction (tag ?op))))
         =>
         (printout werror 
                   (format nil "ERROR: Instruction %s on line %d is invalid%n" ?op ?l)
                   (format nil " Line: %s%n%n" ?input))
         (halt))
(defrule invalid-argument-count-for-operation
         (declare (salience 2))
         (input-line (raw-input ?operation $?rest)
                     (line-number ?l)
                     (original-input ?input))
         (instruction (tag ?operation)
                      (arg-count ?ac&:(!= ?ac (length$ $?rest))))
         =>
         (printout werror 
                   (format nil "ERROR: Invalid number of arguments for %s on line %d%n"
                           ?operation ?l)
                   (format nil " Line: %s%n%n" ?input))
         (halt))

(defrule process-input-line
         (declare (salience 1)) ; evaluate each line as it goes through
         ?f <- (input-line (raw-input ?operation $?rest)
                           (line-number ?l))
         (instruction (tag ?operation)
                      (arg-count =(length$ $?rest)))

         =>
         (retract ?f)
         (assert (operation (line-number ?l)
                            (operation ?operation)
                            (arguments $?rest))))
;-------------------------------------------------------------------------------
; this becomes a script that reads from standard input
;-------------------------------------------------------------------------------
(reset) ; load the instruction information for our purposes
(run)
(exit)
