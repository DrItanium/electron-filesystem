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
(load* MachineCommon.clp)
;-------------------------------------------------------------------------------
(defglobal MAIN
           ?*symbol-label-instruction* = label
           )
(deffacts macro-instructions
          (instruction (tag ?*symbol-label-instruction*)
                       (arg-count 1)
                       (is-macro TRUE)))
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
         (test (> (length$ $?input) 4))
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

;TODO: Build assembly generator
;TODO: Mark labels and data instructions as a way to setup a jump table ahead
;      of time.
(defrule encode-operation:core-instruction:set
         "Take a generated operation and construct an encoded version"
         (declare (salience 2))
         ?op <- (operation (line-number ?l)
                           (operation set)
                           (arguments $?rest))
         ?inst <- (instruction (tag set)
                               (machine-tag ?mt)
                               (is-macro FALSE))
         =>
         ; encode the machine tag in the first "byte"
         (retract ?op)
         (put-char ?mt)
         (put-char (register-name-to-index (nth$ 1 ?rest)))
         (progn$ (?z (slice8 (nth$ 2 ?rest)))
                 (put-char ?z)))
         
(defrule encode-operation:core-instruction:generic
         "Take a generated operation and construct an encoded version"
         (declare (salience 1))
         ?op <- (operation (line-number ?l)
                           (operation ?operation&~set)
                           (arguments $?rest))
         ?inst <- (instruction (tag ?operation)
                               (machine-tag ?mt)
                               (is-macro FALSE))
         =>
         ; encode the machine tag in the first "byte"
         (retract ?op)
         (put-char ?mt)
         (progn$ (?r $?rest)
                 (put-char (register-name-to-index ?r))))
         
         
;-------------------------------------------------------------------------------
; this becomes a script that reads from standard input
;-------------------------------------------------------------------------------
(reset) ; load the instruction information for our purposes
(run)
(exit)
