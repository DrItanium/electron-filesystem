;------------------------------------------------------------------------------
;electron-filesystem
;Copyright (c) 2013, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of electron-filesystem nor the
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
;------------------------------------------------------------------------------
(if (neq (operating-system) Linux) then
  (printout werror "ERROR: This only works on linux" crlf)
  (exit -1))

(load* (fs /sys/core/message.clp))
(load* (fs /sys/core/strings.clp))
(defclass processor
  (is-a USER)
  (slot processor)
  (slot vendor_id)
  (slot cpu-family)
  (slot model)
  (slot model-name)
  (slot stepping)
  (slot microcode)
  (slot cpu-MHz)
  (slot cache-size)
  (slot physical-id)
  (slot siblings)
  (slot core-id)
  (slot cpu-cores)
  (slot apicid)
  (slot initial-apicid)
  (slot fpu)
  (slot fpu_exception)
  (slot cpuid-level)
  (slot wp)
  (multislot flags)
  (slot bogomips)
  (slot clflush-size)
  (slot cache_alignment)
  (slot address-sizes)
  (slot power-management))

(defrule open-file
         (initial-fact)
         =>
         (if (open /proc/cpuinfo /proc/cpuinfo "r") then
           (assert (message (action /proc/cpuinfo)
                            (contents (readline /proc/cpuinfo))))
           else
           (printout werror "ERROR: Couldn't open /proc/cpuinfo!" crlf)
           (exit -1)))

(defrule build-up:/proc/cpuinfo
         ?f <- (message (action /proc/cpuinfo)
                        (contents $?lines))
         =>
         (bind ?v (readline /proc/cpuinfo))
         (if (eq ?v EOF) then 
           (close /proc/cpuinfo)
           (modify ?f (action build-entries))
           else
           (modify ?f (contents $?lines ?v))))

(defrule build-processor-entry
         ?f <- (message (action build-entries)
                        (contents $?contents "" $?rest))
         (test (not (member$ "" $?contents)))
         =>
         (modify ?f (contents $?rest))
         (duplicate ?f 
                    (action processor-entry)
                    (from (instance-name (make-instance of processor)))
                    (contents $?contents)))

(defrule finish-building-entries
         ?f <- (message (action build-entries)
                        (contents))
         =>
         (retract ?f))

(defrule create-processor-entries
         ?f <- (message (action processor-entry)
                        (from ?uid)
                        (contents ?line $?rest))
         =>
         (modify ?f (contents $?rest))
         (duplicate ?f (action remove-colon)
                    (contents ?line)))

(defrule finish-processor-decomposition
         ?f <- (message (action processor-entry)
                        (contents))
         =>
         (retract ?f))

(defrule remove-colon
         ?f <- (message (action remove-colon)
                        (contents ?line))
         (test (contains ":" ?line))
         =>
         (bind ?ind (str-index ":" ?line))
         (modify ?f (action mangle-name)
                 (contents (sub-string 1 (- ?ind 1) ?line)
                           (sub-string (+ ?ind 1) (str-length ?line) ?line))))

(defrule mangle-name
         ?f <- (message (action mangle-name)
                        (contents ?name ?contents))
         =>
         (bind ?ename (explode$ ?name))
         (if (multifieldp ?ename) then
           (bind ?bname (nth$ 1 ?ename))
           (progn$ (?z (rest$ ?ename))
                   (bind ?bname (format nil "%s-%s" ?bname ?z)))
           (modify ?f (action parse-entry)
                   (contents (sym-cat ?bname) ?contents))
           else
           (modify ?f (action parse-entry)
                   (contents (sym-cat ?ename) ?contents))))

(defrule parse-entry
         ?f <- (message (action parse-entry)
                        (contents ?name $?contents))
         =>
         (modify ?f (action ?name)
                 (to populate)
                 (contents $?contents)))

(defrule populate-object:flags
         (declare (salience 1))
         ?f <- (message (to populate)
                        (from ?name)
                        (action flags)
                        (contents ?line))
         ?proc <- (object (is-a processor)
                          (name ?name))
         =>
         (retract ?f)
         (send ?proc put-flags (explode$ ?line)))

(defrule populate-object:model-name
         (declare (salience 1))
         ?f <- (message (to populate)
                        (from ?name)
                        (action model-name)
                        (contents ?line))
         ?proc <- (object (is-a processor)
                          (name ?name))
         =>
         (retract ?f)
         (send ?proc put-model-name (sub-string 2 (str-length ?line) ?line)))

(defrule populate-object:generic
         ?f <- (message (to populate)
                        (from ?name)
                        (action ?field)
                        (contents ?value&~""))
         ?proc <- (object (is-a processor)
                          (name ?name))
         =>
         (retract ?f)
         (send ?proc (sym-cat put- ?field) (string-to-field ?value)))

(defrule populate-object:empty
         ?f <- (message (to populate)
                        (from ?name)
                        (action ?field)
                        (contents ""))
         ?proc <- (object (is-a processor)
                          (name ?name))
         =>
         (retract ?f)
         (send ?proc (sym-cat put- ?field) nil))

(defrule print-processor
         (declare (salience -1))
         ?f <- (object (is-a processor))
         =>
         (printout t "(")
         (send ?f print)
         (printout t ")" crlf))
