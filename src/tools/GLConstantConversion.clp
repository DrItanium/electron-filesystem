;------------------------------------------------------------------------------
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
;    * Neither the name of Joshua Scoggins nor the
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
; GLConstantConversion.clp - an expert system that reads a define and generates
; conversion code for it. 
;------------------------------------------------------------------------------
(defclass opened-file 
  (is-a USER)
  (slot id)
  (slot index (type INTEGER) (range 0 ?VARIABLE))
  (message-handler next-index)
  (message-handler read-line)
  (message-handler close-file))
;------------------------------------------------------------------------------
(defmessage-handler opened-file next-index ()
                    (bind ?old ?self:index)
                    (bind ?self:index (+ ?old 1))
                    (return ?old))
;------------------------------------------------------------------------------
(defmessage-handler opened-file read-line ()
                    (readline ?self:id))
;------------------------------------------------------------------------------
(defmessage-handler opened-file close-file ()
                    (close ?self:id))
;------------------------------------------------------------------------------
(deftemplate file-line 
             (slot index)
             (slot parent)
             (slot type)
             (multifield contents))
;------------------------------------------------------------------------------
(deftemplate heading-span
             "Defines a span between two different headings"
             (slot from)
             (slot to)
             (slot parent)
             (slot distance))
;------------------------------------------------------------------------------
; INPUT FACT FORM: (parse constant file ?path)
;------------------------------------------------------------------------------
(deffunction get-input-form-factor () 
             (printout t "(parse constant file ?path)" crlf))
;------------------------------------------------------------------------------
(defrule open-target-file
         "This rule takes a fact of the above INPUT FORM and attempts to open the file"
         ?fct <- (parse constant file ?path)
         =>
         (bind ?name (gensym*))
         (retract ?fct)
         (if (open ?path ?name "r") then 
           (make-instance of opened-file (id ?name) (index 0))
           (assert (read file ?name))
           else
           (printout t "ERROR: target file at " ?path " does not exist!" crlf 
                     "Halting!" crlf)
           (halt)))
;------------------------------------------------------------------------------
(defrule build-file-line
         ?fct <- (read file ?fid)
         ?obj <- (object (is-a opened-file) 
                         (id ?fid))
         =>
         (retract ?fct)
         (bind ?result (send ?obj read-line))
         (if (neq ?result EOF) then
           (assert (read file ?fid)
                   (file-line (index (send ?obj next-index))
                              (type UNKNOWN)
                              (parent ?fid)
                              (contents (explode$ ?result))))
           else
           (send ?obj close-file)
           (unmake-instance ?obj)))
;------------------------------------------------------------------------------
(defrule retract-unknowns 
         (declare (salience -10))
         ?f <- (file-line (type UNKNOWN))
         =>
         (retract ?f))
;------------------------------------------------------------------------------
(defrule mark-heading-groups
         "Tags lines that consist of /* $? */ as group headings"
         ?f <- (file-line (type UNKNOWN)
                          (contents /* $? */))
         =>
         (modify ?f (type heading)))
;------------------------------------------------------------------------------
(defrule mark-entry-line
         "Tags lines that are defines and modifies the associated contents"
         ?f <- (file-line (type UNKNOWN) 
                          (contents #define ?name ?))
         =>
         (modify ?f (type #define) (contents ?name)))
;------------------------------------------------------------------------------
(defrule merge-three-line-headers
         "Merges simple three line headings into one rule"
         ?f0 <- (file-line (index ?i)
                           (type UNKNOWN) 
                           (contents /*))
         ?f1 <- (file-line (index ?i2&:(= ?i2 (+ ?i 1)))
                           (type UNKNOWN) 
                           (contents * $?c))
         ?f2 <- (file-line (index ?i3&:(= ?i3 (+ ?i 2)))
                           (type UNKNOWN)
                           (contents */))
         =>
         (retract ?f1 ?f2)
         (modify ?f0 (contents /* $?c */)))
;------------------------------------------------------------------------------
(defrule mark-glapi-calls
         "marks glapi calls"
         ?f <- (file-line (type UNKNOWN)
                          (contents GLAPI $? "(" $? ")"))
         =>
         (modify ?f (type GLAPI-DEF)))
;------------------------------------------------------------------------------
(defrule merge-potential-glapi-calls
         "Merges the next line of a GLAPI call if the line doesn't end with )"
         (declare (salience -1))
         ?f <- (file-line (type UNKNOWN)
                          (contents GLAPI $?vals "(" $?c)
                          (index ?i))
         (test (not (member$ ")" $?c)))
         ?f0 <- (file-line (index ?i0&:(= ?i0 (+ ?i 1)))
                           (type UNKNOWN)
                           (contents $?contents))
         =>
         (retract ?f)
         (modify ?f0 (contents GLAPI $?vals "(" $?c $?contents)))
;------------------------------------------------------------------------------
(defrule define-header-spans-initial
         "Defines spans between headers"
         (declare (salience -3))
         (file-line (type heading)
                    (parent ?parent)
                    (index ?i))
         (not (exists (heading-span (from ?i) 
                                    (parent ?parent))))
         (file-line (type heading)
                    (parent ?parent)
                    (index ?i2&:(> ?i2 ?i)))
         =>
         (bind ?difference (- ?i2 ?i))
         (if (> ?difference 0) then
           (assert (heading-span (from ?i) 
                                 (to ?i2) 
                                 (parent ?parent) 
                                 (distance ?difference)))))
;------------------------------------------------------------------------------
(defrule modify-header-spans-for-smaller-size
         "Retracts the heading-span in response to finding an earlier heading"
         ?f <- (heading-span (from ?i)
                             (to ?j)
                             (parent ?parent))
         (file-line (type heading)
                    (parent ?parent)
                    (index ?i2&:(> ?j ?i2 ?i)))
         =>
         (modify ?f (to ?i2) 
                 (distance (- ?i2 ?i))))
;------------------------------------------------------------------------------
