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
; INPUT FACT FORM: (parse constant file ?path)
;------------------------------------------------------------------------------
(deffunction types::get-input-form-factor () 
             (printout t "(parse constant file ?path)" crlf))
;------------------------------------------------------------------------------
(defrule MAIN::open-target-file
         "This rule takes a fact of the above INPUT FORM and attempts to open the file"
         ?fct <- (parse constant file ?path)
         =>
         (bind ?name (gensym*))
         (retract ?fct)
         (if (open ?path ?name "r") then 
           (make-instance of opened-file 
                          (file-id ?name) 
                          (index 0))
           (assert (message (to identify-lines)
                            (action read-file)
                            (arguments ?name)))
           (focus read-input 
                  modify-input 
                  identify-lines 
                  convert-templates 
                  build-groups
                  grouping-update)
           else
           (printout t "ERROR: target file at " ?path " does not exist!" crlf 
                     "Halting!" crlf)
           (halt)))
;------------------------------------------------------------------------------
(defrule read-input::build-file-line
         ?fct <- (message (to identify-lines)
                          (action read-file)
                          (arguments ?name))
         ?obj <- (object (is-a opened-file) 
                         (file-id ?fid))
         =>
         (retract ?fct)
         (bind ?result (send ?obj read-line))
         (if (neq ?result EOF) then
           (duplicate ?fct)
           (if (neq ?result "") then
             (make-instance of file-line (index (send ?obj next-index))
                            (type UNKNOWN)
                            (parent ?fid)
                            (contents (explode$ ?result))))
           else
           (send ?obj close-file)
           (unmake-instance ?obj)))
;------------------------------------------------------------------------------
(defrule modify-input::identify-symbols-with-commas
         ?fct <- (object (is-a file-line)
                         (contents $?before ?symbol $?after))
         (test (and (neq 0 (str-compare (str-cat ?symbol) ","))
                    (str-index "," (str-cat ?symbol))))
         =>
         (bind ?str (if (stringp ?symbol) then ?symbol else (str-cat ?symbol)))
         (bind ?ind (str-index "," ?str))
         (bind ?p0 (sub-string 1 (- ?ind 1) ?str))
         (bind ?p1 (sub-string (+ ?ind 1) (str-length ?str) ?str))
         (modify-instance ?fct (contents $?before ?p0 , ?p1 $?after)))
;------------------------------------------------------------------------------
(defrule modify-input::identify-symbols-with-open-parens
         ?fct <- (object (is-a file-line)
                         (contents $?before ?symbol $?after))
         (test (and (neq 0 (str-compare (str-cat ?symbol) "("))
                    (str-index "(" (str-cat ?symbol))))
         =>
         (bind ?str (if (stringp ?symbol) then ?symbol else (str-cat ?symbol)))
         (bind ?ind (str-index "(" ?str))
         (bind ?p0 (sub-string 1 ?ind ?str))
         (bind ?p1 (sub-string ?ind (str-length ?str) ?str))
         (modify-instance ?fct (contents $?before ?p0 "(" ?p1 $?after)))
;------------------------------------------------------------------------------
(defrule modify-input::identify-symbols-with-close-parens
         ?fct <- (object (is-a file-line)
                         (contents $?before ?symbol&~")" $?after))
         (test (and (neq 0 (str-compare (str-cat ?symbol) ")"))
                    (str-index ")" (str-cat ?symbol))))
         =>
         (bind ?str (if (stringp ?symbol) then ?symbol else (str-cat ?symbol)))
         (bind ?ind (str-index ")" ?str))
         (bind ?p0 (sub-string 1 ?ind ?str))
         (bind ?p1 (sub-string ?ind (str-length ?str) ?str))
         (modify-instance ?fct (contents $?before ?p0 ")" ?p1 $?after)))
;------------------------------------------------------------------------------
(defrule identify-lines::mark-heading-groups
         "Tags lines that consist of /* $? */ as group headings"
         ?f <- (object (is-a file-line) 
                       (type UNKNOWN)
                       (contents /* $? */))
         =>
         (modify-instance ?f (type heading)))
;------------------------------------------------------------------------------
(defrule identify-lines::mark-entry-line
         "Tags lines that are defines and modifies the associated contents"
         ?f <- (object (is-a file-line)
                       (type UNKNOWN) 
                       (contents #define ?name ?))
         =>
         (modify-instance ?f (type #define) (contents ?name)))
;------------------------------------------------------------------------------
(defrule identify-lines::merge-three-line-headers
         "Merges simple three line headings into one rule"
         ?f0 <- (object (is-a file-line) 
                        (index ?i)
                        (type UNKNOWN) 
                        (contents /*))
         ?f1 <- (object (is-a file-line)
                        (index ?i2&:(= ?i2 (+ ?i 1)))
                        (type UNKNOWN) 
                        (contents * $?c))
         ?f2 <- (object (is-a file-line) 
                        (index ?i3&:(= ?i3 (+ ?i 2)))
                        (type UNKNOWN)
                        (contents */))
         =>
         (unmake-instance ?f1 ?f2)
         (modify-instance ?f0 (contents /* $?c */)))
;------------------------------------------------------------------------------
(defrule identify-lines::mark-glapi-calls
         "marks glapi calls"
         ?f <- (object (is-a file-line)
                       (type UNKNOWN)
                       (contents GLAPI $? "(" $? ")"))
         =>
         (modify-instance ?f (type GLAPI-DEF)))
;------------------------------------------------------------------------------
(defrule identify-lines::merge-potential-glapi-calls
         "Merges the next line of a GLAPI call if the line doesn't end with )"
         (declare (salience -1))
         ?f <- (object (is-a file-line)
                       (type UNKNOWN)
                       (contents GLAPI $?vals "(" $?c)
                       (index ?i))
         (test (not (member$ ")" $?c)))
         ?f0 <- (object (is-a file-line)
                        (index ?i0&:(= ?i0 (+ ?i 1)))
                        (type UNKNOWN)
                        (contents $?contents))
         =>
         (unmake-instance ?f)
         (modify-instance ?f0 (contents GLAPI $?vals "(" $?c $?contents)))
;------------------------------------------------------------------------------
(defrule identify-lines::define-header-spans-initial
         "Defines spans between headers"
         (declare (salience -3))
         (object (is-a file-line)
                 (type heading)
                 (parent ?parent)
                 (index ?i)
                 (contents /* $?name */))
         (not (exists (object (is-a heading-span) 
                              (from ?i) 
                              (parent ?parent))))
         (object (is-a file-line)
                 (type heading)
                 (parent ?parent)
                 (index ?i2&:(> ?i2 ?i)))
         =>
         (if (> (- ?i2 ?i) 0) then
           (make-instance of heading-span
                          (header-name (implode$ $?name))
                          (from ?i)
                          (to ?i2)
                          (parent ?parent))))
;------------------------------------------------------------------------------
(defrule identify-lines::modify-header-spans-for-smaller-size
         "Retracts the heading-span in response to finding an earlier heading"
         ?f <- (object (is-a heading-span)
                       (from ?i)
                       (to ?j)
                       (parent ?parent))
         (object (is-a file-line)
                 (type heading)
                 (parent ?parent)
                 (index ?i2&:(> ?j ?i2 ?i)))
         =>
         (modify-instance ?f (to ?i2)))
;------------------------------------------------------------------------------
(defrule convert-templates::retract-unknowns 
         ?f <- (object (is-a file-line) 
                       (type UNKNOWN))
         =>
         (unmake-instance ?f))
;------------------------------------------------------------------------------
(defrule convert-templates::assert-build-groups
         ?f <- (object (is-a file-line) 
                       (type ~UNKNOWN) 
                       (id ?id))
         =>
         (assert (message (to build-groups)
                          (action add-to-span)
                          (arguments ?id))))
;------------------------------------------------------------------------------
(defrule build-groups::build-grouping
         ?f <- (object (is-a heading-span)
                       (from ?i) 
                       (to ?i2) 
                       (parent ?parent)
                       (contents $?c))
         (object (is-a file-line) 
                 (parent ?parent) 
                 (index ?loc&:(> ?i2 ?loc ?i))
                 (id ?id))
         ?msg <- (message (to build-groups)
                          (action add-to-span)
                          (arguments ?id))
         =>
         (retract ?msg)
         (modify-instance ?f (contents $?c ?id)))
;------------------------------------------------------------------------------
(defrule build-groups::delete-still-existing-elements
         (declare (salience -100))
         ?msg <- (message (to build-groups)
                          (action add-to-span)
                          (arguments ?id))
         ?obj <- (object (is-a file-line)
                         (id ?id))
         =>
         (unmake-instance ?obj)
         (retract ?msg))
;------------------------------------------------------------------------------
(defrule grouping-update::generate-constant-if-statement
         (object (is-a heading-span)
                 (header-name ?group)
                 (contents $? ?name $?))
         ?obj <- (object (is-a file-line) 
                         (id ?name)
                         (type #define)
                         (contents ?element))
         =>
         (bind ?str (str-cat ?element))
         (printout t (format nil "//%s" ?group) crlf 
                   (format nil "if(strcmp(input,\"%s\")) { return %s; }" 
                           (sub-string (+ (str-index "_" ?str) 1) 
                                       (str-length ?str) ?str)
                           ?element) crlf crlf))
;------------------------------------------------------------------------------
