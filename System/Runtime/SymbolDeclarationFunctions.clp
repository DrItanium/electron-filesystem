;------------------------------------------------------------------------------
;electron
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
(deffunction Runtime::blank-on-empty-string(?str)
 "Returns a blank multifield if the given input string is empty"
             (if (> (str-length ?str) 0) then ?str else (create$)))
;------------------------------------------------------------------------------
(deffunction Runtime::split-on-symbol (?sp ?i)
             (bind ?str (if (stringp ?i) then ?i else (str-cat ?i)))
             (bind ?ind (str-index ?sp ?str))
             (bind ?p0 (sub-string 1 (- ?ind 1) ?str))
             (bind ?p1 (sub-string (+ ?ind 1) (str-length ?str) ?str))
             (create$ (blank-on-empty-string ?p0) ?sp
                      (blank-on-empty-string ?p1)))
;------------------------------------------------------------------------------
(deffunction Runtime::input-is-target-symbol (?sp ?input)
             (bind ?str (str-cat ?input))
             (and (neq 0 (str-compare ?str ?sp))
                  (str-index ?sp ?str)))
;------------------------------------------------------------------------------
(deffunction Runtime::defsymbol-lexer
 "Builds a rule for parsing a given input string for the given symbol"
             (?module-name ?symbol ?name ?docString)
             (build (format nil "(defrule %s::%s 
                                          \"%s\"
                                          ?fct <- (object (is-a file-line) 
                                                          (contents $?b ?s $?a))
                                          (test (input-is-target-symbol \"%s\" ?s))
                                          =>
                                          (modify-instance ?fct 
                                                           (contents $?b 
                                                                     (split-on-symbol \"%s\" ?s) 
                                                                     $?a)))" 
                     ?module-name ?name ?docString ?symbol ?symbol)))
;------------------------------------------------------------------------------
