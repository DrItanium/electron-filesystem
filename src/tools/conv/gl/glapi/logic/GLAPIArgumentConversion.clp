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
; GLAPIArgumentConversion.clp
; 
; Written by Joshua Scoggins
;------------------------------------------------------------------------------
(defrule grouping-update::construct-function-argument-builder
			?f <- (message (to grouping-update)
								(action construct-entry)
								(arguments ?arg => ?name))
			(object (is-a GLAPIArgument)
					  (id ?arg)
					  (index ?index)
					  (is-pointer FALSE))
			=>
			(make-instance ?name of CLIPSGLAPIArgumentBuilder
								(index (+ 1 ?index))
								(parent ?arg)
								(argument-name-base (sym-cat (format nil "arg%d" ?index))))
			(modify ?f (action make-variable-declaration))
			(duplicate ?f (action make-type-check-code))
			(duplicate ?f (action make-data-object-declaration))
			(duplicate ?f (action make-conversion-code)))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-function-argument-builder-multifield
			?f <- (message (to grouping-update)
								(action construct-entry)
								(arguments ?arg => ?name))
			(object (is-a GLAPIArgument)
					  (id ?arg)
					  (index ?index)
					  (is-pointer TRUE))
			=>
			(make-instance ?name of CLIPSGLAPIMultifieldArgumentBuilder 
								(index (+ 1 ?index))
								(parent ?arg)
								(argument-name-base (sym-cat (format nil "arg%d" ?index))))
			(modify ?f (action make-variable-declaration))
			(duplicate ?f (action make-type-check-code))
			(duplicate ?f (action make-data-object-declaration))
			(duplicate ?f (action make-multifield-conversion-code))
			(duplicate ?f (action make-conversion-code)))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-function-argument-builder-multifield-fixed
			(declare (salience 1))
			?f <- (message (to grouping-update)
								(action construct-entry)
								(arguments ?arg => ?name))
			(object (is-a GLAPIFixedArrayArgument)
					  (id ?arg)
					  (index ?index)
					  (size ?size))
			=>
			(make-instance ?name of CLIPSGLAPIFixedMultifieldArgumentBuilder 
								(index (+ 1 ?index))
								(parent ?arg)
								(multifield-size ?size)
								(argument-name-base (sym-cat (format nil "arg%d" ?index))))
			(modify ?f (action make-variable-declaration))
			(duplicate ?f (action make-type-check-code))
			(duplicate ?f (action make-data-object-declaration))
			(duplicate ?f (action make-multifield-conversion-code))
			(duplicate ?f (action make-conversion-code)))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-multifield-conversion-code
			?f <- (message (to grouping-update)
								(action make-multifield-conversion-code)
								(arguments ?arg => ?name))
			?o <- (object (is-a CLIPSGLAPIMultifieldArgumentBuilder)
							  (id ?name)
							  (argument-name-base ?anb))
			=>
			(modify ?f (action completed-make-multifield-conversion-code))
			(bind ?baseName (sym-cat (format nil "%s_MF" ?anb)))
			(modify-instance ?o 
								  (multifield-pointer-argument-name ?baseName)
								  (multifield-pointer-declaration (format nil "void* %s;" ?baseName))))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-data-object-declaration
			?f <- (message (to grouping-update)
								(action make-data-object-declaration)
								(arguments ?arg => ?name))
			?ab <- (object (is-a CLIPSGLAPIArgumentBuilder)
								(id ?name)
								(argument-name-base ?anb))
			=>
			(modify ?f (action completed-data-object-declaration))
			(bind ?doArg (sym-cat (format nil "%s_DO" ?anb)))
			(modify-instance ?ab 
								  (data-object-argument-name ?doArg)
								  (data-object-declaration 
									 (format nil "DATA_OBJECT %s;" ?doArg))))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-type-check-contents
			?fct <- (message (to grouping-update)
								  (action make-type-check-code)
								  (arguments ?arg => ?name))
			(object (is-a GLAPIArgument)
					  (id ?arg)
					  (parent ?p)
					  (argument-type ?arg-type)
					  (is-pointer FALSE))
			(object (is-a GLAPIFunction)
					  (id ?p)
					  (function-name ?fname))
			?b <- (object (is-a CLIPSGLAPIArgumentBuilder)
							  (id ?name))
			=>
			)
;------------------------------------------------------------------------------
