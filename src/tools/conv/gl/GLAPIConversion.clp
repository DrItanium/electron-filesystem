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
; GLAPIConversion.clp - an expert system that reads a define and generates
; corresponding CLIPS functions that convert the CLIPS values into
; corresponding opengl types. This gets a little goofy when dealing with
; multifields but the idea is sound. 
;------------------------------------------------------------------------------
; We need to add functionality to diverge asterisks from a symbol
;------------------------------------------------------------------------------
(target-symbol-is-special "*" 
								  identify-symbols-with-asterisks 
								  "Splits asterisks (*) out of symbols if necessary")
;------------------------------------------------------------------------------
; Can't escape with double quotes with ] and [...strange
;------------------------------------------------------------------------------
(target-symbol-is-special "["
								  identify-symbols-with-open-square-bracket
								  "Splits an input string on [")
;------------------------------------------------------------------------------
(target-symbol-is-special "]"
								  identify-symbols-with-close-square-bracket
								  "Splits an input string on ]")
;------------------------------------------------------------------------------
; Unlike GLConstantConversion, I don't need to use the heading span objects.
; This is because each file-line has already been correctly merged.
;------------------------------------------------------------------------------
(defrule build-groups::delete-heading-span
			"We don't need heading spans in this expert system. So delete them"
			?obj <- (object (is-a heading-span))
			=>
			(unmake-instance ?obj))
;------------------------------------------------------------------------------
; We need to define a transformation function/rule f which is responsible for
; taking in a knowledge representation of the original code from the input
; language and translates it to a knowledge representation of the code in the
; output language. In this case it is C => C with the added "benefit" of
; needing to add CLIPS api calls to interact with the overall environment. 
;
; This isn't really that hard as what we need to do is just parse the arguments
; and generate the corresponding C code. 
;------------------------------------------------------------------------------
(defclass types::GLAPIFunction
  "Defines a given GLAPI function"
  (is-a Object)
  (slot return-type (type SYMBOL STRING))
  (slot function-name)
  (slot clips-function-name)
  (multislot arguments)
  (message-handler add-argument))
;------------------------------------------------------------------------------
(defmessage-handler types::GLAPIFunction add-argument 
						  "Adds the arg name and returns the index"
						  (?name)
						  (bind ?index (length$ ?self:arguments))
						  (if (= 0 ?index) then
							 (slot-direct-insert$ arguments 1 ?name)
							 (return 1)
							 else
							 (slot-direct-insert$ arguments ?index ?name)
							 (return ?index)))
;------------------------------------------------------------------------------
(defclass types::GLAPIArgument
  "Defines a given GLAPI function argument"
  (is-a Object)
  (slot argument-type (visibility public))
  (slot argument-name)
  (slot index)
  (slot is-constant (type SYMBOL) (allowed-values FALSE TRUE))
  (slot is-pointer (type SYMBOL) (allowed-values FALSE TRUE))
  (message-handler reconstitute))
;------------------------------------------------------------------------------
(defmessage-handler types::GLAPIArgument reconstitute
						  ()
						  (return (format nil "%s %s %s %s" 
												(if ?self:is-constant then "const" else "")
												?self:argument-type
												(if ?self:is-pointer then "*" else "")
												?self:argument-name)))
;------------------------------------------------------------------------------
(defclass types::GLAPIFixedArrayArgument
  "Refers to fixed size arrays"
  (is-a GLAPIArgument)
  (slot array-size)
  (message-handler reconstitute around))
;------------------------------------------------------------------------------
(defmessage-handler types::GLAPIFixedArrayArgument reconstitute around 
						  ()
						  (return (format nil "%s[%d]" 
												(call-next-handler)
												?self:array-size)))
;------------------------------------------------------------------------------
(defrule build-groups::build-glapi-function
			?msg <- (message (to build-groups)
								  (action add-to-span)
								  (arguments ?id))
			?obj <- (object (is-a file-line) 
								 (id ?id)
								 (type GLAPI-DEF)
								 (contents GLAPI ?ret GLAPIENTRY ?name "(" $?args ")"))
			=>
			;we need to set this up to do conversion of the different arguments
			(bind ?objName (gensym*))
			(modify ?msg (to grouping-update)
					  (action parse-arguments)
					  (arguments ?objName => $?args))
			(make-instance ?objName of GLAPIFunction 
								(return-type ?ret)
								(function-name ?name)
								(clips-function-name (sym-cat CLIPS_ ?name))))
;------------------------------------------------------------------------------
(defrule build-groups::build-glapi-function-void
			(declare (salience 1))
			?msg <- (message (to build-groups)
								  (action add-to-span)
								  (arguments ?id))
			?obj <- (object (is-a file-line) 
								 (id ?id)
								 (type GLAPI-DEF)
								 (contents GLAPI ?ret GLAPIENTRY ?name "(" void ")"))
			=>
			;we need to set this up to do conversion of the different arguments
			(bind ?objName (gensym*))
			(retract ?msg)
			(make-instance ?objName of GLAPIFunction 
								(return-type ?ret)
								(function-name ?name)
								(clips-function-name (sym-cat CLIPS_ ?name))))
;------------------------------------------------------------------------------
(defrule grouping-update::parse-fixed-size-array-argument
			(declare (salience 2))
			?fct <- (message (to grouping-update)
								  (action parse-arguments)
								  (arguments ?o => $?sentry "[" ?size "]" ,|"," $?rest))
			?obj <- (object (is-a GLAPIFunction)
								 (id ?o))
			=>
			(modify ?fct (arguments ?o => $?rest))
			(bind ?name (gensym*))
			(duplicate ?fct (action populate-argument)
						  (arguments ?name => $?sentry))
			(make-instance ?name of GLAPIFixedArrayArgument
								(parent ?o)
								(index (send ?obj add-argument ?name))
								(array-size ?size)))
;------------------------------------------------------------------------------
(defrule grouping-update::parse-arguments-generic
			(declare (salience 1))
			?fct <- (message (to grouping-update)
								  (action parse-arguments)
								  (arguments ?o => $?sentry ,|"," $?rest))
			?obj <- (object (is-a GLAPIFunction)
								 (id ?o))
			=>
			(modify ?fct (arguments ?o => $?rest))
			(bind ?name (gensym*))
			(duplicate ?fct (action populate-argument)
						  (arguments ?name => $?sentry))
			(make-instance ?name of GLAPIArgument
								(parent ?o)
								(index (send ?obj add-argument ?name))))
;------------------------------------------------------------------------------
(defrule grouping-update::create-last-argument
			?fct <- (message (to grouping-update)
								  (action parse-arguments)
								  (arguments ?o => $?all))
			(test (and (not (member$ , $?all))
						  (not (member$ "," $?all))))
			?obj <- (object (is-a GLAPIFunction)
								 (id ?o))
			=>
			(bind ?name (gensym*))
			(modify ?fct (action populate-argument)
					  (arguments ?name => $?all))
			(make-instance ?name of GLAPIArgument
								(parent ?o)
								(index (send ?obj add-argument ?name))))
;------------------------------------------------------------------------------
(defrule grouping-update::retract-empty-parse-message
			?fct <- (message (to grouping-update)
								  (action parse-arguments)
								  (arguments ? =>))
			=>
			(retract ?fct))
;------------------------------------------------------------------------------
(defrule grouping-update::mark-argument-is-pointer
			(declare (salience 1))
			?fct <- (message (to grouping-update)
								  (action populate-argument)
								  (arguments ?o => $?before "*" $?after))
			?obj <- (object (is-a GLAPIArgument) 
								 (id ?o))
			=>
			(modify ?fct (arguments ?o => $?before $?after))
			(modify-instance ?obj (is-pointer TRUE)))
;------------------------------------------------------------------------------
(defrule grouping-update::mark-argument-is-constant
			(declare (salience 1))
			?fct <- (message (to grouping-update)
								  (action populate-argument)
								  (arguments ?o => $?before const|"const" $?after))
			?obj <- (object (is-a GLAPIArgument)
								 (id ?o))
			=>
			(modify ?fct (arguments ?o => $?before $?after))
			(modify-instance ?obj (is-constant TRUE)))
;------------------------------------------------------------------------------
(defrule grouping-update::set-argument-core-info
			?fct <- (message (to grouping-update)
								  (action populate-argument)
								  (arguments ?o => ?type ?name))
			?obj <- (object (is-a GLAPIArgument)
								 (id ?o))
			=>
			(retract ?fct)
			(modify-instance ?obj (argument-type ?type)
								  (argument-name ?name)))
;------------------------------------------------------------------------------
(defrule grouping-update::retract-arguments
			?fct <- (message (to grouping-update)
								  (action populate-argument)
								  (arguments ? =>))
			=>
			(retract ?fct))
;------------------------------------------------------------------------------
(defrule grouping-update::printout-arguments
			(declare (salience -10))
			?obj <- (object (is-a GLAPIArgument))
			=>
			(printout t (send ?obj reconstitute) crlf))
;------------------------------------------------------------------------------
