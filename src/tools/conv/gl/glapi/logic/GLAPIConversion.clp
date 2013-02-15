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
; build-groups module
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
			(modify ?msg 
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
			(retract ?msg)
			(make-instance of GLAPIFunction 
								(return-type ?ret)
								(function-name ?name)
								(clips-function-name (sym-cat CLIPS_ ?name))))
;------------------------------------------------------------------------------
(defrule build-groups::concatentate-length
			"Adds a range to the target argument set"
			?fct <- (message (to build-groups)
								  (action parse-arguments)
								  (arguments ?o => $?contents))
			=>
			(modify ?fct (action diverge-arguments)
					  (arguments ?o 0 (count-commas $?contents) =>
									 $?contents)))
;------------------------------------------------------------------------------
(defrule build-groups::diverge-arguments
			"Splits the set of arguments into two separate facts"
			(declare (salience 1))
			?fct <- (message (to build-groups)
								  (action diverge-arguments)
								  (arguments ?o ?start ?finish => $?a "," $?b))
			(test (neq ?start ?finish))
			=>
			(bind ?cca (count-commas $?a))
			(bind ?starta ?start)
			(bind ?enda (+ ?starta ?cca))
			;from start to end
			(bind ?startb (+ 1 ?enda))
			;we use finish since it's the upper bound on this argument set
			(bind ?endb ?finish)
			(modify ?fct (arguments ?o ?starta ?enda => $?a))
			(duplicate ?fct (arguments ?o ?startb ?endb => $?b)))
;------------------------------------------------------------------------------
(defrule build-groups::generate-build-argument-statement
			?fct <- (message (to build-groups)
								  (action diverge-arguments)
								  (arguments ?o ?pos ?pos => $?conditions))
			=>
			(modify ?fct (action build-argument)
					  (arguments ?o ?pos => $?conditions)))
;------------------------------------------------------------------------------
(defrule build-groups::argument-is-fixed-array
			(declare (salience 1))
			?fct <- (message (to build-groups)
								  (action build-argument)
								  (arguments ?o ?ind => $?field "[" ?size "]"))
			=>
			(bind ?name (gensym*))
			(modify ?fct (to grouping-update)
					  (action populate-argument)
					  (arguments ?name => $?field))
			(make-instance ?name of GLAPIFixedArrayArgument
								(parent ?o)
								(array-size ?size)
								(index ?ind)))
;------------------------------------------------------------------------------
(defrule build-groups::argument-is-generic
			?fct <- (message (to build-groups)
								  (action build-argument)
								  (arguments ?o ?ind => $?input))
			=>
			(bind ?name (gensym*))
			(modify ?fct (to grouping-update)
					  (action populate-argument)
					  (arguments ?name => $?input))
			(make-instance ?name of GLAPIArgument
								(parent ?o)
								(index ?ind)))
;------------------------------------------------------------------------------
; grouping-update module
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
			(modify ?fct (arguments ?o =>))

			(modify-instance ?obj (argument-type ?type)
								  (argument-name ?name)))
;------------------------------------------------------------------------------
(defrule grouping-update::retract-arguments
			?fct <- (message (to grouping-update)
								  (action populate-argument)
								  (arguments ?o =>))
			(object (is-a GLAPIArgument)
					  (id ?o)
					  (parent ?p))
			=>
			(modify ?fct (action make-function-builder)
					  (arguments ?p)))
;------------------------------------------------------------------------------
; So, what do we need to do to construct a single function
; 0) Generate the registration entry for the target builder
; 1) Generate the function builder (done)
; 2) Define required arguments 
; 3) Figure out type conversions
; 4) Define translation code for each argument
; 5) Generate code to call the target function with the given arguments
; 6) Assemble the function together
;------------------------------------------------------------------------------
; We use EnvDefineFunction in all cases of registration
;
; For conversion of pointers, we take in a multifield from clips and convert
; that to a corresponding pointer. We can also use macros or a function to
; do the conversion, that would make this whole process a lot easier. 
;
; While it goes against the standard practices of C, I am going to have this
; conversion function perform the malloc and return the pointer. It is up to
; the function that called the conversion function to clean up the pointer once
; finished.
;
; However, continually calling malloc and free is costly. We can allocate a
; pointer ahead of time and continually resize it as necessary. We could also
; define a huge block of memory and then slice it accordingly. 
;
; I know this for certain, this technique will go through many iterations
; before I arrive at a valid solution (most likely). 
;
; The functions that take in a fixed size array are easy because I can allocate
; a static block ahead of time that I can read and write from. 
;------------------------------------------------------------------------------
; Conversion table 
;------------------------------------------------------------------------------
; float => all floating point types 
; int => all integer types
; symbol => GLenum or GLboolean
; multifield => pointer type or fixed array
;------------------------------------------------------------------------------
(defrule grouping-update::make-function-builder
			?fct <- (message (to grouping-update)
								  (action make-function-builder)
								  (arguments ?p))
			=>
			(modify ?fct (action build-function)
					  (arguments ?p -1))
			(make-instance of CLIPSFunctionBuilder 
								(parent ?p)))
;------------------------------------------------------------------------------
(defrule grouping-update::build-function-header
			(declare (salience 1))
			?fct <- (message (to grouping-update)
								  (action build-function)
								  (arguments ?p -1))
			?obj <- (object (is-a CLIPSFunctionBuilder)
								 (parent ?p))
			(object (is-a GLAPIFunction) 
					  (id ?p)
					  (return-type ?ret)
					  (clips-function-name ?cfn))
			=>
			(modify ?fct (arguments ?p 0))
			(modify-instance ?obj (contents 
											(format nil "extern %s %s(void* theEnv) {" 
													  ?ret ?cfn))))
;------------------------------------------------------------------------------
(defrule grouping-update::build-function-add-argument
			?fct <- (message (to grouping-update)
								  (action build-function)
								  (arguments ?p ?index))
			?arg <- (object (is-a GLAPIArgument)
								 (parent ?p)
								 (index ?index)
								 (id ?argID))
			=>
			(modify ?fct (arguments ?p (+ ?index 1)))
			(duplicate ?fct (action parse-entry)
						  (arguments ?p ?index => ?argID)))
;------------------------------------------------------------------------------
(defrule grouping-update::close-function
			(declare (salience -1))
			?fct <- (message (to grouping-update)
								  (action build-function)
								  (arguments ?p ?index))
			(not (exists (object (is-a GLAPIArgument) 
										(parent ?p) 
										(index ?index))))
			?f <- (object (is-a CLIPSFunctionBuilder) 
							  (parent ?p))
			=>
			(retract ?fct)
			(modify ?fct (action construct-function)
					  (arguments ?p -1))
			(modify-instance ?f (count ?index)))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-function-entry
			?f <- (message (to grouping-update)
								(action construct-function)
								(arguments ?p -1))

			=>
			(modify ?f (argument ?p 0)))
;------------------------------------------------------------------------------
(defrule grouping-update::construct-function-argument-deferred
			?f <- (message (to grouping-update)
								(action construct-function)
								(arguments ?p ?index))
			?f1 <- (message (to grouping-update)
								 (action parse-entry)
								 (arguments ?p ?index => ?argID))
			?b <- (object (is-a CLIPSFunctionBuilder)
							  (parent ?p)
							  (data-objects $?do)
							  (variables $?vars)
							  (parsing-entries $?pe))
			=>
			;generate the name of the object for the target argument entry 
			(bind ?name (gensym*))
			(modify ?f (arguments ?p (+ ?index 1)))
			;put name on each of the element sets.
			;We use that name and message passing to generate corresponding
			;entries in a macro-expand fashion
			(modify-instance ?b (data-objects $?do ?name)
								  (variables $?vars ?name)
								  (parsng-entries $?pe ?name))
			(modify ?f1 (action construct-entry)
					  (arguments ?argID => ?name)))
;------------------------------------------------------------------------------
; Printout results
;------------------------------------------------------------------------------
(defrule grouping-update::print-built-function
			?fct <- (message (to grouping-update)
								  (action print-function)
								  (arguments ?p))
			?f <- (object (is-a FunctionBuilder)
							  (parent ?p)
							  (contents $?c))
			=>
			(progn$ (?line ?c)
					  (printout t ?line crlf))
			(printout t crlf crlf)
			(retract ?fct)
			(unmake-instance ?f))
;------------------------------------------------------------------------------
