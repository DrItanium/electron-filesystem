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
; GLAPIConversionTypes.clp - Functions for the glapi conversion expert
; system
;------------------------------------------------------------------------------
(defclass types::GLAPIFunction
  "Defines a given GLAPI function"
  (is-a Object)
  (slot return-type (type SYMBOL STRING))
  (slot function-name)
  (slot clips-function-name))
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
						  (return (format nil "%s[%s]" 
												(call-next-handler)
												(str-cat ?self:array-size))))
;------------------------------------------------------------------------------
(defclass types::FunctionBuilder
  "Builds C functions"
  (is-a Object)
  (multislot contents))
;------------------------------------------------------------------------------
(defclass types::CLIPSFunctionBuilder
  (is-a FunctionBuilder)
  (multislot data-objects)
  (multislot variables)
  (multislot parsing-entries)
  (slot count))
;------------------------------------------------------------------------------
(defclass types::CLIPSGLAPIArgumentBuilder
  (is-a Object)
  (slot clips-type (type SYMBOL))
  (slot gl-type (type SYMBOL))
  (slot clips-function-name (type SYMBOL))
  (slot index (type INTEGER) (range 1 ?VARIABLE))
  (slot argument-name-base (type SYMBOL STRING))
  (slot data-object-argument-name (type SYMBOL STRING))
  (slot variable-argument-name (type SYMBOL STRING))
  (slot variable-declaration (type STRING))
  (slot data-object-declaration (type STRING))
  (message-handler get-type-check-code)
  (message-handler get-conversion-code))
;------------------------------------------------------------------------------
(defmessage-handler types::CLIPSGLAPIArgumentBuilder get-type-check-code ()
						  (return (format nil 
												"if(EnvArgTypeCheck(theEnv,%s,%d,%s,&%s) == -1) { return; }"
												?self:clips-function-name
												?self:index
												?self:clips-type
												?self:data-object-argument-name)))
;------------------------------------------------------------------------------
(defmessage-handler types::CLIPSGLAPIArgumentBuilder get-conversion-code ()
						  (return (format nil "%s = %s(%s);"
												?self:variable-argument-name
												(get-conversion-function 
												  (flatten-type ?self:clips-type))
												?self:data-object-argument-name)))
;------------------------------------------------------------------------------
(defclass types::CLIPSGLAPIMultifieldArgumentBuilder
  (is-a CLIPSGLAPIArgumentBuilder)
  (slot multifield-length-variable-declaration)
  (slot multifield-length-variable-name)
  (slot multifield-pointer-argument-name)
  (slot multifield-pointer-declaration)
  (message-handler get-type-check-code)
  (message-handler get-conversion-code))
;------------------------------------------------------------------------------
(defmessage-handler types::CLIPSGLAPIMultifieldArgumentBuilder
						  get-type-check-code primary ()
						  (return (format nil 
												"if(EnvArgTypeCheck(theEnv,%s,%d,MULTIFIELD,&%s) == -1) { return; }"
												?self:clips-function-name
												?self:index
												?self:data-object-argument-name)))
;------------------------------------------------------------------------------
(defmessage-handler types::CLIPSGLAPIMultifieldArgumentBuilder
						  get-conversion-code ()
						  (return (format nil "%s%n%s%n%s%n%s%n"
												;GetLength
												(format nil "%s = GetDOLength(%s);"
														  ?self:multifield-length-variable-name
														  ?self:data-object-argument-name)
												;GetValue
												(format nil "%s = GetValue(%s);"
														  ?self:multifield-pointer-argument-name
														  ?self:data-object-argument-name)
												;Generate malloc
												(format nil "%s = (%s*)calloc(%s,sizeof(%s));"
														  ?self:variable-argument-name
														  ?self:gl-type
														  ?self:multifield-length-variable-name
														  ?self:gl-type)
												;Conversion
												(format nil "%s%n%s%n%s%n" 
														  (format nil "int i, end;%nend = GetDOEnd(%s);"
																	 ?self:data-object-argument-name)
														  (format nil "for(i = GetDOBegin(%s); i<=end; i++) {", 
																	 ?self:data-object-argument-name)
														  ;TODO: Continue
														  ))))
;------------------------------------------------------------------------------
(defclass types::CLIPSGLAPIFixedSizeMultifieldArgumentBuilder
  (is-a CLIPSGLAPIMultifieldArgumentBuilder)
  (slot multifield-size))
;------------------------------------------------------------------------------
