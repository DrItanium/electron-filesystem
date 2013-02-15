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
; GLAPIConversionFunctions.clp - Functions for the glapi conversion expert
; system
;------------------------------------------------------------------------------
(deffunction build-groups::count-commas
				 "Iterates through a multifield and counts the number of commas"
				 (?list)
				 (bind ?count 0)
				 (progn$ (?e ?list)
							(if (or (eq ?e ",")
									  (eq ?e ,)) then
							  (bind ?count (+ 1 ?count))))
				 (return ?count))
;------------------------------------------------------------------------------
(deffunction types::flatten-type (?symbol)
				 (switch ?symbol
							(case Float then FLOAT)
							(case float then FLOAT)
							(case FLOAT then FLOAT)
							(case double then DOUBLE)
							(case Double then DOUBLE)
							(case DOUBLE then DOUBLE)
							(case integer then INTEGER)
							(case Integer then INTEGER)
							(case INTEGER then INTEGER)
							(case LONG then LONG)
							(case long then LONG)
							(case Long then LONG)
							(case symbol then SYMBOL)
							(case Symbol then SYMBOL)
							(case SYMBOL then SYMBOL)
							(case string then STRING)
							(case String then STRING)
							(case STRING then STRING)
							(default ?symbol)))
;------------------------------------------------------------------------------
(deffunction types::get-conversion-function (?symbol)
				 (bind ?fn-float "DOToFloat")
				 (bind ?fn-double "DOToDouble")
				 (bind ?fn-int "DOToInteger")
				 (bind ?fn-long "DOToLong")
				 (bind ?fn-string "DOToString")
				 (switch ?symbol
							(case FLOAT then ?fn-float)
							(case DOUBLE then ?fn-double)
							(case INTEGER then ?fn-int)
							(case LONG then ?fn-long)
							(case SYMBOL then ?fn-string)
							(case STRING then ?fn-string)
							(default "ERROR_NO_CONVERSION_FOUND")))
;------------------------------------------------------------------------------
