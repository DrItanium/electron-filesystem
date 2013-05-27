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
; DeclareLibraryFunctions.clp - Defines a series of functions used to
; streamline the loading of libraries in the adventure engine
; 
; Written by Joshua Scoggins 
; Started on 3/11/2013
;------------------------------------------------------------------------------
(defgeneric init::load-library "Loads a given library header")
;------------------------------------------------------------------------------
(defgeneric init::library-files "Loads the contents of a target library")
;------------------------------------------------------------------------------
(defmethod init::load-library 
  "Loads an adventure engine library from the root of the src folder"
  ((?name LEXEME))
  (generic-load lib ?name Library.clp))
;------------------------------------------------------------------------------
(defmethod init::library-files 
  "Loads the files that make up the target library"
  ((?offset LEXEME)
   ($?names LEXEME))
  (generic-load lib ?offset ?names))
;------------------------------------------------------------------------------
(defmethod init::library-files
  "Loads the files that make up the target library"
  ((?offset LEXEME)
   (?names LEXEME MULTIFIELD))
  (generic-load lib ?offset ?names))
;------------------------------------------------------------------------------
