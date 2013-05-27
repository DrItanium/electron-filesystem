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
; GenericDeclareFunctions.clp - Contains generic backing methods for loading
; code
; 
; Written by Joshua Scoggins 
; Started on 3/13/2013
;------------------------------------------------------------------------------
(defgeneric Runtime::generic-load
 "Loads a single file in an abstract way")
;------------------------------------------------------------------------------
(defmethod Runtime::generic-load
 ((?path LEXEME))
 (batch* ?path))
;------------------------------------------------------------------------------
(defmethod Runtime::generic-load
 "Loads a single file in an abstract way"
 ((?folder-name LEXEME)
  (?path-to-folder LEXEME)
  (?file-name LEXEME))
 (generic-load (format nil "%s/%s/%s" ?folder-name ?path-to-folder ?file-name)))
;------------------------------------------------------------------------------
(defmethod Runtime::generic-load
 ((?folder-name LEXEME)
  (?path-to-folder LEXEME)
  (?files MULTIFIELD LEXEME (= (length$ ?files) 1)))
 (generic-load ?folder-name ?path-to-folder (nth$ 1 ?files)))
;------------------------------------------------------------------------------
(defmethod Runtime::generic-load
 ((?folder-name LEXEME)
  (?path-to-folder LEXEME)
  (?files MULTIFIELD LEXEME (> (length$ ?files) 1)))
 (bind ?core-offset (format nil "%s/%s/%s" 
                     ?folder-name ?path-to-folder "%s"))
 (progn$ (?a ?files)
  (generic-load (format nil ?core-offset ?a))))
;------------------------------------------------------------------------------
(defmethod Runtime::generic-load
 ((?folder-name LEXEME)
  (?path-to-folder LEXEME)
  ($?files LEXEME (> (length$ ?files) 1)))
 (generic-load ?folder-name ?path-to-folder $?files))
;------------------------------------------------------------------------------
(defmethod Runtime::generic-load
 ((?folder-name LEXEME)
  (?path-to-folder LEXEME)
  ($?files LEXEME (= (length$ ?files) 1)))
 (generic-load ?folder-name ?path-to-folder (nth$ 1 ?files)))
;------------------------------------------------------------------------------
