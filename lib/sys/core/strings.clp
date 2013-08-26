;------------------------------------------------------------------------------
;electron-filesystem
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
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
;ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; strings.clp - Defines useful string operations to save time
;------------------------------------------------------------------------------
(defgeneric starts-with)
(defgeneric contains)
(defgeneric char-at)
(defgeneric to-lexeme)
;------------------------------------------------------------------------------
(defmethod starts-with
  "Checks to see if a given lexeme starts with another lexeme"
  ((?looking-for LEXEME)
   (?in LEXEME))
  (eq 1 (str-index ?looking-for ?in)))

(defmethod starts-with
  (?looking-for ?in)
  (starts-with (to-lexeme ?looking-for)
               (to-lexeme ?in)))
;------------------------------------------------------------------------------
(defmethod contains
  ((?looking-for LEXEME)
   (?in LEXEME))
  (neq (str-index ?looking-for ?in) FALSE))

(defmethod contains
  (?looking-for ?in)
  (contains (to-lexeme ?looking-for)
            (to-lexeme ?in)))

;------------------------------------------------------------------------------
(defmethod char-at 
 ((?index INTEGER)
  (?string LEXEME))
 (sub-string ?index ?index ?string))

(defmethod char-at
 ((?index FLOAT)
  (?string LEXEME))
 (char-at (integer ?index) ?string))
;------------------------------------------------------------------------------
(defmethod to-lexeme
 ((?value LEXEME))
 ?value)

(defmethod to-lexeme
 ((?value (not (lexemep ?value))))
 (str-cat ?value))
;------------------------------------------------------------------------------
