;------------------------------------------------------------------------------
;theoretical-architecture
;Copyright (c) 2012-2013, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of theoretical-architecture nor the
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
;-------------------------------------------------------------------------------
; MachineCommone.clp - Defines the common pieces of the theoretical processor.
;                      This makes it easier to define a tool chain 
;-------------------------------------------------------------------------------
(deftemplate instruction
             (slot tag
                   (type SYMBOL))
             (slot arg-count
                   (type INTEGER))
             (slot machine-tag
                   (type INTEGER))
             (slot is-macro ; is it a convienience instruction/operation?
                   (type SYMBOL)
                   (allowed-symbols FALSE TRUE))
             (slot funcall
                   (type LEXEME)
                   (default-dynamic !!)))
;-------------------------------------------------------------------------------
(defgeneric register-name-to-index)
;-------------------------------------------------------------------------------
(defmethod register-name-to-index
  ((?register-name SYMBOL))
  (string-to-field (sub-string 2 (str-length ?register-name) ?register-name)))
;-------------------------------------------------------------------------------
