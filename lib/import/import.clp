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
;    * Neither the name of electron-filesystem nor the
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
; import.clp - Defines the import method, a wrapper over load, load*, batch,
; and batch* that makes it possible to do relative includes without having to
; worry about absolute paths
;------------------------------------------------------------------------------
; Assume that etc/sys.clp has already been loaded
(defglobal MAIN
           ?*valid-import-actions* = (create$ load load* batch batch*))
(defgeneric import)

(defmethod import 
  ((?action SYMBOL (neq ?action (expand$ ?*valid-import-actions*)))
   (?local-path LEXEME))
  (printout t 
            (format nil "Provided action %s is not valid for import" ?action) crlf
            "Valid actions are: " ?*valid-import-actions* crlf)
  (return FALSE))

(defmethod import 
  ((?action SYMBOL (not (neq ?action (expand$ ?*valid-import-actions*))))
   (?local-path LEXEME))
  (progn$ (?front ?*path*)
          (if (funcall ?action 
                       (format nil "%s/%s" ?front ?local-path)) then
            (return TRUE)))
  (return FALSE))
