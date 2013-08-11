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
; permissions.clp - provides functions to compute unix file permissions
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Make umask, mkdir, and mkfifo a little less raw to work with
;------------------------------------------------------------------------------
(defgeneric compute-perms)
;------------------------------------------------------------------------------
(defmethod compute-perms
  ((?self INTEGER (and (>= ?self 0)
                       (< ?self 8)))
   (?group INTEGER (and (>= ?group 0)
                        (< ?group 8)))
   (?all INTEGER (and (>= ?all 0)
                      (< ?all 8))))
  (binary-or (left-shift ?self 6) 
             (binary-or (left-shift ?group 3)
                        ?all)))
(defmethod compute-perms 
  ((?self SYMBOL)
   (?group SYMBOL)
   (?all SYMBOL))
  (compute-perms (compute-perms ?self)
                 (compute-perms ?group)
                 (compute-perms ?all)))

(defmethod compute-perms
  ((?set SYMBOL (eq ?set r))) 4)
(defmethod compute-perms
  ((?set SYMBOL (eq ?set w))) 2)
(defmethod compute-perms
  ((?set SYMBOL (eq ?set x))) 1)
(defmethod compute-perms
  ((?set SYMBOL (eq ?set rwx))) 7)
(defmethod compute-perms
  ((?set SYMBOL (eq ?set rw))) 6)
(defmethod compute-perms
  ((?set SYMBOL (eq ?set rx))) 5)
(defmethod compute-perms
  ((?set SYMBOL (eq ?set wx))) 3)
