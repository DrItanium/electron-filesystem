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
; shell.clp - starts up the electron shell with all libraries loaded
;
; This file has to be batched
;------------------------------------------------------------------------------
(defglobal MAIN
           ; Change the value of this global to change the name of the
           ; corresponding shell variable.
           ?*electron-fs-root* = ElectronFSRoot
           ; Use this to make sure that we fail out if we can't bootstrap
           ?*fs* = (progn (bind ?result (get-shell-variable ?*electron-fs-root*))
                           (if (not ?result) then
                             (printout t "ERROR: " ?*electron-fs-root* " not defined - Exiting" crlf)
                             (exit)
                             else
                             ?result)))
;------------------------------------------------------------------------------
; Now that we have a base point we can really define some elegant fs points
; Use fs as a builder of paths. I believe it's quite elegant :D
;------------------------------------------------------------------------------
(defgeneric fs)
;------------------------------------------------------------------------------
(defmethod fs () ?*fs*)
(defmethod fs 
  "Builds a path around the electron file system from the root"
  ((?atoms MULTIFIELD))
  (str-cat ?*fs* (expand$ ?atoms)))
(defmethod fs 
  ($?atoms) 
  (fs ?atoms))
;------------------------------------------------------------------------------
; load the base system definitions
(load* (fs /etc/sys.clp))
; define the import statements
(load* (/lib /import/import.clp))
(import load* util/message.clp)
(import load* util/strings.clp)
