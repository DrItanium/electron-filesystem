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
; bootstrap.clp - Bootstraps the file system and loads sys.clp. It is up to a
; shell script to call electron with the customized version of this file.
;
; This file has to be batched
;------------------------------------------------------------------------------
(defglobal MAIN
           ; Change the value of this global to change the name of the
           ; corresponding shell variable.
           ?*electron-fs-root* = TheoreticalFSRoot 
           ; Use this to make sure that we fail out if we can't bootstrap
           ?*fsys* = (progn (bind ?result (get-shell-variable ?*electron-fs-root*))
                            (if (not ?result) then
                              (printout t "ERROR: " ?*electron-fs-root* " not defined - Exiting" crlf)
                              (exit)
                              else
                              ?result)))
; Can't do loads within defglobal calls because it could cause crashes if
; we do a defglobal within a defglobal.

; Load the filesystem base points
(load* (format nil "%s/etc/sys.clp" ?*fsys*))
; Load the system base (import commands)
(load* (format nil "%s/import/import.clp" ?*lib*))
; Load the simulator
(import batch* "theoretical-architecture/Simulator.clp")

(reset)
(run)
(exit)
