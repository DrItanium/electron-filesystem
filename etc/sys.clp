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
; sys.clp - Defines system variables that the rest of the filesystem uses
; By default, the electron-fs looks for the following shell variables 
;             ElectronFSRoot - location of the root of the electron-filesystem
; 
; These variables are case sensitive!
;------------------------------------------------------------------------------
(defglobal MAIN
 ; root of the electron file system
 ?*fsys* = (progn (bind ?loc (get-shell-variable ElectronFSRoot))
                       (if ?loc then
                        ?loc
                        else
                        (printout werror "ERROR: ElectronFSRoot is not defined!" crlf)
                        (exit)))
 ; lib directory
 ?*lib* = (format nil "%s/lib" ?*fsys*)
 ; data directory
 ?*data* = (format nil "%s/data" ?*fsys*)
 ; bin directory
 ?*bin* = (format nil "%s/bin" ?*fsys*)
 ; etc directory
 ?*etc* = (format nil "%s/etc" ?*fsys*)
 ?*path* = (create$ ?*lib*))
