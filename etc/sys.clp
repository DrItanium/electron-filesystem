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
; sys.clp - Defines system variables that the rest of the filesystem uses. This
;           is called by the system bootstrap after finding out where it is
;------------------------------------------------------------------------------
; Define filesystem base points 
(defglobal MAIN
           ; lib directory
           ?*/lib* = (fs /lib)
           ; data directory
           ?*/data* = (fs /data)
           ; bin directory
           ?*/bin* = (fs /bin)
           ; etc directory
           ?*/etc* = (fs /etc)
           ; logic directory
           ?*/logic* = (fs /logic)
           ; path variable
           ?*path* = (create$ ?*/lib*))
;------------------------------------------------------------------------------
; Define the different element base points
; Makes loading code far more elegant :D
;------------------------------------------------------------------------------
(defgeneric /etc)
(defgeneric /data)
(defgeneric /lib)
(defgeneric /logic)
(defgeneric /data/input)
(defgeneric /data/output)
;------------------------------------------------------------------------------
(defmethod /etc 
  () 
  ?*/etc*)

(defmethod /etc 
  ((?atoms MULTIFIELD))
  (fs /etc ?atoms))

(defmethod /etc
  ($?atoms)
  (/etc ?atoms))
;------------------------------------------------------------------------------
(defmethod /data 
  () 
  ?*/data*)

(defmethod /data 
  ((?atoms MULTIFIELD))
  (fs /data ?atoms))

(defmethod /data
  ($?atoms)
  (/data ?atoms))
;------------------------------------------------------------------------------
(defmethod /lib 
  () 
  ?*/lib*)

(defmethod /lib 
  ((?atoms MULTIFIELD))
  (fs /lib ?atoms))

(defmethod /lib
  ($?atoms)
  (/lib ?atoms))
;------------------------------------------------------------------------------
(defmethod /logic 
  () 
  ?*/logic*)

(defmethod /logic 
  ((?atoms MULTIFIELD))
  (fs /logic ?atoms))

(defmethod /logic
  ($?atoms)
  (/logic ?atoms))
;------------------------------------------------------------------------------
(defmethod /data/input 
  ((?atoms MULTIFIELD))
  (fs /data/input ?atoms))

(defmethod /data/input
  ($?atoms)
  (/data/input ?atoms))
;------------------------------------------------------------------------------
(defmethod /data/output 
  ((?atoms MULTIFIELD))
  (fs /data/output ?atoms))

(defmethod /data/output
  ($?atoms)
  (/data/output ?atoms))
;------------------------------------------------------------------------------
