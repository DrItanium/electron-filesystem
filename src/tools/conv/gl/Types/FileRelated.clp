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
; FileRelated.clp - Defines file related objects and templates
; Written by Joshua Scoggins
;------------------------------------------------------------------------------
(defclass types::opened-file 
  (is-a Object)
  (slot file-id)
  (slot index (type INTEGER) (range 0 ?VARIABLE))
  (message-handler next-index)
  (message-handler read-line)
  (message-handler close-file))
;------------------------------------------------------------------------------
(defmessage-handler types::opened-file next-index ()
                    (bind ?old ?self:index)
                    (bind ?self:index (+ ?old 1))
                    (return ?old))
;------------------------------------------------------------------------------
(defmessage-handler types::opened-file read-line ()
                    (readline ?self:file-id))
;------------------------------------------------------------------------------
(defmessage-handler types::opened-file close-file ()
                    (close ?self:file-id))
;------------------------------------------------------------------------------
(defclass types::file-line
  (is-a Object)
  (slot index)
  (slot type)
  (multislot contents))
;------------------------------------------------------------------------------
(defclass types::heading-span
  "Defines a span between two different headings"
  (is-a Object)
  (slot header-name)
  (slot from)
  (slot to)
  (multislot contents))
;------------------------------------------------------------------------------
(deftemplate types::message 
             (slot from)
             (slot to)
             (slot action)
             (multislot arguments))
;------------------------------------------------------------------------------
