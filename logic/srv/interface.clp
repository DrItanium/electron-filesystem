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
; interface.clp - Simple expert system that is controlled through a file 
;                (named pipe).
; 
;------------------------------------------------------------------------------
(if (or (operating-system-is-windows32)
        (operating-system-is-windows64)) then
  (printout werror "Interface server not supported on native windows!" crlf)
  (exit -1))

(load* (fs /sys/core/message.clp))
(load* (fs /sys/core/strings.clp))
(load* (fs /conf/srv/interface.clp))
(deftemplate stage (slot current-stage)
             (multislot interfaces))

(deffacts execution-stages
          (stage (current-stage setup)))

(defrule next-stage:setup
         (declare (salience -10000))
         ?f <- (stage (current-stage setup))
         =>
         (modify ?f (current-stage get)))

(defrule next-stage:get
         (declare (salience -10000))
         ?f <- (stage (current-stage get))
         =>
         (modify ?f (current-stage parse)))

(defrule next-stage:parse
         (declare (salience -10000))
         ?f <- (stage (current-stage parse))
         =>
         (modify ?f (current-stage execute)))

(defrule next-stage:execute
         (declare (salience -10000))
         ?f <- (stage (current-stage execute))
         =>
         (modify ?f (current-stage get)))


(defrule initialize-interface
         (stage (current-stage setup))
         ?f <- (message (action declare-target-interface)
                        (contents ?ifname ?path))
         =>
         (if (open ?path ?ifname "r+") then
           (modify ?f (action interface)
                   (contents ?ifname))
           (assert (has-interface))
           (printout werror "INFORMATION: Opened interface " ?path crlf)
           else
           (printout werror "ERROR: couldn't open " ?path crlf)
           (halt)))

(defrule merge-interfaces
         ?f <- (stage (current-stage setup)
                      (interfaces $?ifs))
         ?f2 <- (message (action interface)
                         (contents ?if))
         =>
         (modify ?f (interfaces $?ifs ?if))
         (retract ?f2))

(defrule fail-setup
         (declare (salience -1000))
         (stage (current-stage setup))
         (not (exists (has-interface)))
         =>
         (printout werror "ERROR: interface server needs an interface to bind to!" crlf)
         (halt))

(defrule get-input-from-interface
         (stage (current-stage get)
                (interfaces $? ?interface $?))
         =>
         (bind ?line (readline ?interface))
         (format werror "INFORMATION: INTERFACE: %s CONTENTS: %s%n" ?interface ?line)
         (assert (message (action parse-raw-input)
                          (interface ?interface)
                          (contents ?line))))

(defrule decompose-message 
         (stage (current-stage parse))
         ?f <- (message (action parse-raw-input)
                        (contents ?contents))
         =>
         (modify ?f (action check-input)
                 (contents (explode$ ?contents))))

(defrule valid-message
         (stage (current-stage parse))
         ?f <- (message (action check-input)
                        (contents From: ?from To: ?to Action: ?action Contents: $?contents))
         =>
         (modify ?f (from ?from)
                 (to ?to)
                 (action ?action)
                 (contents ?contents)))


(defrule invalid-message 
         (declare (salience -1))
         (stage (current-stage parse))
         ?f <- (message (action check-input)
                        (interface ?if)
                        (contents $?contents))
         =>
         (retract ?f)
         (format werror "ERROR: INTERFACE: %s DROPPING INVALID MESSAGE: %s%n" ?if (implode$ $?contents)))

(defrule shutdown-server
         (declare (salience 1))
         ?q <- (stage (current-stage execute))
         ?f <- (message (action shutdown-server))
         =>
         (retract ?f)
         (modify ?q (current-stage deactivate-interfaces))
         (printout werror "INFORMATION: Shutting down server!" crlf))


(defrule handle-message-default
         (stage (current-stage execute))
         ?f <- (message (from ?from)
                        (to ?to)
                        (action ?action)
                        (contents $?contents))
         =>
         (retract ?f)
         (printout werror 
                   "WARNING: UNHANDLED MESSAGE! (FROM: " ?from 
                   " TO: " ?to 
                   " ACTION: " ?action 
                   " CONTENTS: " $?contents crlf))

(defrule deactivate-interfaces
         (stage (current-stage deactivate-interfaces))
         ?iface <- (message (action interface)
                            (contents ?interface))
         =>
         (retract ?iface)
         (close ?interface)
         (printout werror "INFORMATION: Shutdown interface " ?interface crlf))

(defrule done-shutting-down-server
         (declare (salience -1))
         ?f <- (stage (current-stage deactivate-interfaces))
         =>
         (retract ?f)
         (printout werror "Shutdown complete!" crlf "Goodbye!" crlf))
