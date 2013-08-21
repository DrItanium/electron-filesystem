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
; Simulator.clp - Defines a simple computer to be executed through an expert
;                 system. This computer is unique in that instructions declare
;                 actions instead of perform actions directly.
;
;                 The Theoretical Architecture is an 8-bit declarative
;                 processor architecture. It has an 8-bit direct address space
;                 that can be extended through the use of jump instructions.
;-------------------------------------------------------------------------------
(load* (fs /logic/arch/theoretical/header.clp))
(load* (fs /lib/arch/theoretical/SimulatorComponents.clp))
(batch* (fs /conf/arch/theoretical/sim.clp))
;-------------------------------------------------------------------------------
(defrule setup-machine
         (initial-fact)
         =>
         (bind ?registers (instance-name (make-instance pc of register (offset 256))))
         (loop-for-count (?i 0 255) do
                         (bind ?registers 
                               (create$ ?registers 
                                        (instance-name 
                                          (make-instance (sym-cat r ?i) of register 
                                                         (offset ?i))))))
         (make-instance proc of machine 
                        (registers ?registers))
         (assert (Machine setup)))

(defrule load-program-into-memory
         ?s <- (Machine setup)
         =>
         (retract ?s)
         ;build the memory cells right now
         (bind ?this (get-char))
         (bind ?i 0)
         (while (!= ?this -1) do
                (make-instance of memory-cell
                               (address ?i)
                               (value ?this))
                (bind ?this (get-char))
                (bind ?i (+ ?i 1)))
         (assert (stage decode execute restart)))

(defrule next-stage
         (declare (salience -10000))
         ?f <- (stage ? $?rest)
         =>
         (retract ?f)
         (if (> (length$ ?rest) 0) then
           (assert (stage $?rest))))

(defrule load-instruction-into-decoder 
         (stage decode $?)
         (object (is-a register)
                 (name [pc])
                 (value ?location))
         (object (is-a memory-cell)
                 (address ?location)
                 (value ?operation))
         =>
         ;tee hee, silly computer scientist, it's not a fixed 
         ;decoder location :D. But it will serve our purposes
         ;well.
         (assert (invoke operation ?operation at ?location)))

(defrule decode:invalid
         (declare (salience -1))
         (stage decode $?)
         ?f <- (invoke operation ? at ?)
         =>
         (retract ?f)
         (printout t "ERROR: Target operation is not valid" crlf)
         (halt))


; MACHINE SPECIFIC DESCRIPTION CODE FOLLOWS:
; With the exception of one instruction, the rest of the instruction
; set operates on available registers (which is why there are 255 of
; them). This only applies to the externally visible instruction set
; which is transformed internally to provide on the fly optimizations.
;
; This is the basis of the procedurally declarative processor design
; I'm implementing. Eventually, this will have more features but at
; this point it is important to get a basic implementation.
;
; TODO: Modify the instruction set to define the program counter
(defrule decode:zero-arg-instruction
         "decodes a zero-argument instruction"
         (stage decode $?)
         ?f <- (invoke operation ?x at ?)
         (instruction (machine-tag ?x)
                      (arg-count 0)
                      (tag ?t))
         =>
         (retract ?f)
         (assert (op ?t)))

(defrule decode:one-arg-instruction
         (stage decode $?)
         ?f <- (invoke operation ?x at ?loc)
         (instruction (machine-tag ?x)
                      (arg-count 1)
                      (tag ?t))
         =>
         (retract ?f)
         (assert (op ?t (next ?loc 1))))

(defrule decode:set
         "Decode the set operation, it has to be handled differently"
         (declare (salience 2))
         (stage decode $?)
         ?f <- (invoke operation ?x&:(= ?x ?*set-instruction*) at ?loc)
         =>
         (retract ?f)
         (assert (op set (next ?loc 9)))) 

(defrule decode:two-arg-instruction
         (stage decode $?)
         ?f <- (invoke operation ?x at ?loc)
         (instruction (machine-tag ?x)
                      (arg-count 2)
                      (tag ?t))

         =>
         (retract ?f)
         (assert (op ?t (next-two ?loc))))

(defrule decode:three-arg-instruction
         (stage decode $?)
         ?f <- (invoke operation ?x at ?loc)
         (instruction (machine-tag ?x)
                      (arg-count 3)
                      (tag ?t))
         =>
         (retract ?f)
         (assert (op ?t (next-three ?loc))))

(defrule execute:set-operation
         (declare (salience 1))
         (stage execute $?)
         ?f <- (op set ?r ?m0 ?m1 ?m2 ?m3 ?m4 ?m5 ?m6 ?m7)
         (object (is-a memory-cell)
                 (address ?r)
                 (value ?e))
         ?reg <- (object (is-a register)
                         (offset ?e))
         (object (is-a memory-cell)
                 (address ?m0)
                 (value ?v0))
         (object (is-a memory-cell)
                 (address ?m1)
                 (value ?v1))
         (object (is-a memory-cell)
                 (address ?m2)
                 (value ?v2))
         (object (is-a memory-cell)
                 (address ?m3)
                 (value ?v3))
         (object (is-a memory-cell)
                 (address ?m4)
                 (value ?v4))
         (object (is-a memory-cell)
                 (address ?m5)
                 (value ?v5))
         (object (is-a memory-cell)
                 (address ?m6)
                 (value ?v6))
         (object (is-a memory-cell)
                 (address ?m7)
                 (value ?v7))
         =>
         (retract ?f)
         (assert (advance pc 10))
         (send ?reg put-value (merge (create$ ?v0 ?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7))))

(defrule execute:nop
         (declare (salience 1))
         (stage execute $?)
         ?f <- (op nop) 
         =>
         (retract ?f))

(defrule execute:terminate
         (declare (salience 1))
         ?f2 <- (stage execute $?)
         ?f <- (op terminate)
         =>
         ;stop execution
         (retract ?f ?f2))

(defrule execute:three-operand-exec
         (stage execute $?)
         ?f <- (op ?action ?mdest ?m0 ?m1)
         (instruction (tag ?action)
                      (funcall ?fn))
         (object (is-a memory-cell)
                 (address ?mdest)
                 (value ?rdest))
         ?destination <- (object (is-a register)
                                 (offset ?rdest))
         (object (is-a memory-cell)
                 (address ?m0)
                 (value ?r0))
         (object (is-a register)
                 (offset ?r0)
                 (value ?v0))
         (object (is-a memory-cell)
                 (address ?m1)
                 (value ?r1))
         (object (is-a register)
                 (offset ?r1)
                 (value ?v1))
         =>
         (retract ?f)
         (assert (advance pc 4))
         (send ?destination put-value (funcall ?fn ?v0 ?v1)))
(defrule execute:two-operand-exec
         (stage execute $?)
         ?f <- (op ?action ?dest ?s0)
         (instruction (tag ?action)
                      (funcall ?fn))
         (object (is-a memory-cell)
                 (address ?dest)
                 (value ?rd))
         ?destination <- (object (is-a register)
                                 (offset ?rd))
         (object (is-a memory-cell)
                 (address ?s0)
                 (value ?r0))
         (object (is-a register)
                 (offset ?r0)
                 (value ?v0))
         =>
         (retract ?f)
         (assert (advance pc 3))
         (send ?destination put-value (funcall ?fn ?v0)))
(defrule execute:store-operand-decompose
         (declare (salience 1))
         (stage execute $?)
         ?f <- (op store ?dest ?s0)
         (object (is-a memory-cell)
                 (address ?dest)
                 (value ?rd))
         ?d <- (object (is-a register)
                       (offset ?rd)
                       (value ?v0))
         (object (is-a memory-cell)
                 (address ?s0)
                 (value ?r0))
         (object (is-a register)
                 (offset ?r0)
                 (value ?v1))
         =>
         (retract ?f)
         ;we need to decompose our number into 
         ;eight cells for writing.
         (assert (to ?v0 write (slice8 ?v1))))

(defrule execute:store-operand-exec:new-cell
         (stage execute $?)
         ?f <- (to ?dest write ?a $?rest)
         (not (exists (object (is-a memory-cell)
                              (address ?dest))))
         =>
         (make-instance of memory-cell
                        (address ?dest)
                        (value ?a))
         (retract ?f)
         (assert (to (+ ?dest 1) write $?rest)))

(defrule execute:store-operand-exec:existent-cell
         (stage execute $?)
         ?f <- (to ?dest write ?a $?rest)
         ?d <- (object (is-a memory-cell)
                       (address ?dest))
         =>
         (retract ?f)
         (assert (to (+ ?dest 1) write $?rest))
         (send ?d put-value ?a))

(defrule execute:store-operand-finish
         (stage execute $?)
         ?f <- (to ?dest write)
         =>
         (assert (advance pc 3))
         (retract ?f))

(defrule execute:load-operand-decompose
         (declare (salience 1))
         (stage execute $?)
         ?f <- (op load ?dest ?s0)
         (object (is-a memory-cell)
                 (address ?dest)
                 (value ?rd))
         ?d <- (object (is-a register)
                       (offset ?rd))
         (object (is-a memory-cell)
                 (address ?s0)
                 (value ?r0))
         (object (is-a register)
                 (offset ?r0)
                 (value ?v0))
         =>
         ;start at this position and pull in eight cells
         (retract ?f)
         (assert (from ?v0 into ?d acquire 8 cells as )))

(defrule execute:load-operand-exec:new-cell
         (stage execute $?)
         ?f <- (from ?s into ?d acquire ?j&:(> ?j 0) cells as $?cells) 
         (not (exists (object (is-a memory-cell)
                              (address ?s))))
         =>
         (make-instance of memory-cell 
                        (address ?s)
                        (value 0))
         (retract ?f)
         (assert (from (+ ?s 1) into ?d acquire (- ?j 1) cells as $?cells 0)))

(defrule execute:load-operand-exec:existent-cell
         (stage execute $?)
         ?f <- (from ?s into ?d acquire ?j&:(> ?j 0) cells as $?cells) 
         (object (is-a memory-cell)
                 (address ?s)
                 (value ?v))
         =>
         (retract ?f)
         (assert (from (+ ?s 1) into ?d acquire (- ?j 1) cells as $?cells ?v )))

(defrule execute:finish-load-operation
         (stage execute $?)
         ?f <- (from ? into ?d acquire 0 cells as $?cells)
         =>
         (retract ?f)
         ; Have to wrap this in a create$ call to make it work
         ; correctly :/
         (bind ?result (merge (create$ $?cells)))
         (send ?d put-value ?result)
         (assert (advance pc 3)))


(defrule advance-program-counter:explicit-value
         (declare (salience 1))
         ?f2 <- (stage restart)
         ?f <- (advance pc ?z)
         ?pc <- (object (is-a register)
                        (name [pc])
                        (value ?location))
         =>
         (retract ?f ?f2)
         (send ?pc put-value (+ ?location ?z))
         (assert (stage decode execute restart)))
(defrule advance-program-counter:one
         ?f <- (stage restart)
         ?pc <- (object (is-a register)
                        (name [pc])
                        (value ?location))
         =>
         (retract ?f)
         (send ?pc put-value (+ ?location 1)))
