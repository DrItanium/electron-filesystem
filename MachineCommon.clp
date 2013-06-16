;------------------------------------------------------------------------------
;electron
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
;    * Neither the name of The Adventure Engine nor the
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
(defglobal MAIN 
           ; finding a zero will cause the processor to terminate instruction
           ; execution
           ?*terminate-instruction* = 0
           ?*nop-instruction* = 1
           ?*add-instruction* = 2
           ?*subtract-instruction* = 3
           ?*multiply-instruction* = 4
           ?*divide-instruction* = 5
           ?*right-shift-instruction* = 6
           ?*left-shift-instruction* = 7
           ?*equal-instruction* = 8
           ?*not-equal-instruction* = 9
           ?*less-than-instruction* = 10
           ?*greater-than-instruction* = 11
           ?*and-instruction* = 12
           ?*or-instruction* = 13
           ?*not-instruction* = 14
           ?*branch-instruction* = 15
           ; load r1 <= [r2] is the only supported load instruction
           ?*load-instruction* = 16 
           ; store [r1] <= r2 is the only supported store instruction
           ?*store-instruction* = 17
           ; set r1 <= constant is the only immediate operation in the
           ; instruction set
           ?*set-instruction* = 18
           ; system r1
           ?*interrupt-instruction* = 250
           ;Human representation
           ?*symbol-terminate-instruction* = terminate
           ?*symbol-nop-instruction* = nop
           ?*symbol-add-instruction* = add
           ?*symbol-subtract-instruction* = sub
           ?*symbol-multiply-instruction* = mul
           ?*symbol-divide-instruction* = div
           ?*symbol-right-shift-instruction* = right-shift
           ?*symbol-left-shift-instruction* = left-shift
           ?*symbol-equal-instruction* = eq
           ?*symbol-not-equal-instruction* = neq
           ?*symbol-less-than-instruction* = lt
           ?*symbol-greater-than-instruction* = gt
           ?*symbol-and-instruction* = and
           ?*symbol-or-instruction* = or
           ?*symbol-not-instruction* = not
           ?*symbol-branch-instruction* = branch
           ?*symbol-load-instruction* = load
           ?*symbol-store-instruction* = store
           ?*symbol-set-instruction* = set
           ?*symbol-interrupt-instruction* = interrupt)
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
                   (allowed-symbols FALSE TRUE)))
;-------------------------------------------------------------------------------
(deffacts instructions 
          (instruction (tag ?*symbol-terminate-instruction*)
                       (machine-tag  ?*terminate-instruction*)
                       (arg-count 0))
          (instruction (tag ?*symbol-nop-instruction*)
                       (machine-tag ?*nop-instruction*)
                       (arg-count 0))
          (instruction (tag ?*symbol-add-instruction*)
                       (machine-tag ?*add-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-subtract-instruction*)
                       (machine-tag ?*subtract-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-multiply-instruction*)
                       (machine-tag ?*multiply-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-divide-instruction*)
                       (machine-tag ?*divide-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-right-shift-instruction*)
                       (machine-tag ?*right-shift-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-left-shift-instruction*)
                       (machine-tag ?*left-shift-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-equal-instruction*)
                       (machine-tag ?*equal-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-not-equal-instruction*)
                       (machine-tag ?*not-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-less-than-instruction*)
                       (machine-tag ?*less-than-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-greater-than-instruction*)
                       (machine-tag ?*greater-than-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-and-instruction*)
                       (machine-tag ?*and-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-or-instruction*)
                       (machine-tag ?*or-instruction*)
                       (arg-count 3))
          (instruction (tag ?*symbol-not-instruction*)
                       (machine-tag ?*not-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-branch-instruction*)
                       (machine-tag ?*branch-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-load-instruction*)
                       (machine-tag ?*load-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-store-instruction*)
                       (machine-tag ?*store-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-set-instruction*)
                       (machine-tag ?*set-instruction*)
                       (arg-count 2))
          (instruction (tag ?*symbol-interrupt-instruction*)
                       (machine-tag ?*interrupt-instruction*)
                       (arg-count 1)))
;-------------------------------------------------------------------------------
