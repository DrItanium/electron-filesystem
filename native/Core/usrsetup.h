/*
The Adventure Engine
Copyright (c) 2013, Joshua Scoggins 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Joshua Scoggins nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef _CORE_USER_SETUP_H
#define _CORE_USER_SETUP_H
/* architecture define macros */
#if defined(__alpha__)
#define ARCH_ALPHA 1
#else
#define ARCH_ALPHA 0
#endif 

#if defined(__i386__) || defined(i386) || defined(__i386) || defined(_M_IX86) \
	|| defined(_X86_) || defined(_i386) || defined(__X86__)
#define ARCH_X86_32 1
#else
#define ARCH_X86_32 0
#endif


#if defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64) || \
	defined(__amd64__) || defined(__amd64) || defined(__x86_64)
#define ARCH_X86_64 1
#else
#define ARCH_X86_64 0
#endif

#if defined(__sparc__)
#define ARCH_SPARC 1
#else
#define ARCH_SPARC 0
#endif

#if defined(__ia64__) || defined(_IA64) || defined(__IA64__) || defined(__ia64) \
	|| defined(_M_IA64) || defined(__itanium__)
#define ARCH_ITANIUM 1
#else
#define ARCH_ITANIUM 0
#endif

#if defined(__ppc__) || defined(__powerpc) || defined(__powerpc__) || \
	defined(__POWERPC__) || defined(_M_PPC) || defined(_ARCH_PPC)
#define ARCH_POWERPC_32 1
#else
#define ARCH_POWERPC_32 0
#endif

#if defined(__ppc64__) 
#define ARCH_POWERPC_64 1
#else
#define ARCH_POWERPC_64 0
#endif

#if defined(__arm__)
#define ARCH_ARM 1
#else
#define ARCH_ARM 0
#endif

#if defined(__aarch64__)
#define ARCH_ARM64 1
#else
#define ARCH_ARM64 0
#endif

#if defined(__hppa__) || defined(_hppa)
#define ARCH_HPPA 1
#else
#define ARCH_HPPA 0
#endif

#if defined(__m68k__) || defined(M68000)
#define ARCH_M68K 1
#else
#define ARCH_M68K 0
#endif

#if defined(__mips__) || defined(__mips) || defined(__MIPS__)
#define ARCH_MIPS 1
#else
#define ARCH_MIPS 0
#endif

#if defined(__sh__)
#define ARCH_SH 1
#else
#define ARCH_SH 0
#endif

#if defined(__s390x__) 
#define ARCH_S390X 1
#else
#define ARCH_S390X 0
#endif

#if (! ARCH_ALPHA) && (! ARCH_X86_32) && (! ARCH_X86_64) && (! ARCH_SPARC) && \
	 (! ARCH_ITANIUM) && (! ARCH_POWERPC_32) && (! ARCH_POWERPC_32) && (!ARCH_ARM) && \
    (! ARCH_ARM64) && (! ARCH_HPPA) && (! ARCH_M68K) && (! ARCH_MIPS) && (!ARCH_SH) && \
    (! ARCH_S390X) 
#define ARCH_UNKNOWN 1
#else
#define ARCH_UNKNOWN 0
#endif

#endif 
