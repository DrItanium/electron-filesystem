/*
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

#include "clips.h"
#include "ArchitectureDetection.h"

#define str(x) #x
#define DefineArchIdentFunc(kn, r, cn) \
   EnvDefineFunction2(theEnv, (char*)kn, r, PTIEF cn, (char*) str(cn), "00a")

extern void* GetArchitectureVersion(void* theEnv); 
extern void* GetArchitecture(void* theEnv);
extern void ArchitectureDetectionFunctionDefinitions(void* theEnv) {
   DefineArchIdentFunc("get-architecture", 'w', GetArchitecture);
   DefineArchIdentFunc("get-architecture-version", 'w', GetArchitectureVersion);
}

void* GetArchitectureVersion(void* theEnv) {
#if defined(__alpha__)
    #if defined(__alpha_ev4__)
       return EnvAddSymbol(theEnv, "ev4");
    #elif defined(__alpha_ev5__)
       return EnvAddSymbol(theEnv, "ev5");
    #elif defined(__alpha_ev6__)
       return EnvAddSymbol(theEnv, "ev6");
    #else
       return EnvAddSymbol(theEnv, "Generic");
    #endif
 //TODO: Continue to implement this...there are a lot of things to add here
#else
   return EnvAddSymbol(theEnv, "Generic");
#endif 
}

void* GetArchitecture(void* theEnv) {
#if defined(__i386__) || defined(i386) || defined(__i386) || defined(_M_IX86) || defined(_X86_) || defined(_i386) || defined(__X86__)
   return EnvAddSymbol(theEnv, "x86");
#elif defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64) || defined(__amd64__) || defined(__amd64) || defined(__x86_64)
   return EnvAddSymbol(theEnv, "x64");
#elif defined(__sparc__)
   return EnvAddSymbol(theEnv, "sparc64");
#elif defined(__ia64__) || defined(_IA64) || defined(__IA64__) || defined(__ia64) || defined(_M_IA64) || defined(__itanium__)
   return EnvAddSymbol(theEnv, "ia64");
#elif defined(__ppc__) || defined(__powerpc) || defined(__powerpc__) || defined(__POWERPC__) || defined(_M_PPC) || defined(_ARCH_PPC)
   return EnvAddSymbol(theEnv, "ppc");
#elif defined(__ppc64__)
   return EnvAddSymbol(theEnv, "ppc64");
#elif defined(__arm__)
   return EnvAddSymbol(theEnv, "arm");
#elif defined(__aarch64__)
   return EnvAddSymbol(theEnv, "aarch64");
#elif defined(__alpha__)
   return EnvAddSymbol(theEnv, "alpha");
#elif defined(__hppa__) || defined(_hppa)
   return EnvAddSymbol(theEnv, "hppa");
#elif defined(__m68k__) || defined(M68000)
   return EnvAddSymbol(theEnv, "m68k");
#elif defined(__mips__) || defined(__mips) || defined(__MIPS__)
   return EnvAddSymbol(theEnv, "mips");
#elif defined(__sh__)
   return EnvAddSymbol(theEnv, "sh");
#elif defined(__s390x__) 
   return EnvAddSymbol(theEnv, "s390x");
#else
   return EnvAddSymbol(theEnv, "Unknown");
#endif
}


#undef str
#undef DefineArchIdentFunc
