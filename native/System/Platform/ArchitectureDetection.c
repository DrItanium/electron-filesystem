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

#include <Core/clips.h>
#include "ArchitectureDetection.h"

#define str(x) #x
#define DefineArchIdentFunc(kn, r, cn) \
   EnvDefineFunction2(theEnv, (char*)kn, r, PTIEF cn, (char*) str(cn), "00a")

extern void* GetArchitectureVersion(void* theEnv); 
extern void* GetArchitecture(void* theEnv);
extern void ArchitectureDetectionFunctionDefinitions(void* theEnv) {
   DefineArchIdentFunc("architecture", 'w', GetArchitecture);
   DefineArchIdentFunc("architecture-version", 'w', GetArchitectureVersion);
}

void* GetArchitectureVersion(void* theEnv) {
#if ARCH_ALPHA
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
#if ARCH_X86_32
   return EnvAddSymbol(theEnv, "x86");
#elif ARCH_X86_64
   return EnvAddSymbol(theEnv, "x64");
#elif ARCH_SPARC
   return EnvAddSymbol(theEnv, "sparc64");
#elif ARCH_ITANIUM
   return EnvAddSymbol(theEnv, "itanium");
#elif ARCH_POWERPC_32
   return EnvAddSymbol(theEnv, "ppc");
#elif ARCH_POWERPC_64
   return EnvAddSymbol(theEnv, "ppc64");
#elif ARCH_ARM
   return EnvAddSymbol(theEnv, "arm");
#elif ARCH_ARM64
   return EnvAddSymbol(theEnv, "aarch64");
#elif ARCH_ALPHA
   return EnvAddSymbol(theEnv, "alpha");
#elif ARCH_HPPA
   return EnvAddSymbol(theEnv, "hppa");
#elif ARCH_M68K
   return EnvAddSymbol(theEnv, "m68k");
#elif ARCH_MIPS
   return EnvAddSymbol(theEnv, "mips");
#elif ARCH_SH
   return EnvAddSymbol(theEnv, "sh");
#elif ARCH_S390X
   return EnvAddSymbol(theEnv, "s390x");
#else
   return EnvAddSymbol(theEnv, "Unknown");
#endif
}


#undef str
#undef DefineArchIdentFunc
