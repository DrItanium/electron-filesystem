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
#include "../clips.h"
#include "PlatformDetection.h"
#if defined(__APPLE__)
#include "TargetConditionals.h"
#endif

#define str(x) #x
#define DefinePlatformIdentFunc(kn, r, cn) \
   EnvDefineFunction(theEnv, (char*)kn, r, PTIEF cn, (char*) str(cn), "00a")
extern void* GetHardwarePlatform(void* theEnv) {
#if defined(__APPLE__) && ( TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR)
      return EnvAddSymbol(theEnv, "iPhone");
#elif defined(__PPU__)
    return EnvAddSymbol(theEnv, "PS3");
#elif defined(PSP) || defined (__psp__) || defined(__PSP__) || defined(_PSP)
    return EnvAddSymbol(theEnv, "PSP");
#elif defined(__wii__) || defined(_WII)
    return EnvAddSymbol(theEnv, "Wii");
#elif defined(_XBOX)
    #if _XBOX_VER < 200
        return EnvAddSymbol(theEnv, "Xbox");
    #elif _XBOX_VER >= 200
        return EnvAddSymbol(theEnv, "Xbox360");
    #else
        return EnvAddSymbol(theEnv, "XboxUnknown");
    #endif
#else
    return EnvAddSymbol(theEnv, "Generic");
#endif
}
extern void* PlatformIsGeneric(void* theEnv) {
#if (defined(__APPLE__) && (TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR)) || defined(__PPU__) || defined(PSP) || defined(__psp__) || defined(__PSP__) || defined(_PSP) || defined(__wii__) || defined(_WII) || defined(_XBOX)
   return FalseSymbol();
#else
   return TrueSymbol();
}

extern void* PlatformIsIPhone(void* theEnv) {
#if defined(__APPLE__) && ( TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR)
   return TrueSymbol();
#else
   return FalseSymbol();
}

extern void* PlatformIsPS3(void* theEnv) {
#if defined(__PPU__)
   return TrueSymbol();
#else
   return FalseSymbol();
#endif
}
extern void* PlatformIsPSP(void* theEnv) {
#if defined(PSP) || defined (__psp__) || defined(__PSP__) || defined(_PSP)
   return TrueSymbol();
#else 
   return FalseSymbol();
#endif
}

extern void* PlatformIsWii(void* theEnv) {
#if defined(__wii__) || defined(_WII) 
   return TrueSymbol();
#else
   return FalseSymbol();
#endif
}

extern void* PlatformIsXbox(void* theEnv) {
#if defined(_XBOX) && (_XBOX_VER < 200) 
   return TrueSymbol();
#else
   return FalseSymbol();
#endif
}

extern void* PlatformIsXbox360(void* theEnv) {
#if defined(_XBOX) && (_XBOX_VER >= 200)
   return TrueSymbol();
#else
   return FalseSymbol();
#endif
}

//This file contains the code describing specific hardware platforms. 
extern void PlatformDetectionFunctionDefinitions(void* theEnv) {
   DefinePlatformIdentFunc("get-hardware-platform", 'k', GetHardwarePlatform);
   DefinePlatformIdentFunc("platform-is-generic", 'b', PlatformIsGeneric);
   DefinePlatformIdentFunc("platform-is-iphone", 'b', PlatformIsIPhone);
   DefinePlatformIdentFunc("platform-is-ps3", 'b', PlatformIsPS3);
   DefinePlatformIdentFunc("platform-is-psp", 'b', PlatformIsPSP);
   DefinePlatformIdentFunc("platform-is-wii", 'b', PlatformIsWii);
   DefinePlatformIdentFunc("platform-is-xbox", 'b', PlatformIsXbox);
   DefinePlatformIdentFunc("platform-is-xbox360", 'b', PlatformIsXbox360);
}

#undef str
#undef DefinePlatformIdentFunc
