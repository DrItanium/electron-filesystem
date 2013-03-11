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
#include "HardwareDetection.h"
#if defined(__APPLE__)
#include "TargetConditionals.h"
#endif

#define str(x) #x
#define DefinePlatformIdentFunc(kn, r, cn) \
   EnvDefineFunction2(theEnv, (char*)kn, r, PTIEF cn, (char*) str(cn), "00a")
extern void* GetHardwarePlatform(void* theEnv);
extern int HardwarePlatformIsGeneric(void* theEnv); 
extern int HardwarePlatformIsIPhone(void* theEnv);
extern int HardwarePlatformIsPS3(void* theEnv); 
extern int HardwarePlatformIsPSP(void* theEnv); 
extern int HardwarePlatformIsWii(void* theEnv); 
extern int HardwarePlatformIsXbox(void* theEnv);
extern int HardwarePlatformIsXbox360(void* theEnv);

//This file contains the code describing specific hardware platforms. 
extern void HardwareDetectionFunctionDefinitions(void* theEnv) {
   DefinePlatformIdentFunc("hardware", 'w', GetHardwarePlatform);
   DefinePlatformIdentFunc("hardware-is-generic", 'b', HardwarePlatformIsGeneric);
   DefinePlatformIdentFunc("hardware-is-iphone", 'b', HardwarePlatformIsIPhone);
   DefinePlatformIdentFunc("hardware-is-ps3", 'b', HardwarePlatformIsPS3);
   DefinePlatformIdentFunc("hardware-is-psp", 'b', HardwarePlatformIsPSP);
   DefinePlatformIdentFunc("hardware-is-wii", 'b', HardwarePlatformIsWii);
   DefinePlatformIdentFunc("hardware-is-xbox", 'b', HardwarePlatformIsXbox);
   DefinePlatformIdentFunc("hardware-is-xbox360", 'b', HardwarePlatformIsXbox360);
}

void* GetHardwarePlatform(void* theEnv) {
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
int HardwarePlatformIsGeneric(void* theEnv) {
#if (defined(__APPLE__) && (TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR)) || defined(__PPU__) || defined(PSP) || defined(__psp__) || defined(__PSP__) || defined(_PSP) || defined(__wii__) || defined(_WII) || defined(_XBOX)
   return FALSE;
#else
   return TRUE;
#endif
}

int HardwarePlatformIsIPhone(void* theEnv) {
#if defined(__APPLE__) && ( TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR)
   return TRUE;
#else
   return FALSE;
#endif
}

int HardwarePlatformIsPS3(void* theEnv) {
#if defined(__PPU__)
   return TRUE;
#else
   return FALSE;
#endif
}
int HardwarePlatformIsPSP(void* theEnv) {
#if defined(PSP) || defined (__psp__) || defined(__PSP__) || defined(_PSP)
   return TRUE;
#else 
   return FALSE;
#endif
}

int HardwarePlatformIsWii(void* theEnv) {
#if defined(__wii__) || defined(_WII) 
   return TRUE;
#else
   return FALSE;
#endif
}

int HardwarePlatformIsXbox(void* theEnv) {
#if defined(_XBOX) && (_XBOX_VER < 200) 
   return TRUE;
#else
   return FALSE;
#endif
}

int HardwarePlatformIsXbox360(void* theEnv) {
#if defined(_XBOX) && (_XBOX_VER >= 200)
   return TRUE;
#else
   return FALSE;
#endif
}

#undef str
#undef DefinePlatformIdentFunc
