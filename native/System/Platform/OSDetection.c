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
#include "OSDetection.h"
#if defined(__APPLE__)
#include "TargetConditionals.h"
#endif
//These are function definitions to find out what OS we're on

extern void* OSGetOperatingSystem(void* theEnv);
extern int OSIsLinux(void* theEnv);
extern int OSIsWindows64(void* theEnv); 
extern int OSIsWindows32(void* theEnv);
extern int OSIsOSX(void* theEnv);
extern int OSIsAndroid(void* theEnv);
extern int OSIsIOS(void* theEnv);
extern int OSIsFreeBSD(void* theEnv);
extern int OSIsOpenBSD(void* theEnv);
extern int OSIsNetBSD(void* theEnv);
extern int OSIsDragonFlyBSD(void* theEnv);
extern int OSIsMicrosoftConsole(void* theEnv);
extern int OSIsSonyConsole(void* theEnv);
extern int OSIsNintendoConsole(void* theEnv);

extern void OSDetectionFunctionDefinitions(void* theEnv) {
   //capture the standard operating system call
   EnvDefineFunction2(theEnv,
         (char*)"operating-system",
         'w',
         PTIEF OSGetOperatingSystem,
         (char*)"OSGetOperatingSystem",
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-android", 
         'b',
         PTIEF OSIsAndroid, 
         (char*)"OSIsAndroid",
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-windows32", 
         'b',
         PTIEF OSIsWindows32, 
         (char*)"OSIsWindows32",
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-windows64", 
         'b',
         PTIEF OSIsWindows64, 
         (char*)"OSIsWindows64",
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-osx", 
         'b',
         PTIEF OSIsOSX, 
         (char*)"OSIsOSX", 
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-linux", 
         'b',
         PTIEF OSIsLinux, 
         (char*)"OSIsLinux", 
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-freebsd", 
         'b',
         PTIEF OSIsFreeBSD, 
         (char*)"OSIsFreeBSD",
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-openbsd", 
         'b',
         PTIEF OSIsOpenBSD, 
         (char*)"OSIsOpenBSD",
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-netbsd", 
         'b',
         PTIEF OSIsNetBSD, 
         (char*)"OSIsNetBSD", 
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-dragonflybsd", 
         'b',
         PTIEF OSIsDragonFlyBSD, 
         (char*)"OSIsDragonFlyBSD", 
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-ios", 
         'b',
         PTIEF OSIsIOS, 
         (char*)"OSIsIOS", 
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-microsoft-console", 
         'b', 
         PTIEF OSIsMicrosoftConsole, 
         (char*)"OSIsMicrosoftConsole", 
         (char*) "00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-sony-console", 
         'b',
         PTIEF OSIsSonyConsole, 
         (char*)"OSIsSonyConsole", 
         (char*)"00a");
   EnvDefineFunction2(theEnv, 
         (char*)"operating-system-is-nintendo-console", 
         'b', 
         PTIEF OSIsNintendoConsole, 
         (char*)"OSIsNintendoConsole", 
         (char*)"00a");
}

void* OSGetOperatingSystem(void* theEnv) {
#if OS_LINUX
   return EnvAddSymbol(theEnv, "Linux");
#elif OS_FREEBSD
   return EnvAddSymbol(theEnv, "FreeBSD");
#elif OS_OPENBSD
   return EnvAddSymbol(theEnv, "OpenBSD");
#elif OS_NETBSD
   return EnvAddSymbol(theEnv, "NetBSD");
#elif OS_DRAGONFLYBSD
   return EnvAddSymbol(theEnv, "DragonFlyBSD");
#elif OS_ANDROID
   return EnvAddSymbol(theEnv, "Android");
#elif OS_WIN64
   return EnvAddSymbol(theEnv, "Windows64");
#elif OS_WIN32
   return EnvAddSymbol(theEnv, "Windows32");
#elif OS_OSX
   return EnvAddSymbol(theEnv, "OSX");
#elif OS_IOS
   return EnvAddSymbol(theEnv, "iOS");
#elif OS_APPLE_UNKNOWN
   return EnvAddSymbol(theEnv, "AppleUnknown");
#elif OS_XMB
   return EnvAddSymbol(theEnv, "XMB");
#elif OS_SONY_UNKNOWN 
	return EnvAddSymbol(theEnv, "SonyUnknown");
#elif OS_XBOX1
   return EnvAddSymbol(theEnv, "Xbox1");
#elif OS_XBOX360
   return EnvAddSymbol(theEnv, "Xbox360");
#elif OS_XBOX_UNKNOWN
   return EnvAddSymbol(theEnv, "XboxUnknown");
#elif OS_WII 
   return EnvAddSymbol(theEnv, "Wii");
#elif OS_NINTENDO_UNKNOWN
   return EnvAddSymbol(theEnv, "NintendoUnknown");
#else
   return EnvAddSymbol(theEnv, "Unknown");
#endif
}

//These are function definitions to find out what OS we're on
int OSIsLinux(void* theEnv) {
#if OS_LINUX
	 return TRUE;
#else
   return FALSE;
#endif
}
int OSIsWindows64(void* theEnv) {
#if OS_WIN64
   return TRUE;
#else 
   return FALSE;
#endif
}
int OSIsWindows32(void* theEnv) {
#if OS_WIN32
#warning "WIN32 is defined"
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsOSX(void* theEnv) {
#if OS_OSX
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsAndroid(void* theEnv) {
#if OS_ANDROID
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsIOS(void* theEnv) {
#if OS_IOS
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsFreeBSD(void* theEnv) {
#if OS_FREEBSD
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsOpenBSD(void* theEnv) {
#if OS_OPENBSD
   return TRUE;
#else
   return FALSE;
#endif
}

int OSIsNetBSD(void* theEnv) {
#if OS_NETBSD
   return TRUE;
#else
   return FALSE;
#endif
}

int OSIsDragonFlyBSD(void* theEnv) {
#if OS_DRAGONFLYBSD 
   return TRUE;
#else
   return FALSE;
#endif
}

/* 
 * These are for consoles...by default these will return FALSE
 * I use these generic names because there are many devices (future and 
 * current) that are a part of this class of operating systems. We use the
 * platform detection functions to figure out what platform we are on
 */
int OSIsMicrosoftConsole(void* theEnv) {
   //Microsoft's console is easy
#if PLATFORM_XBOX_FAMILY
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsSonyConsole(void* theEnv) {
   //Playstation 2 isn't supported
#if PLATFORM_SONY
   return TRUE;
#else
   return FALSE;
#endif
}
int OSIsNintendoConsole(void* theEnv) {
   //GameCube isn't supported...not enough RAM!
#if PLATFORM_NINTENDO
   return TRUE;
#else
   return FALSE;
#endif
}

