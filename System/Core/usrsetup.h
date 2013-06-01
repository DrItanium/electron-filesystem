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
#define ARCH_VERSION "Generic"
/* architecture define macros */
#if defined(__alpha__)
#define ARCH_ALPHA 1
#define ARCH_STRING "alpha"
#else
#define ARCH_ALPHA 0
#endif 

#if defined(__i386__) || defined(i386) || defined(__i386) || defined(_M_IX86) \
	|| defined(_X86_) || defined(_i386) || defined(__X86__)
#define ARCH_X86_32 1
#define ARCH_STRING "x86_32"
#else
#define ARCH_X86_32 0
#endif


#if defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64) || \
	defined(__amd64__) || defined(__amd64) || defined(__x86_64)
#define ARCH_X86_64 1
#define ARCH_STRING "x86_64"
#else
#define ARCH_X86_64 0
#endif

#if defined(__sparc__)
#define ARCH_SPARC 1
#define ARCH_STRING "sparc"
#else
#define ARCH_SPARC 0
#endif

#if defined(__ia64__) || defined(_IA64) || defined(__IA64__) || defined(__ia64) \
	|| defined(_M_IA64) || defined(__itanium__)
#define ARCH_ITANIUM 1
#define ARCH_STRING "Itanium"
#else
#define ARCH_ITANIUM 0
#endif

#if defined(__ppc__) || defined(__powerpc) || defined(__powerpc__) || \
	defined(__POWERPC__) || defined(_M_PPC) || defined(_ARCH_PPC)
#define ARCH_POWERPC_32 1
#define ARCH_STRING "powerpc32"
#else
#define ARCH_POWERPC_32 0
#endif

#if defined(__ppc64__) 
#define ARCH_POWERPC_64 1
#define ARCH_STRING "powerpc64"
#else
#define ARCH_POWERPC_64 0
#endif

#if defined(__arm__)
#define ARCH_ARM 1
#define ARCH_STRING "arm32"
#else
#define ARCH_ARM 0
#endif

#if defined(__aarch64__)
#define ARCH_ARM64 1
#define ARCH_STRING "arm64"
#else
#define ARCH_ARM64 0
#endif

#if defined(__hppa__) || defined(_hppa)
#define ARCH_HPPA 1
#define ARCH_STRING "hppa"
#else
#define ARCH_HPPA 0
#endif

#if defined(__m68k__) || defined(M68000)
#define ARCH_M68K 1
#define ARCH_STRING "m68k"
#else
#define ARCH_M68K 0
#endif

#if defined(__mips__) || defined(__mips) || defined(__MIPS__)
#define ARCH_MIPS 1
#define ARCH_STRING "mips"
#else
#define ARCH_MIPS 0
#endif

#if defined(__sh__)
#define ARCH_SH 1
#define ARCH_STRING "sh"
#else
#define ARCH_SH 0
#endif

#if defined(__s390x__) 
#define ARCH_S390X 1
#define ARCH_STRING "s390x"
#else
#define ARCH_S390X 0
#endif

#if (! ARCH_ALPHA) && (! ARCH_X86_32) && (! ARCH_X86_64) && (! ARCH_SPARC) && \
	 (! ARCH_ITANIUM) && (! ARCH_POWERPC_32) && (! ARCH_POWERPC_32) && (!ARCH_ARM) && \
    (! ARCH_ARM64) && (! ARCH_HPPA) && (! ARCH_M68K) && (! ARCH_MIPS) && (!ARCH_SH) && \
    (! ARCH_S390X) 
#define ARCH_UNKNOWN 1
#define ARCH_STRING "unknown"
#else
#define ARCH_UNKNOWN 0
#endif


/* Hardware Platform Specific Defines */

#if defined(__APPLE__)
#include "TargetConditionals.h"
#define PLATFORM_APPLE 1
#if TARGET_OS_MAC
    #define PLATFORM_APPLE_OSX 1
    #define PLATFORM_APPLE_IOS 0
    #define PLATFORM_HARDWARE_NAME "ApplePC"
#elif TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR
    #define PLATFORM_APPLE_OSX 0
    #define PLATFORM_APPLE_IOS 1
    #define PLATFORM_HARDWARE_NAME "iPhone"
#else
    #define PLATFORM_APPLE_OSX 0
    #define PLATFORM_APPLE_IOS 0
    #define PLATFORM_HARDWARE_NAME "AppleUnknown" 
#endif	
#else
    #define PLATFORM_APPLE 0
    #define PLATFORM_APPLE_OSX 0
    #define PLATFORM_APPLE_IOS 0
#endif



#if defined(__PPU__) || defined(PS3)
#define PLATFORM_PS3 1
#define PLATFORM_HARDWARE_NAME "PS3"
#else
#define PLATFORM_PS3 0
#endif

#if defined(PSP) || defined(__psp__) || defined(__PSP__) || defined(_PSP)
#define PLATFORM_PSP 1
#define PLATFORM_HARDWARE_NAME "PSP"
#else
#define PLATFORM_PSP 0
#endif

#if PLATFORM_PS3 || PLATFORM_PSP
#define PLATFORM_SONY 1
#else
#define PLATFORM_SONY 0
#endif

#if defined(__wii__) || defined(_WII) 
#define PLATFORM_WII 1
#define PLATFORM_HARDWARE_NAME "Wii"
#else
#define PLATFORM_WII 0
#endif

#if PLATFORM_WII
#define PLATFORM_NINTENDO 1
#else
#define PLATFORM_NINTENDO 0
#endif

/* TODO: Fix this define abstraction */
#if defined(_XBOX) 
#define PLATFORM_MICROSOFT 1
#define PLATFORM_XBOX_VERSION _XBOX_VER
#if PLATFORM_XBOX_VERSION < 200
#define PLATFORM_HARDWARE_NAME "Xbox"
#define PLATFORM_XBOX1 1
#define PLATFORM_XBOX360 0
#define PLATFORM_XBOX_UNKNOWN 0
#elif PLATFORM_XBOX_VERSION >= 200 
#define PLATFORM_HARDWARE_NAME "Xbox360"
#define PLATFORM_XBOX1 0
#define PLATFORM_XBOX360 1
#define PLATFORM_XBOX_UNKNOWN 0
#else
#define PLATFORM_HARDWARE_NAME "XboxUnknown"
#define PLATFORM_XBOX1 0
#define PLATFORM_XBOX360 0
#define PLATFORM_XBOX_UNKNOWN 1
#endif
#else
#define PLATFORM_MICROSOFT 0
#define PLATFORM_XBOX_VERSION -1
#define PLATFORM_XBOX1 0
#define PLATFORM_XBOX360 0
#define PLATFORM_XBOX_UNKNOWN 0
#endif

#if (! PLATFORM_APPLE) && (! PLATFORM_SONY) && (! PLATFORM_NINTENDO) && \
	 (! PLATFORM_MICROSOFT)
#define PLATFORM_GENERIC 1
#define PLATFORM_HARDWARE_NAME "Generic"
#else
#define PLATFORM_GENERIC 0
#endif


/* Operating System Specific Defines */
#if defined(_WIN32) 
#define OS_WIN32 1
#define OS_NAME "Windows32"
#else
#define OS_WIN32 0
#endif

#if defined(_WIN64)
#define OS_WIN64 1
#define OS_NAME "Windows64"
#else 
#define OS_WIN64 0
#endif

#if defined(__CYGWIN__)
#define OS_CYGWIN 1
#define OS_NAME "Cygwin"
#else
#define OS_CYGWIN 0
#endif

#if defined(__linux__)
#define OS_LINUX 1
#define OS_NAME "Linux"
#else
#define OS_LINUX 0
#endif

#if defined(__FreeBSD__)
#define OS_FREEBSD 1
#define OS_NAME "FreeBSD"
#else
#define OS_FREEBSD 0
#endif

#if defined(__OpenBSD__)
#define OS_OPENBSD 1
#define OS_NAME "OpenBSD"
#else
#define OS_OPENBSD 0
#endif

#if defined(__NetBSD__)
#define OS_NETBSD 1
#define OS_NAME "NetBSD"
#else
#define OS_NETBSD 0
#endif

#if defined(__DragonFlyBSD__)
#define OS_DRAGONFLYBSD 1
#define OS_NAME "DragonFlyBSD"
#else
#define OS_DRAGONFLYBSD 0
#endif

#if defined(__ANDROID__) || defined(__android__)
#define OS_ANDROID 1
#define OS_NAME "Android"
#else
#define OS_ANDROID 0
#endif

#if PLATFORM_APPLE 
   #if PLATFORM_APPLE_OSX 
       #define OS_OSX 1
       #define OS_IOS 0
       #define OS_APPLE_UNKNOWN 0
       #define OS_NAME "OSX"
   #elif PLATFORM_APPLE_IOS
       #define OS_IOS 1
       #define OS_OSX 0
       #define OS_APPLE_UNKNOWN 0
       #define OS_NAME "iOS"
   #else
       #define OS_IOS 0
       #define OS_OSX 0
       #define OS_APPLE_UNKNOWN 1
       #define OS_NAME "AppleUnknown"
#endif
#else
       #define OS_IOS 0
       #define OS_OSX 0
       #define OS_APPLE_UNKNOWN 0
#endif

#if PLATFORM_SONY
    #if PLATFORM_PS3 || PLATFORM_PSP
	     #define OS_XMB 1
		  #define OS_SONY_UNKNOWN 0
        #define OS_NAME "XMB"
    #else
	     #define OS_XMB 0
		  #define OS_SONY_UNKNOWN 1
        #define OS_NAME "SonyUnknown"
#endif
#else
	     #define OS_XMB 0
		  #define OS_SONY_UNKNOWN 0
#endif

#if PLATFORM_MICROSOFT
    #if PLATFORM_XBOX_VERSION < 200
        #define OS_XBOX1 1
        #define OS_XBOX360 0
        #define OS_XBOX_UNKNOWN 0
        #define OS_NAME "Xbox1"
    #elif PLATFORM_XBOX_VERSION >= 200
        #define OS_XBOX1 0
        #define OS_XBOX360 1
        #define OS_XBOX_UNKNOWN 0
        #define OS_NAME "Xbox360"
    #else
        #define OS_XBOX1 0
        #define OS_XBOX360 0
        #define OS_XBOX_UNKNOWN 1
        #define OS_NAME "XboxUnknown"
#endif
#else
        #define OS_XBOX1 0
        #define OS_XBOX360 0
        #define OS_XBOX_UNKNOWN 0
#endif
      
#if PLATFORM_NINTENDO
    #if PLATFORM_WII
        #define OS_WII 1
        #define OS_NINTENDO_UNKNOWN 0
        #define OS_NAME "Wii"
    #else
        #define OS_WII 0
        #define OS_NINTENDO_UNKNOWN 1
        #define OS_NAME "NintendoUnknown"
    #endif
#else
        #define OS_WII 0
        #define OS_NINTENDO_UNKNOWN 0
#endif

   

#if (! OS_WIN32) && (! OS_WIN64) && (! OS_LINUX) && (! OS_FREEBSD) && \
    (! OS_OSX) && (! OS_IOS) && (! OS_APPLE_UNKNOWN) && (! OS_OPENBSD) && \
    (! OS_NETBSD) && (!OS_XMB) && (!OS_SONY_UNKNOWN) && (! OS_XBOX1) && \
    (! OS_XBOX360) && (! OS_XBOX_UNKNOWN) && (! OS_WII) && \
    (! OS_NINTENDO_UNKNOWN) && (! OS_DRAGONFLYBSD) && (! OS_CYGWIN)
#define OS_UNKNOWN 1
#define OS_NAME "Unknown"
#else
#define OS_UNKNOWN 0
#endif




#endif
