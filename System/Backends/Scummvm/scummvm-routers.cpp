/*
The Adventure Engine
Copyright (c) 2012-2013, Joshua Scoggins 
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

/* Interface with the CLIPS router system to make it possible to use standard
 * CLIPS mechanism to print to the screen
 */
//just in case
#define FORBIDDEN_SYMBOL_ALLOW_ALL 
#include "common/debug.h"
#include "common/str.h"
#include <System/Backends/Scummvm/scummvm-routers.h>
extern "C" {
#include <System/Core/clips.h>
}

extern "C" int FindDebug(void* theEnv, char* rtr);
extern "C" int ExitDebug(void* theEnv, int num);
extern "C" int PrintDebug(void* theEnv, char* logicalName, char* str);

void InitializeSCUMMVMRouters(void* theEnv) {
   EnvAddRouter(theEnv, 
         (char*)"debug",
         40,
         FindDebug,
         PrintDebug,
         NULL,
         NULL,
         ExitDebug);
}

int FindDebug(void* theEnv, char* logicalName) {
   return TRUE;
}

int PrintDebug(void* theEnv, char* logicalName, char* str) {
   //this is the bottom so we don't pass the information on

   //prevent exploits by using format strings
   debugN("%s",str);
   return 1;
}


int ExitDebug(void* theEnv, int num) {
   return 1;
}

