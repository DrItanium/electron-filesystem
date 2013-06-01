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
#define FORBIDDEN_SYMBOL_ALLOW_ALL
#include "engines/util.h"
#include <System/Backends/Scummvm/scummvm.h>
#include <System/Backends/Scummvm/scummvm-routers.h>
extern "C" {
  #include <System/Core/clips.h>
}
extern "C" void ExitOverrideFunction(void* theEnv);
extern "C" void Function_InitializeGraphics(void* theEnv);
void InitializeGraphicsInterface(int width, int height, bool scalar);
extern "C" void ScummVMFunctions(void* theEnv) {
   EnvDefineFunction(theEnv,
         (char*)"exit",
         'v',
         PTIEF ExitOverrideFunction,
         (char*)"ExitOverrideFunction");
   InitializeSCUMMVMRouters(theEnv); 
   EnvDefineFunction(theEnv,
         (char*)"init-graphics",
         'v',
         PTIEF Function_InitializeGraphics,
         (char*)"Function_InitializeGraphics");

}

void ExitOverrideFunction(void* theEnv) {
     //just halt execution
     EnvHalt(theEnv);
}

void Function_InitializeGraphics(void* theEnv) {
   int width, height;
   bool scalar;
   DATA_OBJECT a0, a1, a2;
   if(EnvArgCountCheck(theEnv,(char*)"init-graphics",EXACTLY,3) == -1) {
      EnvPrintRouter(theEnv, (char*)"debug", "Error: Too few arguments provided\n");
      return;
   }

   if(EnvArgTypeCheck(theEnv, (char*)"init-graphics",1,INTEGER,&a0) == FALSE) {
      EnvPrintRouter(theEnv, (char*)"debug", "Error: Expected an integer\n");
      return;
   }
   if(EnvArgTypeCheck(theEnv, (char*)"init-graphics",2,INTEGER,&a1) == FALSE) {
      EnvPrintRouter(theEnv, (char*)"debug", "Error: Expected an integer\n");
      return;
   }
   if(EnvArgTypeCheck(theEnv, (char*)"init-graphics",3,INTEGER,&a2) == FALSE) {
      EnvPrintRouter(theEnv, (char*)"debug", "Error: Expected an integer\n");
      return;
   }
   width = DOToInteger(a0);
   height = DOToInteger(a1);
   scalar = (DOToInteger(a2) == 0) ? FALSE : TRUE;
   InitializeGraphicsInterface(width, height, scalar);
}

void InitializeGraphicsInterface(int width, int height, bool scalar) {
   initGraphics(width, height, scalar);
}
