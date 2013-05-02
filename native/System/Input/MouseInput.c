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
#include <Core/clips.h>
#include <System/Input/MouseInput.h> 


extern void GetMouseLocation(void* theEnv, DATA_OBJECT_PTR returnValue);
extern void* GetMouseAction(void* theEnv);

void InitializeMouseInputFunctions(void* theEnv) {
   EnvDefineFunction2(theEnv, 
         (char*)"get-mouse-action",
         'w',
         PTIEF GetMouseAction,
         (char*)"GetMouseAction",
         (char*)"00a");
   EnvDefineFunction2(theEnv,
         (char*)"get-mouse-location",
         'm',
         PTIEF GetMouseLocation,
         (char*)"GetMouseLocation",
         (char*)"00a");
}

void GetMouseLocation(void* theEnv, DATA_OBJECT_PTR returnValue) {
   void* multifieldPtr;

   multifieldPtr = CreateMultifield(2);
   SetMFType(multifieldPtr, 1, INTEGER);
   SetMFType(multifieldPtr, 2, INTEGER);
   //Stub result
   //change this for different backends
   //{
   SetMFValue(multifieldPtr, 1, EnvAddInteger(theEnv, 0));
   SetMFValue(multifieldPtr, 2, EnvAddInteger(theEnv, 0));
   //}
   SetpType(returnValue, MULTIFIELD);
   SetpValue(returnValue, multifieldPtr);
   SetpDOBegin(returnValue, 1);
   SetpDOEnd(returnValue, 2);
}
void* GetMouseAction(void* theEnv) {
   //CHANGE THIS STUB CODE {
   return EnvAddSymbol(theEnv, "Nothing");
   //}
}
