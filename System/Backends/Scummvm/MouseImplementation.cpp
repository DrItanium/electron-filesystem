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
#define FORBIDDEN_SYMBOL_ALLOW_ALL
#include <System/Backends/Scummvm/AdventureEngine.h>
#include "common/events.h"
#include "common/system.h"

extern "C" {
#include <System/Input/MouseInput.h> 
#include <System/Core/clips.h>
}

#define PullOutEngine(env) (AdventureEngine::AdventureEngineEngine*)GetEnvironmentContext(env)
#define NullMultifield(env, rVal) EnvSetMultifieldErrorValue(env, rVal) 

extern "C" void GetMouseLocation(void* theEnv, DATA_OBJECT_PTR returnValue) {
   void* multifield;
   AdventureEngine::AdventureEngineEngine* engine = PullOutEngine(theEnv);
   multifield = EnvCreateMultifield(theEnv, 2);
   Common::EventManager* _eventMan = engine->getEventManager();
   Common::Point pos = _eventMan->getMousePos();
   SetMFType(multifield, 1, INTEGER);
   SetMFValue(multifield, 1, EnvAddLong(theEnv, pos.x));
   SetMFType(multifield, 2, INTEGER);
   SetMFValue(multifield, 2, EnvAddLong(theEnv, pos.y));
   SetpType(returnValue, MULTIFIELD);
   SetpValue(returnValue, multifield);
   SetpDOBegin(returnValue, 1);
   SetpDOEnd(returnValue, 2);
}

void* GetMouseAction(void* theEnv) {
   unsigned state;
   AdventureEngine::AdventureEngineEngine* engine = PullOutEngine(theEnv);
   Common::EventManager* _eventMan = engine->getEventManager();
   Common::Point pos = _eventMan->getMousePos();
   state = _eventMan->getButtonState();
   if(state == engine->previousMouseCommand()) {
      state = 0;
   } else {
      engine->setPreviousMouseCommand(state);
   }

   switch(state) 
   {
      case 0:
         return EnvAddSymbol(theEnv, (char*)"no-click");
      case 1:
         return EnvAddSymbol(theEnv, (char*)"mouse1");
      case 2:
         return EnvAddSymbol(theEnv, (char*)"mouse2");
      case 3:
         return EnvAddSymbol(theEnv, (char*)"mouse3");
      default:
         return EnvAddSymbol(theEnv, (char*)"unknown");
   }
}

#undef PullOutEngine
#undef NullMultifield
