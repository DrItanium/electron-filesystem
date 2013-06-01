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
#include "common/scummsys.h"

#include "common/config-manager.h"
#include "common/debug.h"
#include "common/debug-channels.h"
#include "common/error.h"
#include "common/EventRecorder.h"
#include "common/file.h"
#include "common/fs.h"
#include "common/timer.h"

#include "engines/util.h"

#include <System/Backends/Scummvm/AdventureEngine.h>

extern "C" {
   #include <System/Core/clips.h>
}


namespace AdventureEngine {

void AdventureEngineEngine::setPreviousMouseCommand(unsigned value) {
   _previousMouseCommand = value;
}
unsigned AdventureEngineEngine::previousMouseCommand() {
   return _previousMouseCommand;
}
void AdventureEngineEngine::setExpertSystemStalled(bool value) {
   _expertSystemStalled = value;
}

bool AdventureEngineEngine::expertSystemStalled() {
   return _expertSystemStalled;
}

static void interruptExpertSystem(void* refCon) {
   AdventureEngineEngine *engine = (AdventureEngineEngine*)refCon;
   //add code for pausing the game at some point
   engine->setExpertSystemStalled(true);
}
AdventureEngineEngine::AdventureEngineEngine(OSystem *syst) 
: Engine(syst) {
   _previousMouseCommand = 0;
   _expertSystemStalled = false;
  clipsEnv = CreateEnvironment();
  debug("AdventureEngineEngine::AdventureEngineEngine: Created CLIPS Environment");
  // Put your engine in a sane state, but do nothing big yet;
  // in particular, do not load data from files; rather, if you
  // need to do such things, do them from run().
  SetEnvironmentContext(clipsEnv, (void*)this);
  //for now I just want to see if this works
  EnvBatchStar(clipsEnv, "Run.clp");
  //TODO: Define default directories once we have a successful compile
  //TODO: Save engine context within the clips environment (pointer)
  //TODO: Add the ability to register applications from CLIPS with SCUMMVM

  // However this is the place to specify all default directories
  //const Common::FSNode gameDataDir(ConfMan.get("path"));
  //SearchMan.addSubDirectoryMatching(gameDataDir, "sound");

  // Here is the right place to set up the engine specific debug channels
  //DebugMan.addDebugChannel(kAdventureEngineDebugExample, "example", "this is just an example for a engine specific debug channel");
  //DebugMan.addDebugChannel(kAdventureEngineDebugExample2, "example2", "also an example");

  // Don't forget to register your random source
  _rnd = new Common::RandomSource("AdventureEngine");

  debug("AdventureEngineEngine::AdventureEngineEngine");
}

AdventureEngineEngine::~AdventureEngineEngine() {
  debug("AdventureEngineEngine::~AdventureEngineEngine");
  DestroyEnvironment(clipsEnv);
  // Dispose your resources here
  delete _rnd;
  delete _console;
}

Common::Error AdventureEngineEngine::run() {
  _console = new Console(this);
  _timer->installTimerProc(interruptExpertSystem, 1000000 / 60, this, "clipsSlowDown"); 
  EnvReset(clipsEnv);
  EnvRun(clipsEnv, -1L);
  _timer->removeTimerProc(interruptExpertSystem);
  return Common::kNoError;
}

} // End of namespace AdventureEngine
