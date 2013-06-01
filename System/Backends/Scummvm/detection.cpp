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
#include <System/Backends/Scummvm/AdventureEngine.h>
 
#include "common/config-manager.h"
#include "common/error.h"
#include "common/fs.h"
 
#include "engines/metaengine.h"
 
static const PlainGameDescriptor AdventureEngine_setting[] = {
	{ "AdventureEngine", "AdventureEngine development mode" },
   { "AdventureEngineFinal", "AdventureEngine release mode" },
	{ 0, 0 }
};
 
class AdventureEngineMetaEngine : public MetaEngine {
public:
	virtual const char *getName() const {
		return "The Adventure Engine";
	}
 
	virtual const char *getOriginalCopyright() const {
		return "Copyright (C) Joshua Scoggins";
	}
 
	virtual GameList getSupportedGames() const {
		GameList games;
		const PlainGameDescriptor *g = AdventureEngine_setting;
		while (g->gameid) {
			games.push_back(*g);
			g++;
		}
 
		return games;
	}
 
	virtual GameDescriptor findGame(const char *gameid) const {
		const PlainGameDescriptor *g = AdventureEngine_setting;
		while (g->gameid) {
			if (0 == scumm_stricmp(gameid, g->gameid))
				break;
			g++;
		}
		return GameDescriptor(g->gameid, g->description);
	}
 
	virtual GameList detectGames(const Common::FSList &fslist) const {
		GameList detectedGames;
 
		// Iterate over all files in the given directory
		for (Common::FSList::const_iterator file = fslist.begin(); file != fslist.end(); ++file) {
			if (!file->isDirectory()) {
				const char *gameName = file->getName().c_str();
 
				if (0 == scumm_stricmp("Run.clp", gameName)) {
					// You could check the contents of the file now if you need to.
					detectedGames.push_back(AdventureEngine_setting[0]);
					break;
				} else if(0 == scumm_stricmp("Run.blob", gameName)) {
               detectedGames.push_back(AdventureEngine_setting[1]);
               break;
            }
			}
		}
		return detectedGames;
	}
 
	virtual Common::Error createInstance(OSystem *syst, Engine **engine) const {
		assert(syst);
		assert(engine);
      *engine = new AdventureEngine::AdventureEngineEngine(syst); 
      return Common::kNoError;
      /*
		// Scan the target directory for files (error out if it does not exist)
		Common::FSList fslist;
		Common::FSNode dir(ConfMan.get("path"));
		if (!dir.getChildren(fslist, Common::FSNode::kListAll)) {
			return Common::kNoGameDataFoundError;
		}
 
		// Invoke the detector
		Common::String gameid = ConfMan.get("gameid");
		GameList detectedGames = detectGames(fslist);
 
		for (uint i = 0; i < detectedGames.size(); i++) {
			if (detectedGames[i].gameid() == gameid) {
				// At this point you may want to perform additional sanity checks.
				*engine = new AdventureEngine::AdventureEngineEngine(syst);
				return Common::kNoError;
			}
		}
 
		// Failed to find any game data
		return Common::kNoGameDataFoundError;
      */
	}
};
 
#if PLUGIN_ENABLED_DYNAMIC(ADVENTUREENGINE)
	REGISTER_PLUGIN_DYNAMIC(ADVENTUREENGINE, PLUGIN_TYPE_ENGINE, AdventureEngineMetaEngine);
#else
	REGISTER_PLUGIN_STATIC(ADVENTUREENGINE, PLUGIN_TYPE_ENGINE, AdventureEngineMetaEngine);
#endif
