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
#include <stdio.h>
#include "SDL/SDL.h"
#include "SDLInterface.h"
#include "SDLAudio.h"


//takes in a max of four arguments
int ConvertInitStatement(char* c);
extern int SDLInitialize(void* theEnv);
extern void SDLQuit(void* theEnv); 
extern void SDLFunctionDefinitions(void* theEnv) {
	EnvDefineFunction2(theEnv, 
			(char*)"sdl-init",
			'b',
			PTIEF SDLInitialize,
			(char*)"SDLInitialize",
			(char*)"11m");
	EnvDefineFunction2(theEnv,
			(char*)"sdl-quit",
			'v',
			PTIEF SDLQuit,
			(char*)"SDLQuit",
			(char*)"00a");
   SDLAudioDefinitions(theEnv);
}

int ConvertInitStatement(char* c) {
	if(strcmp(c,"EVERYTHING") == 0) {
		return SDL_INIT_EVERYTHING;
	} else if(strcmp(c, "AUDIO") == 0) {
		return SDL_INIT_AUDIO;
	} else if(strcmp(c, "VIDEO") == 0) {
		return SDL_INIT_VIDEO;
	} else if(strcmp(c, "CDROM") == 0) {
		return SDL_INIT_CDROM;
	} else if(strcmp(c, "TIMER") == 0) {
		return SDL_INIT_TIMER;
	} else if(strcmp(c, "JOYSTICK") == 0) {
		return SDL_INIT_JOYSTICK;
	} else {
		return 0;
	}
}
void SDLQuit(void* theEnv) {
	SDL_Quit();
}
int SDLInitialize(void* theEnv) {
	DATA_OBJECT arg0;
	void* mf;
	unsigned int flags;
	int end, i;
	char* tmp;

	if(EnvArgTypeCheck(theEnv, (char*)"sdl-init", 1, MULTIFIELD, &arg0) == FALSE) {
		return FALSE;
	}
	mf = GetValue(arg0);
	end = GetDOEnd(arg0);
	for(i = GetDOBegin(arg0); i <= end; ++i) {
		tmp = ValueToString(GetMFValue(mf, i));
		flags |= ConvertInitStatement(tmp);
	}
	if(SDL_Init(flags) == 0) {
		return TRUE;	
	} else {
		EnvPrintRouter(theEnv, (char*)"werror", (char*)"ERROR: Couldn't initialize SDL ");
		EnvPrintRouter(theEnv, (char*)"werror", SDL_GetError());
		EnvPrintRouter(theEnv, (char*)"werror", (char*)"\n");
		return FALSE;
	}
}
