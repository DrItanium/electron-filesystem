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
#include "SDL/SDL.h"
#include "SDLAudio.h"

extern int SDLOpenAudioFunction(void* theEnv);
extern void SDLPauseAudioFunction(void* theEnv);
extern void SDLCloseAudioFunction(void* theEnv);
extern void SDLAudioDefinitions(void* theEnv) {
   EnvDefineFunction2(theEnv, 
         (char*)"sdl-open-audio",
         'b',
         PTIEF SDLOpenAudioFunction,
         (char*)"SDLOpenAudioFunction",
         "11a");
   EnvDefineFunction2(theEnv,
         (char*)"sdl-pause-audio",
         'v',
         PTIEF SDLPauseAudioFunction,
         (char*)"SDLPauseAudioFunction",
         (char*)"11i");
   EnvDefineFunction2(theEnv,
         (char*)"sdl-close-audio",
         'v',
         PTIEF SDLCloseAudioFunction,
         (char*)"SDLCloseAudioFunction",
         (char*)"00a");
}

int SDLOpenAudioFunction(void* theEnv) 
{
   DATA_OBJECT ref;
   SDL_AudioSpec* spec;
   //This function takes in the pointer to a SDL_AudioSpec
   //struct that was _somehow_ created
   if(EnvArgTypeCheck(theEnv,
            (char*)"sdl-open-audio",  
            1, EXTERNAL_ADDRESS, &ref) == 0)
   {
      return FALSE;
   }
   spec = (SDL_AudioSpec*)(void*)GetValue(ref);
   if(SDL_OpenAudio(spec, NULL)) {
      return TRUE;
   } else {
      return FALSE;
   }
}

void SDLPauseAudioFunction(void* theEnv) {
   DATA_OBJECT ref;
   int value;
   if(EnvArgTypeCheck(theEnv, 
            (char*)"sdl-pause-audio",
            1, INTEGER, &ref) == 0) {
      return;
   }

   value = DOToInteger(ref);
   SDL_PauseAudio(value);
}

void SDLCloseAudioFunction(void* theEnv) {
   SDL_CloseAudio();
}
