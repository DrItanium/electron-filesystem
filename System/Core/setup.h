   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  12/07/07            */
   /*             AdventureEngine 2/21/2013               */
   /*                                                     */
   /*                  SETUP HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: This file is the general header file included by */
/*   all of the .c source files. It contains global          */
/*   definitions and the compiler flags which must be edited */
/*   to create a version for a specific machine, operating   */
/*   system, or feature set.                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*      6.24: Default locale modification.                   */
/*                                                           */
/*            Removed CONFLICT_RESOLUTION_STRATEGIES,        */
/*            DYNAMIC_SALIENCE, INCREMENTAL_RESET,           */
/*            LOGICAL_DEPENDENCIES, IMPERATIVE_METHODS,      */
/*            INSTANCE_PATTERN_MATCHING, and                 */
/*            IMPERATIVE_MESSAGE_HANDLERS, and               */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*            Removed the SHORT_LINK_NAMES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Used #ifndef for preprocessor definitions so   */
/*            they can be set at the project or makefile     */
/*            level.                                         */
/*                                                           */
/*            Removed ENVIRONMENT_API_ONLY compilation flag. */
/*                                                           */
/*            Combined BASIC_IO and EXT_IO compilation       */
/*            flags into the IO_FUNCTIONS compilation flag.  */
/*                                                           */    
/*            Changed the EX_MATH compilation flag to        */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Removed VOID definition because of conflict    */
/*            with Windows.h header file.                    */
/*                                                           */    
/*      AdventureEngine 2/21/2013: Added automatic system    */
/*      identification                                       */
/*************************************************************/

#ifndef _H_setup
#define _H_setup

/****************************************************************/
/* -------------------- COMPILER FLAGS ------------------------ */
/****************************************************************/

/*********************************************************************/
/* Flag denoting the environment in which the executable is to run.  */
/* Only one of these flags should be turned on (set to 1) at a time. */
/*********************************************************************/


//#ifndef UNIX_V
//#define UNIX_V  0   /* UNIX System V, 4.2bsd, or HP Unix, presumably with gcc */
//#endif
//
#ifndef UNIX_7
#define UNIX_7  0   /* UNIX System III Version 7 or Sun Unix, presumably with gcc */
#endif


#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFlyBSD__)
	#define UNIX_V 1
#else
	#define UNIX_V 0
#endif

//TODO: Add solaris support

#if defined(__linux__)
    #define LINUX 1
#else
	 #define LINUX 0
#endif

#if defined(__APPLE__)
	#define DARWIN 1
#else 
   #define DARWIN 0
#endif

#if defined(_WIN32) || defined(_WIN64)
	#define WIN_MVC 1
#else
   #define WIN_MVC 0
#endif


#ifndef WIN_GCC
#define WIN_GCC 0   /* Windows XP, with DJGPP 3.21 */
#endif


/* Use GENERIC if nothing else is used. */

#ifndef GENERIC
#if (! UNIX_V) && (! LINUX) && (! UNIX_7) && \
    (! DARWIN) && (! WIN_MVC) && (! WIN_GCC)
#define GENERIC 1   /* Generic (any machine)                   */
#else
#define GENERIC 0   /* Generic (any machine)                   */
#endif
#endif

#if WIN_MVC 
#define IBM 1
#else
#define IBM 0
#endif

/***********************************************/
/* Check for plan9port flag (Compile with 9c)  */
/***********************************************/
#ifdef PLAN9PORT
#define CAN_USE_PLAN9_FEATURES 1
#else
#define CAN_USE_PLAN9_FEATURES 0
#endif
/***********************************************/
/* Some definitions for use with declarations. */
/***********************************************/

#define VOID_ARG void
#define STD_SIZE size_t

#define intBool int
#define globle

/*******************************************/
/* RUN_TIME:  Specifies whether a run-time */
/*   module is being created.              */
/*******************************************/

#ifndef RUN_TIME
#define RUN_TIME 0
#endif

/*************************************************/
/* DEFRULE_CONSTRUCT: Determines whether defrule */
/*   construct is included.                      */
/*************************************************/

#ifndef DEFRULE_CONSTRUCT
#define DEFRULE_CONSTRUCT 1
#endif

/************************************************/
/* DEFMODULE_CONSTRUCT:  Determines whether the */
/*   defmodule construct is included.           */
/************************************************/

#ifndef DEFMODULE_CONSTRUCT
#define DEFMODULE_CONSTRUCT 1
#endif

/****************************************************/
/* DEFTEMPLATE_CONSTRUCT:  Determines whether facts */
/*   and the deftemplate construct are included.    */
/****************************************************/

#ifndef DEFTEMPLATE_CONSTRUCT
#define DEFTEMPLATE_CONSTRUCT 1
#endif

#if ! DEFRULE_CONSTRUCT
#undef DEFTEMPLATE_CONSTRUCT
#define DEFTEMPLATE_CONSTRUCT 0
#endif

/************************************************************/
/* FACT_SET_QUERIES: Determines if fact-set query functions */
/*  such as any-factp and do-for-all-facts are included.    */
/************************************************************/

#ifndef FACT_SET_QUERIES
#define FACT_SET_QUERIES 1
#endif

#if ! DEFTEMPLATE_CONSTRUCT
#undef FACT_SET_QUERIES
#define FACT_SET_QUERIES        0
#endif

/****************************************************/
/* DEFFACTS_CONSTRUCT:  Determines whether deffacts */
/*   construct is included.                         */
/****************************************************/

#ifndef DEFFACT_CONSTRUCT
#define DEFFACTS_CONSTRUCT 1
#endif

#if ! DEFTEMPLATE_CONSTRUCT
#undef DEFFACTS_CONSTRUCT
#define DEFFACTS_CONSTRUCT 0
#endif

/************************************************/
/* DEFGLOBAL_CONSTRUCT:  Determines whether the */
/*   defglobal construct is included.           */
/************************************************/

#ifndef DEFGLOBAL_CONSTRUCT
#define DEFGLOBAL_CONSTRUCT 1
#endif

/**********************************************/
/* DEFFUNCTION_CONSTRUCT:  Determines whether */
/*   deffunction construct is included.       */
/**********************************************/

#ifndef DEFFUNCTION_CONSTRUCT
#define DEFFUNCTION_CONSTRUCT 1
#endif

/*********************************************/
/* DEFGENERIC_CONSTRUCT:  Determines whether */
/*   generic functions  are included.        */
/*********************************************/

#ifndef DEFGENERIC_CONSTRUCT
#define DEFGENERIC_CONSTRUCT 1
#endif

/*****************************************************************/
/* OBJECT_SYSTEM:  Determines whether object system is included. */
/*   The MULTIFIELD_FUNCTIONS flag should also be on if you want */
/*   to be able to manipulate multi-field slots.                 */
/*****************************************************************/

#ifndef OBJECT_SYSTEM
#define OBJECT_SYSTEM 1
#endif

/*****************************************************************/
/* DEFINSTANCES_CONSTRUCT: Determines whether the definstances   */
/*   construct is enabled.                                       */
/*****************************************************************/

#ifndef DEFINSTANCES_CONSTRUCT
#define DEFINSTANCES_CONSTRUCT      1
#endif

#if ! OBJECT_SYSTEM
#undef DEFINSTANCES_CONSTRUCT
#define DEFINSTANCES_CONSTRUCT      0
#endif

/********************************************************************/
/* INSTANCE_SET_QUERIES: Determines if instance-set query functions */
/*  such as any-instancep and do-for-all-instances are included.    */
/********************************************************************/

#ifndef INSTANCE_SET_QUERIES
#define INSTANCE_SET_QUERIES 1
#endif

#if ! OBJECT_SYSTEM
#undef INSTANCE_SET_QUERIES
#define INSTANCE_SET_QUERIES        0
#endif

/******************************************************************/
/* Check for consistencies associated with the defrule construct. */
/******************************************************************/

#if (! DEFTEMPLATE_CONSTRUCT) && (! OBJECT_SYSTEM)
#undef DEFRULE_CONSTRUCT
#define DEFRULE_CONSTRUCT 0
#endif

/*******************************************************************/
/* BLOAD/BSAVE_INSTANCES: Determines if the save/restore-instances */
/*  functions can be enhanced to perform more quickly by using     */
/*  binary files                                                   */
/*******************************************************************/

#ifndef BLOAD_INSTANCES
#define BLOAD_INSTANCES 1
#endif
#ifndef BSAVE_INSTANCES
#define BSAVE_INSTANCES 1
#endif

#if ! OBJECT_SYSTEM
#undef BLOAD_INSTANCES
#undef BSAVE_INSTANCES
#define BLOAD_INSTANCES             0
#define BSAVE_INSTANCES             0
#endif

/****************************************************************/
/* EXTENDED MATH PACKAGE FLAG: If this is on, then the extended */
/* math package functions will be available for use, (normal    */
/* default). If this flag is off, then the extended math        */
/* functions will not be available, and the 30K or so of space  */
/* they require will be free. Usually a concern only on PC type */
/* machines.                                                    */
/****************************************************************/

#ifndef EXTENDED_MATH_FUNCTIONS
#define EXTENDED_MATH_FUNCTIONS 1
#endif

/****************************************************************/
/* TEXT PROCESSING : Turn on this flag for support of the       */
/* hierarchical lookup system.                                  */
/****************************************************************/

#ifndef TEXTPRO_FUNCTIONS
#define TEXTPRO_FUNCTIONS 1
#endif

/****************************************************************/
/* HELP: To implement the help facility, set the flag below and */
/* specify the path and name of the help data file your system. */
/****************************************************************/

#ifndef HELP_FUNCTIONS
#define HELP_FUNCTIONS 1
#endif

#if HELP_FUNCTIONS
#define HELP_DEFAULT "clips.hlp"
#endif

/*************************************************************************/
/* BLOAD_ONLY:      Enables bload command and disables the load command. */
/* BLOAD:           Enables bload command.                               */
/* BLOAD_AND_BSAVE: Enables bload, and bsave commands.                   */
/*************************************************************************/

#ifndef BLOAD_ONLY
#define BLOAD_ONLY 0
#endif
#ifndef BLOAD
#define BLOAD 0
#endif
#ifndef BLOAD_AND_BSAVE
#define BLOAD_AND_BSAVE 1
#endif

#if RUN_TIME
#undef BLOAD_ONLY
#define BLOAD_ONLY      0
#undef BLOAD
#define BLOAD           0
#undef BLOAD_AND_BSAVE
#define BLOAD_AND_BSAVE 0
#endif

/********************************************************************/
/* CONSTRUCT COMPILER: If this flag is turned on, you can generate  */
/*   C code representing the constructs in the current environment. */
/*   With the RUN_TIME flag set, this code can be compiled and      */
/*   linked to create a stand-alone run-time executable.            */
/********************************************************************/

#ifndef CONSTRUCT_COMPILER
#define  CONSTRUCT_COMPILER 1
#endif

#if CONSTRUCT_COMPILER
#define API_HEADER "clips.h"
#endif

/************************************************/
/* IO_FUNCTIONS: Includes printout, read, open, */
/*   close, format, and readline functions.     */
/************************************************/

#ifndef IO_FUNCTIONS
#define IO_FUNCTIONS 1
#endif

/************************************************/
/* STRING_FUNCTIONS: Includes string functions: */
/*   str-length, str-compare, upcase, lowcase,  */
/*   sub-string, str-index, and eval.           */
/************************************************/

#ifndef STRING_FUNCTIONS
#define STRING_FUNCTIONS 1
#endif

/*********************************************/
/* MULTIFIELD_FUNCTIONS: Includes multifield */
/*   functions:  mv-subseq, mv-delete,       */
/*   mv-append, str-explode, str-implode.    */
/*********************************************/

#ifndef MULTIFIELD_FUNCTIONS
#define MULTIFIELD_FUNCTIONS 1
#endif

/****************************************************/
/* DEBUGGING_FUNCTIONS: Includes functions such as  */
/*   rules, facts, matches, ppdefrule, etc.         */
/****************************************************/

#ifndef DEBUGGING_FUNCTIONS
#define DEBUGGING_FUNCTIONS 1
#endif

/***************************************************/
/* PROFILING_FUNCTIONS: Enables code for profiling */
/*   constructs and user functions.                */
/***************************************************/

#ifndef PROFILING_FUNCTIONS
#define PROFILING_FUNCTIONS 1
#endif

/************************************************************************/
/* BLOCK_MEMORY: Causes memory to be allocated in large blocks.         */
/*   INITBUFFERSIZE and BLOCKSIZE should both be set to less than the   */
/*   maximum size of a signed integer.                                  */
/************************************************************************/

#ifndef BLOCK_MEMORY
#define BLOCK_MEMORY 0
#endif

#if BLOCK_MEMORY

#define INITBLOCKSIZE 32000
#define BLOCKSIZE 32000

#endif

/*******************************************************************/
/* WINDOW_INTERFACE : Set this flag if you are recompiling any of  */
/*   the machine specific GUI interfaces. Currently, when enabled, */
/*   this flag disables the more processing used by the help       */
/*   system. This flag also prevents any input or output being     */
/*   directly sent to stdin or stdout.                             */
/*******************************************************************/

#ifndef WINDOW_INTERFACE
#define WINDOW_INTERFACE 0
#endif

/*************************************************************/
/* ALLOW_ENVIRONMENT_GLOBALS: If enabled, tracks the current */
/*   environment and allows environments to be referred to   */
/*   by index. If disabled, CLIPS makes no use of global     */
/*   variables.                                              */
/*************************************************************/

#ifndef ALLOW_ENVIRONMENT_GLOBALS
#define ALLOW_ENVIRONMENT_GLOBALS 1
#endif


/********************************************/
/* DEVELOPER: Enables code for debugging a  */
/*   development version of the executable. */
/********************************************/

#ifndef DEVELOPER
#define DEVELOPER 1
#endif

#if DEVELOPER
#include <assert.h>
#define Bogus(x) assert(! (x))
#else
#define Bogus(x)
#endif

/******************************************************************************/
/* Plan 9 Extensions.                                                         */
/* Enables the use of plan 9 from user space extensions.                      */
/* The p9p libraries will be statically linked into the electron executable   */
/* automatically by 9c. This only works on unix and unix-like platforms.      */
/******************************************************************************/
#ifndef USE_PLAN9_EXTENSIONS 
#define USE_PLAN9_EXTENSIONS 1
#endif

#if ! CAN_USE_PLAN9_FEATURES 
#undef USE_PLAN9_EXTENSIONS
#define USE_PLAN9_EXTENSIONS 0
#endif

/***************************/
/* Environment Definitions */
/***************************/

#include "envrnmnt.h"

/******************************/
/* Compatibilty Redefinitions */
/******************************/

#define PrintCLIPS(x,y) EnvPrintRouter(GetCurrentEnvironment(),x,y)
#define GetcCLIPS(x,y) EnvGetcRouter(GetCurrentEnvironment(),x)
#define UngetcCLIPS(x,y) EnvUngetcRouter(GetCurrentEnvironment(),x,y)
#define ExitCLIPS(x) EnvExitRouter(GetCurrentEnvironment(),x)
#define CLIPSSystemError(x,y) SystemError(x,y)
#define CLIPSFunctionCall(x,y,z) FunctionCall(x,y,z)
#define InitializeCLIPS() InitializeEnvironment()
#define WCLIPS WPROMPT
#define CLIPSTrueSymbol EnvTrueSymbol(GetCurrentEnvironment())
#define CLIPSFalseSymbol EnvFalseSymbol(GetCurrentEnvironment())
#define EnvCLIPSTrueSymbol(theEnv) EnvTrueSymbol(theEnv)
#define EnvCLIPSFalseSymbol(theEnv) EnvFalseSymbol(theEnv)
#define CLIPS_FALSE 0
#define CLIPS_TRUE 1

/*************************************************/
/* Any user defined global setup information can */
/* be included in the file usrsetup.h which is   */
/* an empty file in the baseline version.        */
/*************************************************/

#include "usrsetup.h"

#endif	/* _H_setup */










