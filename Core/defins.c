   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.24  06/02/06            */
   /*                                                     */
   /*                  DEFINSTANCES MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Kernel definstances interface commands           */
/*              and routines                                 */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/*                                                           */
/* Revision History:                                         */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFINSTANCES_CONSTRUCT

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "dfinsbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "dfinscmp.h"
#endif

#include "argacces.h"
#include "classcom.h"
#include "classfun.h"
#include "cstrccom.h"
#include "cstrcpsr.h"
#include "constant.h"
#include "constrct.h"
#include "envrnmnt.h"
#include "evaluatn.h"
#include "extnfunc.h"
#include "insfun.h"
#include "inspsr.h"
#include "memalloc.h"
#include "modulpsr.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "utility.h"

#define _DEFINS_SOURCE_
#include "defins.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define ACTIVE_RLN "active"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

#if (! BLOAD_ONLY) && (! RUN_TIME)
static int ParseDefinstances(void *,char *);
static SYMBOL_HN *ParseDefinstancesName(void *,char *,int *);
static void RemoveDefinstances(void *,void *);
static void SaveDefinstances(void *,void *,char *);
static intBool RemoveAllDefinstances(void *);
static void DefinstancesDeleteError(void *,char *);

#if DEFRULE_CONSTRUCT
static void CreateInitialDefinstances(void *);
#endif
#endif

#if ! RUN_TIME
static void *AllocateModule(void *);
static void  ReturnModule(void *,void *);
static intBool ClearDefinstancesReady(void *);
static void CheckDefinstancesBusy(void *,struct constructHeader *,void *);
static void DestroyDefinstancesAction(void *,struct constructHeader *,void *);
#endif

static void ResetDefinstances(void *);
static void ResetDefinstancesAction(void *,struct constructHeader *,void *);
static void DeallocateDefinstancesData(void *);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupDefinstances
  DESCRIPTION  : Adds the definstance support routines
                   to the Kernel
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Appropriate function lists modified
  NOTES        : None
 ***************************************************/
globle void SetupDefinstances(
  void *theEnv)
  {
   AllocateEnvironmentData(theEnv,DEFINSTANCES_DATA,sizeof(struct definstancesData),DeallocateDefinstancesData);

   DefinstancesData(theEnv)->DefinstancesModuleIndex =
                RegisterModuleItem(theEnv,(char*)"definstances",
#if (! RUN_TIME)
                                    AllocateModule,ReturnModule,
#else
                                    NULL,NULL,
#endif
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDefinstancesModuleRef,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DefinstancesCModuleReference,
#else
                                    NULL,
#endif
                                    EnvFindDefinstances);

   DefinstancesData(theEnv)->DefinstancesConstruct =
      AddConstruct(theEnv,(char*)"definstances",(char*)"definstances",
#if (! BLOAD_ONLY) && (! RUN_TIME)
                   ParseDefinstances,
#else
                   NULL,
#endif
                   EnvFindDefinstances,
                   GetConstructNamePointer,GetConstructPPForm,
                   GetConstructModuleItem,EnvGetNextDefinstances,SetNextConstruct,
                   EnvIsDefinstancesDeletable,EnvUndefinstances,
#if (! BLOAD_ONLY) && (! RUN_TIME)
                   RemoveDefinstances
#else
                   NULL
#endif
                   );

#if ! RUN_TIME
   AddClearReadyFunction(theEnv,(char*)"definstances",ClearDefinstancesReady,0);

#if ! BLOAD_ONLY
   EnvDefineFunction2(theEnv,(char*)"undefinstances",'v',PTIEF UndefinstancesCommand,(char*)"UndefinstancesCommand",(char*)"11w");
   AddSaveFunction(theEnv,(char*)"definstances",SaveDefinstances,0);

#if DEFRULE_CONSTRUCT
   EnvAddClearFunction(theEnv,(char*)"definstances",CreateInitialDefinstances,-1000);
#endif

#endif

#if DEBUGGING_FUNCTIONS
   EnvDefineFunction2(theEnv,(char*)"ppdefinstances",'v',PTIEF PPDefinstancesCommand ,(char*)"PPDefinstancesCommand",(char*)"11w");
   EnvDefineFunction2(theEnv,(char*)"list-definstances",'v',PTIEF ListDefinstancesCommand,(char*)"ListDefinstancesCommand",(char*)"01");
#endif

   EnvDefineFunction2(theEnv,(char*)"get-definstances-list",'m',PTIEF GetDefinstancesListFunction,
                   (char*)"GetDefinstancesListFunction",(char*)"01");
   EnvDefineFunction2(theEnv,(char*)"definstances-module",'w',PTIEF GetDefinstancesModuleCommand,
                   (char*)"GetDefinstancesModuleCommand",(char*)"11w");

#endif
   EnvAddResetFunction(theEnv,(char*)"definstances",(void (*)(void *)) ResetDefinstances,0);

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   SetupDefinstancesBload(theEnv);
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   SetupDefinstancesCompiler(theEnv);
#endif
  }
  
/*******************************************************/
/* DeallocateDefinstancesData: Deallocates environment */
/*    data for the definstances construct.             */
/*******************************************************/
static void DeallocateDefinstancesData(
  void *theEnv)
  {
#if ! RUN_TIME
   struct definstancesModule *theModuleItem;
   void *theModule;
   
#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv)) return;
#endif
   
   DoForAllConstructs(theEnv,DestroyDefinstancesAction,DefinstancesData(theEnv)->DefinstancesModuleIndex,FALSE,NULL); 
   
   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     {
      theModuleItem = (struct definstancesModule *)
                      GetModuleItem(theEnv,(struct defmodule *) theModule,
                                    DefinstancesData(theEnv)->DefinstancesModuleIndex);
      rtn_struct(theEnv,definstancesModule,theModuleItem);
     }
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

#if ! RUN_TIME  
/*****************************************************/
/* DestroyDefinstancesAction: Action used to remove  */
/*   definstances as a result of DestroyEnvironment. */
/*****************************************************/
static void DestroyDefinstancesAction(
  void *theEnv,
  struct constructHeader *theConstruct,
  void *buffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(buffer)
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct definstances *theDefinstances = (struct definstances *) theConstruct;
   
   if (theDefinstances == NULL) return;
   
   ReturnPackedExpression(theEnv,theDefinstances->mkinstance);
   
   DestroyConstructHeader(theEnv,&theDefinstances->header);

   rtn_struct(theEnv,definstances,theDefinstances);
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theConstruct,theEnv)
#endif
#endif
  }
#endif

/***********************************************************
  NAME         : EnvGetNextDefinstances
  DESCRIPTION  : Finds first or next definstances
  INPUTS       : The address of the current definstances
  RETURNS      : The address of the next definstances
                   (NULL if none)
  SIDE EFFECTS : None
  NOTES        : If ptr == NULL, the first definstances
                    is returned.
 ***********************************************************/
globle void *EnvGetNextDefinstances(
  void *theEnv,
  void *ptr)
  {
   return((void *) GetNextConstructItem(theEnv,(struct constructHeader *) ptr,
                                        DefinstancesData(theEnv)->DefinstancesModuleIndex));
  }

/***************************************************
  NAME         : EnvFindDefinstances
  DESCRIPTION  : Looks up a definstance construct
                   by name-string
  INPUTS       : The symbolic name
  RETURNS      : The definstance address, or NULL
                    if not found
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle void *EnvFindDefinstances(
  void *theEnv,
  char *name)
  {
   return(FindNamedConstruct(theEnv,name,DefinstancesData(theEnv)->DefinstancesConstruct));
  }

/***************************************************
  NAME         : EnvIsDefinstancesDeletable
  DESCRIPTION  : Determines if a definstances
                   can be deleted
  INPUTS       : Address of the definstances
  RETURNS      : TRUE if deletable, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
globle int EnvIsDefinstancesDeletable(
  void *theEnv,
  void *ptr)
  {
   if (! ConstructsDeletable(theEnv))
     { return FALSE; }

   return((((DEFINSTANCES *) ptr)->busy == 0) ? TRUE : FALSE);
  }

/***********************************************************
  NAME         : UndefinstancesCommand
  DESCRIPTION  : Removes a definstance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstance deallocated
  NOTES        : H/L Syntax : (undefinstances <name> | *)
 ***********************************************************/
globle void UndefinstancesCommand(
  void *theEnv)
  {
   UndefconstructCommand(theEnv,(char*)"undefinstances",DefinstancesData(theEnv)->DefinstancesConstruct);
  }

/*****************************************************************
  NAME         : GetDefinstancesModuleCommand
  DESCRIPTION  : Determines to which module a definstances belongs
  INPUTS       : None
  RETURNS      : The symbolic name of the module
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (definstances-module <defins-name>)
 *****************************************************************/
globle void *GetDefinstancesModuleCommand(
  void *theEnv)
  {
   return(GetConstructModuleCommand(theEnv,(char*)"definstances-module",DefinstancesData(theEnv)->DefinstancesConstruct));
  }

/***********************************************************
  NAME         : EnvUndefinstances
  DESCRIPTION  : Removes a definstance
  INPUTS       : Address of definstances to remove
  RETURNS      : TRUE if successful,
                 FALSE otherwise
  SIDE EFFECTS : Definstance deallocated
  NOTES        : None
 ***********************************************************/
globle intBool EnvUndefinstances(
  void *theEnv,
  void *vptr)
  {
#if RUN_TIME || BLOAD_ONLY
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv,vptr)
#endif
   return(FALSE);
#else
   DEFINSTANCES *dptr;

   dptr = (DEFINSTANCES *) vptr;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv))
     return(FALSE);
#endif
   if (dptr == NULL)
     return(RemoveAllDefinstances(theEnv));
   if (EnvIsDefinstancesDeletable(theEnv,vptr) == FALSE)
     return(FALSE);
   RemoveConstructFromModule(theEnv,(struct constructHeader *) vptr);
   RemoveDefinstances(theEnv,(void *) dptr);
   return(TRUE);
#endif
  }

#if DEBUGGING_FUNCTIONS

/***************************************************************
  NAME         : PPDefinstancesCommand
  DESCRIPTION  : Prints out the pretty-print form of a definstance
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax : (ppdefinstances <name>)
 ***************************************************************/
globle void PPDefinstancesCommand(
  void *theEnv)
  {
   PPConstructCommand(theEnv,(char*)"ppdefinstances",DefinstancesData(theEnv)->DefinstancesConstruct);
  }

/***************************************************
  NAME         : ListDefinstancesCommand
  DESCRIPTION  : Displays all definstances names
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances name sprinted
  NOTES        : H/L Interface
 ***************************************************/
globle void ListDefinstancesCommand(
  void *theEnv)
  {
   ListConstructCommand(theEnv,(char*)"list-definstances",DefinstancesData(theEnv)->DefinstancesConstruct);
  }

/***************************************************
  NAME         : EnvListDefinstances
  DESCRIPTION  : Displays all definstances names
  INPUTS       : 1) The logical name of the output
                 2) The module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Definstances names printed
  NOTES        : C Interface
 ***************************************************/
globle void EnvListDefinstances(
  void *theEnv,
  char *logicalName,
  struct defmodule *theModule)
  {
   ListConstruct(theEnv,DefinstancesData(theEnv)->DefinstancesConstruct,logicalName,theModule);
  }

#endif

/****************************************************************
  NAME         : GetDefinstancesListFunction
  DESCRIPTION  : Groups all definstances names into
                 a multifield list
  INPUTS       : A data object buffer to hold
                 the multifield result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : H/L Syntax: (get-definstances-list [<module>])
 ****************************************************************/
globle void GetDefinstancesListFunction(
  void *theEnv,
  DATA_OBJECT*returnValue)
  {
   GetConstructListFunction(theEnv,(char*)"get-definstances-list",returnValue,DefinstancesData(theEnv)->DefinstancesConstruct);
  }

/***************************************************************
  NAME         : EnvGetDefinstancesList
  DESCRIPTION  : Groups all definstances names into
                 a multifield list
  INPUTS       : 1) A data object buffer to hold
                    the multifield result
                 2) The module from which to obtain definstances
  RETURNS      : Nothing useful
  SIDE EFFECTS : Multifield allocated and filled
  NOTES        : External C access
 ***************************************************************/
globle void EnvGetDefinstancesList(
  void *theEnv,
  DATA_OBJECT *returnValue,
  struct defmodule *theModule)
  {
   GetConstructList(theEnv,returnValue,DefinstancesData(theEnv)->DefinstancesConstruct,theModule);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if (! BLOAD_ONLY) && (! RUN_TIME)

/*********************************************************************
  NAME         : ParseDefinstances
  DESCRIPTION  : Parses and allocates a definstances construct
  INPUTS       : The logical name of the input source
  RETURNS      : FALSE if no errors, TRUE otherwise
  SIDE EFFECTS : Definstances parsed and created
  NOTES        : H/L Syntax :

                 (definstances  <name> [active] [<comment>]
                    <instance-definition>+)

                 <instance-definition> ::=
                    (<instance-name> of <class-name> <slot-override>*)

                 <slot-override> ::= (<slot-name> <value-expression>*)
 *********************************************************************/
static int ParseDefinstances(
  void *theEnv,
  char *readSource)
  {
   SYMBOL_HN *dname;
   void *mkinsfcall;
   EXPRESSION *mkinstance,*mkbot = NULL;
   DEFINSTANCES *dobj;
   int active;

   SetPPBufferStatus(theEnv,ON);
   FlushPPBuffer(theEnv);
   SetIndentDepth(theEnv,3);
   SavePPBuffer(theEnv,(char*)"(definstances ");

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded(theEnv)) && (! ConstructData(theEnv)->CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage(theEnv,(char*)"definstances");
      return(TRUE);
     }
#endif
   dname = ParseDefinstancesName(theEnv,readSource,&active);
   if (dname == NULL)
     return(TRUE);

   dobj = get_struct(theEnv,definstances);
   InitializeConstructHeader(theEnv,(char*)"definstances",(struct constructHeader *) dobj,dname);
   dobj->busy = 0;
   dobj->mkinstance = NULL;
#if DEFRULE_CONSTRUCT
   if (active)
     mkinsfcall = (void *) FindFunction(theEnv,(char*)"active-make-instance");
   else
     mkinsfcall = (void *) FindFunction(theEnv,(char*)"make-instance");
#else
   mkinsfcall = (void *) FindFunction(theEnv,(char*)"make-instance");
#endif
   while (GetType(DefclassData(theEnv)->ObjectParseToken) == LPAREN)
     {
      mkinstance = GenConstant(theEnv,UNKNOWN_VALUE,mkinsfcall);
      mkinstance = ParseInitializeInstance(theEnv,mkinstance,readSource);
      if (mkinstance == NULL)
        {
         ReturnExpression(theEnv,dobj->mkinstance);
         rtn_struct(theEnv,definstances,dobj);
         return(TRUE);
        }
      if (ExpressionContainsVariables(mkinstance,FALSE) == TRUE)
        {
         LocalVariableErrorMessage(theEnv,(char*)"definstances");
         ReturnExpression(theEnv,mkinstance);
         ReturnExpression(theEnv,dobj->mkinstance);
         rtn_struct(theEnv,definstances,dobj);
         return(TRUE);
        }
      if (mkbot == NULL)
        dobj->mkinstance = mkinstance;
      else
        GetNextArgument(mkbot) = mkinstance;
      mkbot = mkinstance;
      GetToken(theEnv,readSource,&DefclassData(theEnv)->ObjectParseToken);
      PPBackup(theEnv);
      PPCRAndIndent(theEnv);
      SavePPBuffer(theEnv,DefclassData(theEnv)->ObjectParseToken.printForm);
     }

   if (GetType(DefclassData(theEnv)->ObjectParseToken) != RPAREN)
     {
      ReturnExpression(theEnv,dobj->mkinstance);
      rtn_struct(theEnv,definstances,dobj);
      SyntaxErrorMessage(theEnv,(char*)"definstances");
      return(TRUE);
     }
   else
     {
      if (ConstructData(theEnv)->CheckSyntaxMode)
        {
         ReturnExpression(theEnv,dobj->mkinstance);
         rtn_struct(theEnv,definstances,dobj);
         return(FALSE);
        }
#if DEBUGGING_FUNCTIONS
      if (EnvGetConserveMemory(theEnv) == FALSE)
        {
         if (dobj->mkinstance != NULL)
           PPBackup(theEnv);
         PPBackup(theEnv);
         SavePPBuffer(theEnv,(char*)")\n");
         SetDefinstancesPPForm((void *) dobj,CopyPPBuffer(theEnv));
        }
#endif
      mkinstance = dobj->mkinstance;
      dobj->mkinstance = PackExpression(theEnv,mkinstance);
      ReturnExpression(theEnv,mkinstance);
      IncrementSymbolCount(GetDefinstancesNamePointer((void *) dobj));
      ExpressionInstall(theEnv,dobj->mkinstance);
     }

   AddConstructToModule((struct constructHeader *) dobj);
   return(FALSE);
  }

/*************************************************************
  NAME         : ParseDefinstancesName
  DESCRIPTION  : Parses definstance name and optional comment
                 and optional "active" keyword
  INPUTS       : 1) The logical name of the input source
                 2) Buffer to hold flag indicating if
                    definstances should cause pattern-matching
                    to occur during slot-overrides
  RETURNS      : Address of name symbol, or
                   NULL if there was an error
  SIDE EFFECTS : Token after name or comment is scanned
  NOTES        : Assumes "(definstances" has already
                   been scanned.
 *************************************************************/
static SYMBOL_HN *ParseDefinstancesName(
  void *theEnv,
  char *readSource,
  int *active)
  {
   SYMBOL_HN *dname;

   *active = FALSE;
   dname = GetConstructNameAndComment(theEnv,readSource,&DefclassData(theEnv)->ObjectParseToken,(char*)"definstances",
                                      EnvFindDefinstances,EnvUndefinstances,(char*)"@",
                                      TRUE,FALSE,TRUE);
   if (dname == NULL)
     return(NULL);

#if DEFRULE_CONSTRUCT
   if ((GetType(DefclassData(theEnv)->ObjectParseToken) != SYMBOL) ? FALSE :
       (strcmp(ValueToString(GetValue(DefclassData(theEnv)->ObjectParseToken)),ACTIVE_RLN) == 0))
     {
      PPBackup(theEnv);
      PPBackup(theEnv);
      SavePPBuffer(theEnv,(char*)" ");
      SavePPBuffer(theEnv,DefclassData(theEnv)->ObjectParseToken.printForm);
      PPCRAndIndent(theEnv);
      GetToken(theEnv,readSource,&DefclassData(theEnv)->ObjectParseToken);
      *active = TRUE;
     }
#endif
   if (GetType(DefclassData(theEnv)->ObjectParseToken) == STRING)
     {
      PPBackup(theEnv);
      PPBackup(theEnv);
      SavePPBuffer(theEnv,(char*)" ");
      SavePPBuffer(theEnv,DefclassData(theEnv)->ObjectParseToken.printForm);
      PPCRAndIndent(theEnv);
      GetToken(theEnv,readSource,&DefclassData(theEnv)->ObjectParseToken);
     }
   return(dname);
  }

/**************************************************************
  NAME         : RemoveDefinstances
  DESCRIPTION  : Deallocates and removes a definstance construct
  INPUTS       : The definstance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Existing definstance construct deleted
  NOTES        : Assumes busy count of definstance is 0
 **************************************************************/
static void RemoveDefinstances(
  void *theEnv,
  void *vdptr)
  {
   DEFINSTANCES *dptr = (DEFINSTANCES *) vdptr;

   DecrementSymbolCount(theEnv,GetDefinstancesNamePointer((void *) dptr));
   ExpressionDeinstall(theEnv,dptr->mkinstance);
   ReturnPackedExpression(theEnv,dptr->mkinstance);
   SetDefinstancesPPForm((void *) dptr,NULL);
   ClearUserDataList(theEnv,dptr->header.usrData);
   rtn_struct(theEnv,definstances,dptr);
  }

/***************************************************
  NAME         : SaveDefinstances
  DESCRIPTION  : Prints pretty print form of
                   definstances to specified output
  INPUTS       : The logical name of the output
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void SaveDefinstances(
  void *theEnv,
  void *theModule,
  char *logName)
  {
   SaveConstruct(theEnv,theModule,logName,DefinstancesData(theEnv)->DefinstancesConstruct);
  }

/***************************************************
  NAME         : RemoveAllDefinstances
  DESCRIPTION  : Removes all definstances constructs
  INPUTS       : None
  RETURNS      : TRUE if successful,
                 FALSE otherwise
  SIDE EFFECTS : All definstances deallocated
  NOTES        : None
 ***************************************************/
static intBool RemoveAllDefinstances(
  void *theEnv)
  {
   DEFINSTANCES *dptr,*dhead;
   int success = TRUE;

#if BLOAD || BLOAD_AND_BSAVE

   if (Bloaded(theEnv))
     return(FALSE);
#endif
  dhead = (DEFINSTANCES *) EnvGetNextDefinstances(theEnv,NULL);
  while (dhead != NULL)
    {
     dptr = dhead;
     dhead = (DEFINSTANCES *) EnvGetNextDefinstances(theEnv,(void *) dhead);
     if (EnvIsDefinstancesDeletable(theEnv,(void *) dptr))
       {
        RemoveConstructFromModule(theEnv,(struct constructHeader *) dptr);
        RemoveDefinstances(theEnv,(void *) dptr);
       }
     else
       {
        DefinstancesDeleteError(theEnv,EnvGetDefinstancesName(theEnv,(void *) dptr));
        success = FALSE;
       }
    }
   return(success);
  }

/***************************************************
  NAME         : DefinstancesDeleteError
  DESCRIPTION  : Prints an error message for
                 unsuccessful definstances
                 deletion attempts
  INPUTS       : The name of the definstances
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error message printed
  NOTES        : None
 ***************************************************/
static void DefinstancesDeleteError(
  void *theEnv,
  char *dname)
  {
   CantDeleteItemErrorMessage(theEnv,(char*)"definstances",dname);
  }

#if DEFRULE_CONSTRUCT

/********************************************************
  NAME         : CreateInitialDefinstances
  DESCRIPTION  : Makes the initial-object definstances
                 structure for creating an initial-object
                 which will match default object patterns
                 in defrules
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : initial-object definstances created
  NOTES        : None
 ********************************************************/
static void CreateInitialDefinstances(
  void *theEnv)
  {
   EXPRESSION *tmp;
   DEFINSTANCES *theDefinstances;

   theDefinstances = get_struct(theEnv,definstances);
   InitializeConstructHeader(theEnv,(char*)"definstances",(struct constructHeader *) theDefinstances,
                             DefclassData(theEnv)->INITIAL_OBJECT_SYMBOL);
   theDefinstances->busy = 0;
   tmp = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,(char*)"make-instance"));
   tmp->argList = GenConstant(theEnv,INSTANCE_NAME,(void *) DefclassData(theEnv)->INITIAL_OBJECT_SYMBOL);
   tmp->argList->nextArg =
       GenConstant(theEnv,DEFCLASS_PTR,(void *) LookupDefclassInScope(theEnv,INITIAL_OBJECT_CLASS_NAME));
   theDefinstances->mkinstance = PackExpression(theEnv,tmp);
   ReturnExpression(theEnv,tmp);
   IncrementSymbolCount(GetDefinstancesNamePointer((void *) theDefinstances));
   ExpressionInstall(theEnv,theDefinstances->mkinstance);
   AddConstructToModule((struct constructHeader *) theDefinstances);
  }

#endif

#endif

#if ! RUN_TIME

/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of definstances for a new module
  INPUTS       : None
  RETURNS      : The new definstances module
  SIDE EFFECTS : Definstances module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule(
  void *theEnv)
  {
   return((void *) get_struct(theEnv,definstancesModule));
  }

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a definstances module and
                 all associated definstances
  INPUTS       : The definstances module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and definstances deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
  void *theEnv,
  void *theItem)
  {
#if (! BLOAD_ONLY)
   FreeConstructHeaderModule(theEnv,(struct defmoduleItemHeader *) theItem,DefinstancesData(theEnv)->DefinstancesConstruct);
#endif
   rtn_struct(theEnv,definstancesModule,theItem);
  }

/***************************************************
  NAME         : ClearDefinstancesReady
  DESCRIPTION  : Determines if it is safe to
                 remove all definstances
                 Assumes *all* constructs will be
                 deleted
  INPUTS       : None
  RETURNS      : TRUE if all definstances can
                 be deleted, FALSE otherwise
  SIDE EFFECTS : None
  NOTES        : Used by (clear) and (bload)
 ***************************************************/
static intBool ClearDefinstancesReady(
  void *theEnv)
  {
   int flagBuffer = TRUE;

   DoForAllConstructs(theEnv,CheckDefinstancesBusy,DefinstancesData(theEnv)->DefinstancesModuleIndex,
                      FALSE,(void *) &flagBuffer);
   return(flagBuffer);
  }

/***************************************************
  NAME         : CheckDefinstancesBusy
  DESCRIPTION  : Determines if a definstances is
                 in use or not
  INPUTS       : 1) The definstances
                 2) A buffer to set to 0 if the
                    the definstances is busy
  RETURNS      : Nothing useful
  SIDE EFFECTS : Buffer set to 0 if definstances
                 busy
  NOTES        : The flag buffer is not modified
                 if definstances is not busy
                 (assumed to be initialized to 1)
 ***************************************************/
static void CheckDefinstancesBusy(
  void *theEnv,
  struct constructHeader *theDefinstances,
  void *userBuffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif

   if (((DEFINSTANCES *) theDefinstances)->busy > 0)
     * (int *) userBuffer = FALSE;
  }

#endif

/***************************************************
  NAME         : ResetDefinstances
  DESCRIPTION  : Calls EvaluateExpression for each of
                   the make-instance calls in all
                   of the definstances constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : All instances in the definstances
                   are evaluated (and created if
                   there are no errors)
                 Any previously existing instances
                 are deleted first.
  NOTES        : None
 ***************************************************/
static void ResetDefinstances(
  void *theEnv)
  {
   DoForAllConstructs(theEnv,ResetDefinstancesAction,DefinstancesData(theEnv)->DefinstancesModuleIndex,TRUE,NULL);
  }

/***************************************************
  NAME         : ResetDefinstancesAction
  DESCRIPTION  : Performs all the make-instance
                 calls in a definstances
  INPUTS       : 1) The definstances
                 2) User data buffer (ignored)
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instances created
  NOTES        : None
 ***************************************************/
static void ResetDefinstancesAction(
  void *theEnv,
  struct constructHeader *vDefinstances,
  void *userBuffer)
  {
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(userBuffer)
#endif
   DEFINSTANCES *theDefinstances = (DEFINSTANCES *) vDefinstances;
   EXPRESSION *theExp;
   DATA_OBJECT temp;

   SaveCurrentModule(theEnv);
   EnvSetCurrentModule(theEnv,(void *) vDefinstances->whichModule->theModule);
   theDefinstances->busy++;
   for (theExp = theDefinstances->mkinstance ;
        theExp != NULL ;
        theExp = GetNextArgument(theExp))
     {
      EvaluateExpression(theEnv,theExp,&temp);
      if (EvaluationData(theEnv)->HaltExecution ||
          ((GetType(temp) == SYMBOL) &&
           (GetValue(temp) == EnvFalseSymbol(theEnv))))
        {
         RestoreCurrentModule(theEnv);
         theDefinstances->busy--;
         return;
        }
     }
   theDefinstances->busy--;
   RestoreCurrentModule(theEnv);
  }

#endif


