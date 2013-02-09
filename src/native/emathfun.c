   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.22  06/15/04            */
   /*                                                     */
   /*            EXTENDED MATH FUNCTIONS MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, asec, acsc, acot, cosh, sinh, tanh, sech,   */
/*   csch, coth, acosh, asinh, atanh, asech, acsch, acoth,   */
/*   mod, exp, log, log10, sqrt, pi, deg-rad, rad-deg,       */
/*   deg-grad, grad-deg, **, and round.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include "setup.h"
#include "argacces.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "router.h"

#include "emathfun.h"

#if EXTENDED_MATH_FUNCTIONS

#include <math.h>

/***************/
/* DEFINITIONS */
/***************/

#ifndef PI
#define PI   3.14159265358979323846
#endif

#ifndef PID2
#define PID2 1.57079632679489661923 /* PI divided by 2 */
#endif

#define SMALLEST_ALLOWED_NUMBER 1e-15
#define dtrunc(x) (((x) < 0.0) ? ceil(x) : floor(x))

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static int                     SingleNumberCheck(void *,char *,double *);
   static int                     TestProximity(double,double);
   static void                    DomainErrorMessage(void *,char *);
   static void                    ArgumentOverflowErrorMessage(void *,char *);
   static void                    SingularityErrorMessage(void *,char *);
   static double                  genacosh(double);
   static double                  genasinh(double);
   static double                  genatanh(double);
   static double                  genasech(double);
   static double                  genacsch(double);
   static double                  genacoth(double);

/************************************************/
/* ExtendedMathFunctionDefinitions: Initializes */
/*   the extended math functions.               */
/************************************************/
globle void ExtendedMathFunctionDefinitions(
  void *theEnv)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,(char*)"cos",      'd', PTIEF CosFunction,      (char*)"CosFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"sin",      'd', PTIEF SinFunction,      (char*)"SinFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"tan",      'd', PTIEF TanFunction,      (char*)"TanFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"sec",      'd', PTIEF SecFunction,      (char*)"SecFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"csc",      'd', PTIEF CscFunction,      (char*)"CscFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"cot",      'd', PTIEF CotFunction,      (char*)"CotFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"acos",     'd', PTIEF AcosFunction,     (char*)"AcosFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"asin",     'd', PTIEF AsinFunction,     (char*)"AsinFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"atan",     'd', PTIEF AtanFunction,     (char*)"AtanFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"asec",     'd', PTIEF AsecFunction,     (char*)"AsecFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"acsc",     'd', PTIEF AcscFunction,     (char*)"AcscFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"acot",     'd', PTIEF AcotFunction,     (char*)"AcotFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"cosh",     'd', PTIEF CoshFunction,     (char*)"CoshFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"sinh",     'd', PTIEF SinhFunction,     (char*)"SinhFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"tanh",     'd', PTIEF TanhFunction,     (char*)"TanhFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"sech",     'd', PTIEF SechFunction,     (char*)"SechFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"csch",     'd', PTIEF CschFunction,     (char*)"CschFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"coth",     'd', PTIEF CothFunction,     (char*)"CothFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"acosh",    'd', PTIEF AcoshFunction,    (char*)"AcoshFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"asinh",    'd', PTIEF AsinhFunction,    (char*)"AsinhFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"atanh",    'd', PTIEF AtanhFunction,    (char*)"AtanhFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"asech",    'd', PTIEF AsechFunction,    (char*)"AsechFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"acsch",    'd', PTIEF AcschFunction,    (char*)"AcschFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"acoth",    'd', PTIEF AcothFunction,    (char*)"AcothFunction", (char*)"11n");

   EnvDefineFunction2(theEnv,(char*)"mod",      'n', PTIEF ModFunction,      (char*)"ModFunction", (char*)"22n");
   EnvDefineFunction2(theEnv,(char*)"exp",      'd', PTIEF ExpFunction,      (char*)"ExpFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"log",      'd', PTIEF LogFunction,      (char*)"LogFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"log10",    'd', PTIEF Log10Function,    (char*)"Log10Function", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"sqrt",     'd', PTIEF SqrtFunction,     (char*)"SqrtFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"pi",       'd', PTIEF PiFunction,       (char*)"PiFunction", (char*)"00");
   EnvDefineFunction2(theEnv,(char*)"deg-rad",  'd', PTIEF DegRadFunction,   (char*)"DegRadFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"rad-deg",  'd', PTIEF RadDegFunction,   (char*)"RadDegFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"deg-grad", 'd', PTIEF DegGradFunction,  (char*)"DegGradFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"grad-deg", 'd', PTIEF GradDegFunction,  (char*)"GradDegFunction", (char*)"11n");
   EnvDefineFunction2(theEnv,(char*)"**",       'd', PTIEF PowFunction,      (char*)"PowFunction", (char*)"22n");
   EnvDefineFunction2(theEnv,(char*)"round",    'g', PTIEF RoundFunction,    (char*)"RoundFunction", (char*)"11n");
#else
#if MAC_MCW || WIN_MCW || MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/************************************************************/
/* SingleNumberCheck: Retrieves the numeric argument for    */
/*   extended math functions which expect a single floating */
/*   point argument.                                        */
/************************************************************/
static int SingleNumberCheck(
  void *theEnv,
  char *functionName,
  double *theNumber)
  {
   DATA_OBJECT theValue;

   if (EnvArgCountCheck(theEnv,functionName,EXACTLY,1) == -1) return(FALSE);
   if (EnvArgTypeCheck(theEnv,functionName,1,FLOAT,&theValue) == FALSE) return(FALSE);

   *theNumber = DOToDouble(theValue);
   return(TRUE);
  }

/**************************************************************/
/* TestProximity: Returns TRUE if the specified number falls  */
/*   within the specified range, otherwise FALSE is returned. */
/**************************************************************/
static int TestProximity(
  double theNumber,
  double range)
  {
   if ((theNumber >= (- range)) && (theNumber <= range)) return TRUE;
   else return FALSE;
  }

/********************************************************/
/* DomainErrorMessage: Generic error message used when  */
/*   a domain error is detected during a call to one of */
/*   the extended math functions.                       */
/********************************************************/
static void DomainErrorMessage(
  void *theEnv,
  char *functionName)
  {
   PrintErrorID(theEnv,(char*)"EMATHFUN",1,FALSE);
   EnvPrintRouter(theEnv,WERROR,(char*)"Domain error for ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR,(char*)" function.\n");
   SetHaltExecution(theEnv,TRUE);
   SetEvaluationError(theEnv,TRUE);
  }

/************************************************************/
/* ArgumentOverflowErrorMessage: Generic error message used */
/*   when an argument overflow is detected during a call to */
/*   one of the extended math functions.                    */
/************************************************************/
static void ArgumentOverflowErrorMessage(
  void *theEnv,
  char *functionName)
  {
   PrintErrorID(theEnv,(char*)"EMATHFUN",2,FALSE);
   EnvPrintRouter(theEnv,WERROR,(char*)"Argument overflow for ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR,(char*)" function.\n");
   SetHaltExecution(theEnv,TRUE);
   SetEvaluationError(theEnv,TRUE);
  }

/************************************************************/
/* SingularityErrorMessage: Generic error message used when */
/*   a singularity is detected during a call to one of the  */
/*   extended math functions.                               */
/************************************************************/
static void SingularityErrorMessage(
  void *theEnv,
  char *functionName)
  {
   PrintErrorID(theEnv,(char*)"EMATHFUN",3,FALSE);
   EnvPrintRouter(theEnv,WERROR,(char*)"Singularity at asymptote in ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR,(char*)" function.\n");
   SetHaltExecution(theEnv,TRUE);
   SetEvaluationError(theEnv,TRUE);
  }

/*************************************/
/* CosFunction: H/L access routine   */
/*   for the cos function.           */
/*************************************/
globle double CosFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"cos",&num) == FALSE) return(0.0);
   return(cos(num));
  }

/*************************************/
/* SinFunction: H/L access routine   */
/*   for the sin function.           */
/*************************************/
globle double SinFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"sin",&num) == FALSE) return(0.0);
   return(sin(num));
  }

/*************************************/
/* TanFunction: H/L access routine   */
/*   for the tan function.           */
/*************************************/
globle double TanFunction(
  void *theEnv)
  {
   double num, tv;

   if (SingleNumberCheck(theEnv,(char*)"tan",&num) == FALSE) return (0.0);
   tv = cos(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(theEnv,(char*)"tan");
      return(0.0);
     }

   return(sin(num) / tv);
  }

/*************************************/
/* SecFunction: H/L access routine   */
/*   for the sec function.           */
/*************************************/
globle double SecFunction(
  void *theEnv)
  {
   double num, tv;

   if (SingleNumberCheck(theEnv,(char*)"sec",&num) == FALSE) return(0.0);

   tv = cos(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(theEnv,(char*)"sec");
      return(0.0);
     }

   return(1.0 / tv);
  }

/*************************************/
/* CscFunction: H/L access routine   */
/*   for the csc function.           */
/*************************************/
globle double CscFunction(
  void *theEnv)
  {
   double num, tv;

   if (SingleNumberCheck(theEnv,(char*)"csc",&num) == FALSE) return(0.0);
   tv = sin(num);
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(theEnv,(char*)"csc");
      return(0.0);
     }

   return(1.0 / tv);
  }

/*************************************/
/* CotFunction: H/L access routine   */
/*   for the cot function.           */
/*************************************/
globle double CotFunction(
  void *theEnv)
  {
    double num, tv;

    if (SingleNumberCheck(theEnv,(char*)"cot",&num) == FALSE) return(0.0);

    tv = sin(num);
    if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
      {
       SingularityErrorMessage(theEnv,(char*)"cot");
       return(0.0);
      }

    return(cos(num) / tv);
  }

/**************************************/
/* AcosFunction: H/L access routine   */
/*   for the acos function.           */
/**************************************/
globle double AcosFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"acos",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage(theEnv,(char*)"acos");
      return(0.0);
     }
    return(acos(num));
  }

/**************************************/
/* AsinFunction: H/L access routine   */
/*   for the asin function.           */
/**************************************/
globle double AsinFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"asin",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage(theEnv,(char*)"asin");
      return(0.0);
     }
   return(asin(num));
  }

/**************************************/
/* AtanFunction: H/L access routine   */
/*   for the atan function.           */
/**************************************/
globle double AtanFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"atan",&num) == FALSE) return(0.0);
   return(atan(num));
  }

/**************************************/
/* AsecFunction: H/L access routine   */
/*   for the asec function.           */
/**************************************/
globle double AsecFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"asec",&num) == FALSE) return(0.0);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage(theEnv,(char*)"asec");
      return(0.0);
     }
    num = 1.0 / num;
    return(acos(num));
  }

/**************************************/
/* AcscFunction: H/L access routine   */
/*   for the acsc function.           */
/**************************************/
globle double AcscFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"acsc",&num) == FALSE) return(0.0);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage(theEnv,(char*)"acsc");
      return(0.0);
     }
    num = 1.0 / num;
    return(asin(num));
  }

/**************************************/
/* AcotFunction: H/L access routine   */
/*   for the acot function.           */
/**************************************/
globle double AcotFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"acot",&num) == FALSE) return(0.0);
   if (TestProximity(num,1e-25) == TRUE)  return(PID2);
   num = 1.0 / num;
   return(atan(num));
  }

/**************************************/
/* CoshFunction: H/L access routine   */
/*   for the cosh function.           */
/**************************************/
globle double CoshFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"cosh",&num) == FALSE) return(0.0);
   return(cosh(num));
  }

/**************************************/
/* SinhFunction: H/L access routine   */
/*   for the sinh function.           */
/**************************************/
globle double SinhFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"sinh",&num) == FALSE) return(0.0);
   return(sinh(num));
  }

/**************************************/
/* TanhFunction: H/L access routine   */
/*   for the tanh function.           */
/**************************************/
globle double TanhFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"tanh",&num) == FALSE) return(0.0);
   return(tanh(num));
  }

/**************************************/
/* SechFunction: H/L access routine   */
/*   for the sech function.           */
/**************************************/
globle double SechFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"sech",&num) == FALSE) return(0.0);
   return(1.0 / cosh(num));
  }

/**************************************/
/* CschFunction: H/L access routine   */
/*   for the csch function.           */
/**************************************/
globle double CschFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"csch",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      SingularityErrorMessage(theEnv,(char*)"csch");
      return(0.0);
     }
   else if (TestProximity(num,1e-25) == TRUE)
     {
      ArgumentOverflowErrorMessage(theEnv,(char*)"csch");
      return(0.0);
     }
   return(1.0 / sinh(num));
  }

/**************************************/
/* CothFunction: H/L access routine   */
/*   for the coth function.           */
/**************************************/
globle double CothFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"coth",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      SingularityErrorMessage(theEnv,(char*)"coth");
      return(0.0);
     }
   else if (TestProximity(num,1e-25) == TRUE)
     {
      ArgumentOverflowErrorMessage(theEnv,(char*)"coth");
      return(0.0);
     }
   return(1.0 / tanh(num));
  }

/***************************************/
/* AcoshFunction: H/L access routine   */
/*   for the acosh function.           */
/***************************************/
globle double AcoshFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"acosh",&num) == FALSE) return(0.0);
   if (num < 1.0)
     {
      DomainErrorMessage(theEnv,(char*)"acosh");
      return(0.0);
     }
   return(genacosh(num));
  }

/***************************************/
/* AsinhFunction: H/L access routine   */
/*   for the asinh function.           */
/***************************************/
globle double AsinhFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"asinh",&num) == FALSE) return(0.0);
   return(genasinh(num));
  }

/***************************************/
/* AtanhFunction: H/L access routine   */
/*   for the atanh function.           */
/***************************************/
globle double AtanhFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"atanh",&num) == FALSE) return(0.0);
   if ((num >= 1.0) || (num <= -1.0))
     {
      DomainErrorMessage(theEnv,(char*)"atanh");
      return(0.0);
     }
   return(genatanh(num));
  }

/***************************************/
/* AsechFunction: H/L access routine   */
/*   for the asech function.           */
/***************************************/
globle double AsechFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"asech",&num) == FALSE) return(0.0);
   if ((num > 1.0) || (num <= 0.0))
     {
      DomainErrorMessage(theEnv,(char*)"asech");
      return(0.0);
     }
   return(genasech(num));
  }

/***************************************/
/* AcschFunction: H/L access routine   */
/*   for the acsch function.           */
/***************************************/
globle double AcschFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"acsch",&num) == FALSE) return(0.0);
   if (num == 0.0)
     {
      DomainErrorMessage(theEnv,(char*)"acsch");
      return(0.0);
     }
   return(genacsch(num));
  }

/***************************************/
/* AcothFunction: H/L access routine   */
/*   for the acoth function.           */
/***************************************/
globle double AcothFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"acoth",&num) == FALSE) return(0.0);
   if ((num <= 1.0) && (num >= -1.0))
     {
      DomainErrorMessage(theEnv,(char*)"acoth");
      return(0.0);
     }
   return(genacoth(num));
  }

/*************************************/
/* ExpFunction: H/L access routine   */
/*   for the exp function.           */
/*************************************/
globle double ExpFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"exp",&num) == FALSE) return(0.0);
   return(exp(num));
  }

/*************************************/
/* LogFunction: H/L access routine   */
/*   for the log function.           */
/*************************************/
globle double LogFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"log",&num) == FALSE) return(0.0);
   if (num < 0.0)
     {
      DomainErrorMessage(theEnv,(char*)"log");
      return(0.0);
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage(theEnv,(char*)"log");
      return(0.0);
     }

   return(log(num));
  }

/***************************************/
/* Log10Function: H/L access routine   */
/*   for the log10 function.           */
/***************************************/
globle double Log10Function(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"log10",&num) == FALSE) return(0.0);
   if (num < 0.0)
     {
      DomainErrorMessage(theEnv,(char*)"log10");
      return(0.0);
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage(theEnv,(char*)"log10");
      return(0.0);
     }

    return(log10(num));
   }

/**************************************/
/* SqrtFunction: H/L access routine   */
/*   for the sqrt function.           */
/**************************************/
globle double SqrtFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"sqrt",&num) == FALSE) return(0.0);
   if (num < 0.00000)
     {
      DomainErrorMessage(theEnv,(char*)"sqrt");
      return(0.0);
     }
   return(sqrt(num));
  }

/*************************************/
/* PowFunction: H/L access routine   */
/*   for the pow function.           */
/*************************************/
globle double PowFunction(
  void *theEnv)
  {
   DATA_OBJECT value1, value2;

   if (EnvArgCountCheck(theEnv,(char*)"**",EXACTLY,2) == -1) return(0.0);

   if (EnvArgTypeCheck(theEnv,(char*)"**",1,FLOAT,&value1) == FALSE) return(0.0);
   if (EnvArgTypeCheck(theEnv,(char*)"**",2,FLOAT,&value2) == FALSE) return(0.0);

    if (((DOToDouble(value1) == 0.0) &&
        (DOToDouble(value2) <= 0.0)) ||
       ((DOToDouble(value1) < 0.0) &&
        (dtrunc((double) DOToDouble(value2)) != DOToDouble(value2))))
     {
      DomainErrorMessage(theEnv,(char*)"**");
      SetHaltExecution(theEnv,TRUE);
      SetEvaluationError(theEnv,TRUE);
      return(0.0);
     }

   return (pow(DOToDouble(value1),DOToDouble(value2)));
  }

/*************************************/
/* ModFunction: H/L access routine   */
/*   for the mod function.           */
/*************************************/
globle void ModFunction(
  void *theEnv,
  DATA_OBJECT_PTR result)
  {
   DATA_OBJECT item1, item2;
   double fnum1, fnum2;
   long long lnum1, lnum2;

   if (EnvArgCountCheck(theEnv,(char*)"mod",EXACTLY,2) == -1)
     {
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,0L);
      return;
     }

   if (EnvArgTypeCheck(theEnv,(char*)"mod",1,INTEGER_OR_FLOAT,&item1) == FALSE)
     {
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,0L);
      return;
     }

   if (EnvArgTypeCheck(theEnv,(char*)"mod",2,INTEGER_OR_FLOAT,&item2) == FALSE)
     {
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,0L);
      return;
     }

   if (((item2.type == INTEGER) ? (ValueToLong(item2.value) == 0L) : FALSE) ||
       ((item2.type == FLOAT) ? ValueToDouble(item2.value) == 0.0 : FALSE))
     {
      DivideByZeroErrorMessage(theEnv,(char*)"mod");
      SetEvaluationError(theEnv,TRUE);
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,0L);
      return;
     }

   if ((item1.type == FLOAT) || (item2.type == FLOAT))
     {
      fnum1 = CoerceToDouble(item1.type,item1.value);
      fnum2 = CoerceToDouble(item2.type,item2.value);
      result->type = FLOAT;
      result->value = (void *) EnvAddDouble(theEnv,fnum1 - (dtrunc(fnum1 / fnum2) * fnum2));
     }
   else
     {
      lnum1 = DOToLong(item1);
      lnum2 = DOToLong(item2);
      result->type = INTEGER;
      result->value = (void *) EnvAddLong(theEnv,lnum1 - (lnum1 / lnum2) * lnum2);
     }
  }

/************************************/
/* PiFunction: H/L access routine   */
/*   for the pi function.           */
/************************************/
globle double PiFunction(
  void *theEnv)
  {

   if (EnvArgCountCheck(theEnv,(char*)"pi",EXACTLY,0) == -1) return(acos(-1.0));
   return(acos(-1.0));
  }

/****************************************/
/* DegRadFunction: H/L access routine   */
/*   for the deg-rad function.          */
/****************************************/
globle double DegRadFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"deg-rad",&num) == FALSE) return(0.0);
   return(num * PI / 180.0);
  }

/****************************************/
/* RadDegFunction: H/L access routine   */
/*   for the rad-deg function.          */
/****************************************/
globle double RadDegFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"rad-deg",&num) == FALSE) return(0.0);
   return(num * 180.0 / PI);
  }

/*****************************************/
/* DegGradFunction: H/L access routine   */
/*   for the deg-grad function.          */
/*****************************************/
globle double DegGradFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"deg-grad",&num) == FALSE) return(0.0);
   return(num / 0.9);
  }

/*****************************************/
/* GradDegFunction: H/L access routine   */
/*   for the grad-deg function.          */
/*****************************************/
globle double GradDegFunction(
  void *theEnv)
  {
   double num;

   if (SingleNumberCheck(theEnv,(char*)"grad-deg",&num) == FALSE) return(0.0);
   return(num * 0.9);
  }

/***************************************/
/* RoundFunction: H/L access routine   */
/*   for the round function.           */
/***************************************/
globle long long RoundFunction(
  void *theEnv)
  {
   DATA_OBJECT result;

   if (EnvArgCountCheck(theEnv,(char*)"round",EXACTLY,1) == -1)
     { return(0LL); }

   if (EnvArgTypeCheck(theEnv,(char*)"round",1,INTEGER_OR_FLOAT,&result) == FALSE)
     { return(0LL); }

   if (result.type == INTEGER)
     { return(ValueToLong(result.value)); }
   else
     { return((long long) ceil(ValueToDouble(result.value) - 0.5)); }
  }

/*******************************************/
/* genacosh: Generic routine for computing */
/*   the hyperbolic arccosine.             */
/*******************************************/
static double genacosh(
  double num)
  {
   return(log(num + sqrt(num * num - 1.0)));
  }

/*******************************************/
/* genasinh: Generic routine for computing */
/*   the hyperbolic arcsine.               */
/*******************************************/
static double genasinh(
  double num)
  {
   return(log(num + sqrt(num * num + 1.0)));
  }

/*******************************************/
/* genatanh: Generic routine for computing */
/*   the hyperbolic arctangent.            */
/*******************************************/
static double genatanh(
  double num)
  {
   return((0.5) * log((1.0 + num) / (1.0 - num)));
  }

/*******************************************/
/* genasech: Generic routine for computing */
/*   the hyperbolic arcsecant.             */
/*******************************************/
static double genasech(
  double num)
  {
   return(log(1.0 / num + sqrt(1.0 / (num * num) - 1.0)));
  }

/*******************************************/
/* genacsch: Generic routine for computing */
/*   the hyperbolic arccosecant.           */
/*******************************************/
static double genacsch(
  double num)
  {
   return(log(1.0 / num + sqrt(1.0 / (num * num) + 1.0)));
  }

/*******************************************/
/* genacoth: Generic routine for computing */
/*   the hyperbolic arccotangent.          */
/*******************************************/
static double genacoth(
  double num)
  {
   return((0.5) * log((num + 1.0) / (num - 1.0)));
  }

#endif

