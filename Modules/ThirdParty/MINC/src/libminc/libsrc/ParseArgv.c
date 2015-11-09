/*
 * ParseArgv.c --
 *
 *	This file contains a procedure that handles table-based
 *	argv-argc parsing.
 *
 * Copyright 1990 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 *
 * This file has been modified to not rely on tcl, tk or X11.
 * Based on tkArgv.c from tk2.3 : 
 *
 * Modifications by Peter Neelin (November 27, 1992)
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef HAVE_MINC1
#include "minc.h"
#endif

#ifdef HAVE_MINC2
#include <hdf5.h>
#endif 

#include <math.h>
#include <ParseArgv.h>

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/*
 * Default table of argument descriptors.  These are normally available
 * in every application.
 */

static ArgvInfo defaultTable[] = {
    {"-help",	ARGV_HELP,	(char *) NULL,	(char *) NULL,
	"Print summary of command-line options and abort"},
    {"-version", ARGV_VERSION,  (char *) NULL,  (char *) NULL,
        "Print version number of program and exit"},
    {NULL,	ARGV_END,	(char *) NULL,	(char *) NULL,
	(char *) NULL}
};

/*
 * Forward declarations for functions defined in this file:
 */

static void PrintUsage(ArgvInfo *argTable, int flags);
static void PrintVersion(ArgvInfo *argTable);

/*
 * ParseLong
 *
 * Quick replacement for strtol which eliminates the undesirable property
 * of interpreting numbers with leading '0' characters as octal, while
 * retaining "0x" as indicating a hexidecimal number.
 */
long int
ParseLong(const char *argPtr, char **endPtr)
{
  const char *tmpPtr = argPtr;
  int base = 10;                /* Default to decimal. */

  /* Skip sign if present.
   */
  if (tmpPtr[0] == '+' || tmpPtr[0] == '-')
    tmpPtr++;

  /* If '0x' or '0X', treat this as hex.
   */
  if (tmpPtr[0] == '0' && (tmpPtr[1] == 'x' || tmpPtr[1] == 'X'))
    base = 16;

  return strtol(argPtr, endPtr, base);
}

/*
 *----------------------------------------------------------------------
 *
 * ParseArgv --
 *
 *	Process an argv array according to a table of expected
 *	command-line options.  See the manual page for more details.
 *
 * Results:
 *	The return value is a Boolean value with non-zero indicating an 
 *      error.  
 *	If an error occurs then an error message is printed on stderr.
 *	Under normal conditions, both *argcPtr and *argv are modified
 *	to return the arguments that couldn't be processed here (they
 *	didn't match the option table, or followed an ARGV_REST
 *	argument).
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

int
ParseArgv(argcPtr, argv, argTable, flags)
    int *argcPtr;		/* Number of arguments in argv.  Modified
				 * to hold # args left in argv at end. */
    char **argv;		/* Array of arguments.  Modified to hold
				 * those that couldn't be processed here. */
    ArgvInfo *argTable;	/* Array of option descriptions */
    int flags;			/* Or'ed combination of various flag bits,
				 * such as ARGV_NO_DEFAULTS. */
{
   ArgvInfo *infoPtr;
				/* Pointer to the current entry in the
				 * table of argument descriptions. */
   ArgvInfo *matchPtr;	/* Descriptor that matches current argument. */
   char *curArg;		/* Current argument */
   char c;		/* Second character of current arg (used for
				 * quick check for matching;  use 2nd char.
				 * because first char. will almost always
				 * be '-'). */
   int srcIndex;		/* Location from which to read next argument
				 * from argv. */
   int dstIndex;		/* Index into argv to which next unused
				 * argument should be copied (never greater
				 * than srcIndex). */
   int argc;			/* # arguments in argv still to process. */
   size_t length;			/* Number of characters in current argument. */
   uintptr_t nargs;        /* Number of following arguments to get. */
   uintptr_t i;

/* Macro to optionally print errors */
#define FPRINTF if (!(flags&ARGV_NO_PRINT)) (void) fprintf

   if (flags & ARGV_DONT_SKIP_FIRST_ARG) {
      srcIndex = dstIndex = 0;
      argc = *argcPtr;
   } else {
      srcIndex = dstIndex = 1;
      argc = *argcPtr-1;
   }

   while (argc > 0) {
      curArg = argv[srcIndex];
      srcIndex++;
      argc--;
      c = curArg[1];
      length = strlen(curArg);
      
      /*
       * Loop throught the argument descriptors searching for one with
       * the matching key string.  If found, leave a pointer to it in
       * matchPtr.
       */

      matchPtr = NULL;
      for (i = 0; i < 2; i++) {
         if (i == 0) {
            infoPtr = argTable;
         } else {
            infoPtr = defaultTable;
         }
         for (; infoPtr->type != ARGV_END; infoPtr++) {
            if (infoPtr->key == NULL) {
               continue;
            }
            if ((infoPtr->key[1] != c)
                || (strncmp(infoPtr->key, curArg, length) != 0)) {
               continue;
            }
            if (infoPtr->key[length] == 0) {
               matchPtr = infoPtr;
               goto gotMatch;
            }
            if (flags & ARGV_NO_ABBREV) {
               continue;
            }
            if (matchPtr != NULL) {
               FPRINTF(stderr, "ambiguous option \"%s\"\n", curArg);
               return TRUE;
            }
            matchPtr = infoPtr;
         }
      }
      if (matchPtr == NULL) {

         /*
          * Unrecognized argument.  Just copy it down, unless the caller
          * prefers an error to be registered.
          */

         if (flags & ARGV_NO_LEFTOVERS) {
            FPRINTF(stderr, "unrecognized argument \"%s\"\n", curArg);
         }
         argv[dstIndex] = curArg;
         dstIndex++;
         continue;
      }

      /*
       * Take the appropriate action based on the option type
       */
	gotMatch:
      infoPtr = matchPtr;
      switch (infoPtr->type) {
      case ARGV_CONSTANT:
         *((int *) infoPtr->dst) = (intptr_t) infoPtr->src;
         break;
      case ARGV_INT:
         nargs = (uintptr_t) infoPtr->src;
         if (nargs<1) nargs=1;
         for (i=0; i<nargs; i++) {
            if (argc == 0) {
               goto missingArg;
            } else {
               char *endPtr;

               *(((int *) infoPtr->dst)+i) =
                  ParseLong(argv[srcIndex], &endPtr);
               if ((endPtr == argv[srcIndex]) || (*endPtr != 0)) {
                  FPRINTF(stderr, 
                  "expected integer argument for \"%s\" but got \"%s\"",
                          infoPtr->key, argv[srcIndex]);
                  return TRUE;
               }
               srcIndex++;
               argc--;
            }
         }
         break;
      case ARGV_LONG:
         nargs = (uintptr_t) infoPtr->src;
         if (nargs<1) nargs=1;
         for (i=0; i<nargs; i++) {
            if (argc == 0) {
               goto missingArg;
            } else {
               char *endPtr;

               *(((long *) infoPtr->dst)+i) =
                  ParseLong(argv[srcIndex], &endPtr);
               if ((endPtr == argv[srcIndex]) || (*endPtr != 0)) {
                  FPRINTF(stderr, 
                  "expected integer argument for \"%s\" but got \"%s\"",
                          infoPtr->key, argv[srcIndex]);
                  return TRUE;
               }
               srcIndex++;
               argc--;
            }
         }
         break;

      case ARGV_STRING:
         nargs = (uintptr_t) infoPtr->src;
         if (nargs<1) nargs=1;
         for (i=0; i<nargs; i++) {
            if (argc == 0) {
               goto missingArg;
            } else {
               *(((char **)infoPtr->dst)+i) = argv[srcIndex];
               srcIndex++;
               argc--;
            }
         }
         break;
      case ARGV_REST:
         *((int *) infoPtr->dst) = dstIndex;
         goto argsDone;
      case ARGV_FLOAT:
         nargs = (uintptr_t) infoPtr->src;
         if (nargs<1) nargs=1;
         for (i=0; i<nargs; i++) {
            if (argc == 0) {
               goto missingArg;
            } else {
               char *endPtr;

               *(((double *) infoPtr->dst)+i) =
                  strtod(argv[srcIndex], &endPtr);
               if ((endPtr == argv[srcIndex]) || (*endPtr != 0)) {
                  FPRINTF(stderr, 
       "expected floating-point argument for \"%s\" but got\"%s\"\n",
                          infoPtr->key, argv[srcIndex]);
                  return TRUE;
               }
               srcIndex++;
               argc--;
            }
         }
         break;
      case ARGV_FUNC: {
         int (*handlerProc)() =  (int (*)())(uintptr_t)infoPtr->src;
		
         if ((*handlerProc)(infoPtr->dst, infoPtr->key,
                            argv[srcIndex])) {
            srcIndex += 1;
            argc -= 1;
         }
         break;
      }
      case ARGV_GENFUNC: {
         int (*handlerProc)() = (int (*)())(uintptr_t)infoPtr->src;

         argc = (*handlerProc)(infoPtr->dst, infoPtr->key,
                               argc, argv+srcIndex);
         if (argc < 0) {
            return TRUE;
         }
         break;
      }
      case ARGV_HELP:
         PrintUsage (argTable, flags);
         return TRUE;
      case ARGV_VERSION:
         PrintVersion(argTable);
         return FALSE;
      default:
         FPRINTF(stderr, "bad argument type %d in ArgvInfo",
                 infoPtr->type);
         return TRUE;
      }
   }
   
   /*
    * If we broke out of the loop because of an OPT_REST argument,
    * copy the remaining arguments down.
    */

 argsDone:
   while (argc) {
      argv[dstIndex] = argv[srcIndex];
      srcIndex++;
      dstIndex++;
      argc--;
   }
   argv[dstIndex] = (char *) NULL;
   *argcPtr = dstIndex;
   return FALSE;

 missingArg:
   FPRINTF(stderr, "\"%s\" option requires an additional argument\n", curArg);
   return TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * PrintUsage --
 *
 *	Generate a help string describing command-line options.
 *
 * argTable: Array of command-specific argument descriptions.
 *
 * flags: If the ARGV_NO_DEFAULTS bit is set in this word, then don't
 *        generate information for default options.
 *
 * Results:
 *	Prints on stderr (unless ARGV_NO_PRINT is specified in flags) 
 *	a help string describing all the options in argTable, plus all those
 *	in the default table unless ARGV_NO_DEFAULTS is
 *	specified in flags.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
PrintUsage(ArgvInfo *argTable, int flags)
{
   ArgvInfo *infoPtr;
   int width, i, numSpaces;
   intptr_t j, nargs;

/* Macro to optionally print errors */
#define FPRINTF if (!(flags&ARGV_NO_PRINT)) (void) fprintf

   /*
    * First, compute the width of the widest option key, so that we
    * can make everything line up.
    */

   width = 4;
   for (i = 0; i < 2; i++) {
      for (infoPtr = i ? defaultTable : argTable;
           infoPtr->type != ARGV_END; infoPtr++) {
         int length;
         if (infoPtr->key == NULL) {
            continue;
         }
         length = (int) strlen(infoPtr->key);
         if (length > width) {
            width = length;
         }
      }
   }

   FPRINTF(stderr, "Command-specific options:");
   for (i = 0; ; i++) {
      for (infoPtr = i ? defaultTable : argTable;
           infoPtr->type != ARGV_END; infoPtr++) {
         if (infoPtr->type == ARGV_VERINFO) {
            continue;
         }
         if ((infoPtr->type == ARGV_HELP) && (infoPtr->key == NULL)) {
            FPRINTF(stderr, "\n%s", infoPtr->help);
            continue;
         }
         FPRINTF(stderr, "\n %s:", infoPtr->key);
         /* Write out padding after the key, followed by the help text. 
          */
         numSpaces = width + 1 - strlen(infoPtr->key);
         FPRINTF(stderr, "%*s %s", numSpaces, "", infoPtr->help);
         switch (infoPtr->type) {
         case ARGV_INT: {
            FPRINTF(stderr, "\n\t\tDefault value:");
            nargs = (intptr_t) infoPtr->src;
            if (nargs<1) nargs=1;
            for (j=0; j<nargs; j++) {
               FPRINTF(stderr, " %d", *(((int *) infoPtr->dst)+j));
            }
            break;
         }
         case ARGV_FLOAT: {
            FPRINTF(stderr, "\n\t\tDefault value:");
            nargs = (intptr_t) infoPtr->src;
            if (nargs<1) nargs=1;
            for (j=0; j<nargs; j++) {
               FPRINTF(stderr, " %g", *(((double *) infoPtr->dst)+j));
            }
            break;
         }
         case ARGV_STRING: {
            char *string;

            nargs = (intptr_t) infoPtr->src;
            if (nargs<1) nargs=1;
            string = *((char **) infoPtr->dst);
            if ((nargs==1) && (string == NULL)) break;
            for (j=0; j<nargs; j++) {
               string = *(((char **) infoPtr->dst)+j);
               if (string != NULL) {
                  FPRINTF(stderr, " \"%s\"", string);
               }
               else {
                  FPRINTF(stderr, " <null>"); /* Don't print null strings. */
               }
            }

            break;
         }
         default: {
            break;
         }
         }
      }

      if ((flags & ARGV_NO_DEFAULTS) || (i > 0)) {
         break;
      }
      FPRINTF(stderr, "\nGeneric options for all commands:");
   }

   FPRINTF(stderr, "\n");
}

static void PrintVersion(ArgvInfo *argTable)
{
    const char *versionStr = MINC_VERSION;

    for ( ; argTable->type != ARGV_END; argTable++) {
        if (argTable->type == ARGV_VERINFO) {
            /* Version information found? */
            if (argTable->src != NULL) {
                versionStr = argTable->src;
                break;
            }
        }
    }
    printf("program: %s\n", versionStr);
#ifdef HAVE_MINC1
    {
      printf("libminc: %s\n", miget_version());
      printf("netcdf : %s\n", nc_inq_libvers());
    }
#endif
    
#ifdef HAVE_MINC2
    {
        unsigned int major, minor, release;
        H5get_libversion(&major, &minor, &release);
        printf("HDF5   : %d.%d.%d\n", major, minor, release);
    }
#endif
    exit(0);
}
