/*=========================================================================
  
  Program:   gdcm
  Module:    gdcmArgMgr.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
  
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
  
=========================================================================*/

#ifndef  ___ARG_MGR__
#define  ___ARG_MGR__

#include "gdcmCommon.h"
#include <stdio.h>  // for FILE
#include <stdlib.h> // For atof

namespace gdcm
{

#define ID_RFILE_TEXT        "rt"
#define ARGMAXCOUNT          100   // Maximum number of arguments
#define ARG_LONG_MAX         1000

// default file name
#define ARG_DEFAULT_PARAMOUT "fileout.par"
#define ARG_DEFAULT_LOGFILE  "gdcm.log"

#define ARG_LABEL_LOGFILE    "LOG"
#define ARG_LABEL_PARAMOUT   "paramout"

#define START_USAGE(usage)   const char *usage[] = {
#define FINISH_USAGE         0};

//-----------------------------------------------------------------------------
/**
 * \brief   class designed for command line arguments management
 *          (to make programmer's live easier) 
 *          NOT Dicom dependant (could be used for any kind 
 *                              of 'command line program')        
 */
 
class GDCM_EXPORT ArgMgr
{
public:
   ArgMgr(int argc, char **argv);
   ~ArgMgr();

   int    ArgMgrDefined           (const char *param);  // Checks if Param is defined
   char  *ArgMgrValue             (const char *param);  // Returns Param value 
   const char  *ArgMgrUnused      (void);               // Returns a never used arg.
   int    ArgMgrSave              (const char *);       // Save of parameters out
   int    ArgMgrUsage             (const char **usage); // Display program usage 
   int    ArgMgrPrintUnusedLabels (void);               // Prints unused labels

   int    ArgMgrGetInt   (const char *param, int);  // Gets an int   (with default value)
   float  ArgMgrGetFloat (const char *param, float);// Gets a float  (with default value)
   const char  *ArgMgrGetString(const char *param, const char* ext = 0);// Gets a string (with default value)
   int    ArgMgrGetLabel (const char *param, const char *, int);
  
   int    ArgMgrWantInt   (const char *param, const char **usage);  // Demands an int 
   float  ArgMgrWantFloat (const char *param, const char **usage);  // Demands a float
   char  *ArgMgrWantString(const char *param, const char **usage);  // Demands a string
   int    ArgMgrWantLabel (const char *param, char *, const char **usage);

   int   *ArgMgrGetListOfInt   (const char *param, int *);   // Gets a list of int 
   float *ArgMgrGetListOfFloat (const char *param, int *);   // Gets a list of float
   char **ArgMgrGetListOfString(const char *param, int *);   // Gets a list of string

   int        *ArgMgrGetIntEnum     (const char *param, int *); // Gets a list of int pairs
   uint16_t   *ArgMgrGetXInt16Enum  (const char *param, int *); // Gets a list of int16 pairs
   float      *ArgMgrGetFloatEnum   (const char *param, int *); // Gets a list of float pairs

private :

   int    FiltreLong       (const char *);
   const char  *LoadedParam(const char *, FILE *);
   int    ArgLoadFromFile  (const char *);
   void   ArgStdArgs       (void);

   // These ones are 'general purpose methods'
   char  *maj       (char *);
   char  *Majuscule (const char *);

   int       IdStrCountChar  (char *chaine,int caract);
   int      *IdStrIntEnum    (char *value, int *number);
   uint16_t *IdStrXInt16Enum (char *value, int *number);
   float    *IdStrFloatEnum  (char *value, int *number);

// --------------- Attributes ------------------------------

private :

   const char *ArgParamOut;    // Output File Name for param

   char *ArgUsed;              // Used Arguments 
   char *ArgLab[ARGMAXCOUNT];  // Arguments Labels
   char *ArgStr[ARGMAXCOUNT];  // Arguments 'strings'
   int   ArgCount;             // Number of arguments passed 
   char *Appel;                
};
} // end namespace gdcm

#endif
