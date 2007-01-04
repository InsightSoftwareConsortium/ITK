/*=========================================================================
  
  Program:   gdcm
  Module:    gdcmArgMgr.cxx
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

#include <stdio.h>
#include <iostream>
#include <ctype.h>
#include <string.h>  // For strlen

// No strcasecmp in WIN32 world, but stricmp
// http://www.opengroup.org/onlinepubs/007908799/xsh/strcasecmp.html
#ifdef _WIN32
#define strcasecmp stricmp
#endif

#include <string.h>  // For strtok and strlen
#include <stdlib.h>  // For strtol and strtod

#include "gdcmArgMgr.h"

namespace gdcm 
{
//-------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \brief   constructor
 * @param argc arguments count, as passed to main()
 * @param argv  pointers array on the arguments passed to main()  
 */
 ArgMgr::ArgMgr(int argc, char **argv)
 {
   int i;
   size_t nblettre;
   ArgUsed = NULL;
   Appel   = NULL;
  
   /* Read the parameters of the command line *************************/
   for ( ArgCount=0, nblettre=1 , i=0; i<argc; i++) 
   {
      if ( FiltreLong(argv[i]) ) 
      { 
          std::cout << "Argument too long ( > "
                    << ARG_LONG_MAX << ")" << std::endl;
          return;
      }
      if ( argv[i][0] == '@' )
      {                       
         nblettre  += ArgLoadFromFile ( &argv[i][1] );   
      }
      else
      {                                         
         ArgLab [ArgCount] = strcpy ( (char *)malloc(strlen(argv[i])+1), argv[i] ) ;
         nblettre  += 1 + strlen(ArgLab[ArgCount]);     
         ArgCount++;                               
      }
      if (ArgCount >= ARGMAXCOUNT )      
      {
          std::cout << "Too many Arguments ( more than "
                    << ARGMAXCOUNT << ")" << std::endl; 
          return;
      }
   }

   /* Fills an array with the already used parameters ****/
   ArgUsed = (char *)calloc (1, ArgCount );

   /* Builds the full string with all the parameters  **************/
   Appel = (char *) calloc (1, nblettre );

   for ( *Appel = '\0', i=0; i<ArgCount; i++)
   {
      strcat ( Appel, ArgLab [i] ) ;
      strcat ( Appel, " " ) ;
   }

   /* Splitting label from label value *************************************/
   for ( i=0; i<ArgCount; i++) 
   {
      char * egaloufin = ArgLab[i] ;
      while ( (*egaloufin != '\0') && (*egaloufin != '=') ) 
         egaloufin ++ ;
      if ( *egaloufin ) *(egaloufin++) = '\0';
      ArgStr[i]= egaloufin;
   }

   /* Set labels to upper-case (labels are not case sensitive ) *********/
   for ( i=0; i<ArgCount; i++)
      ArgLab[i] = Majuscule ( ArgLab[i] ) ;

  /* Standard arguments are managed by ArgStdArgs **********************/
   ArgStdArgs(); 
 }

/**
 * \brief  canonical destructor
 */
ArgMgr::~ArgMgr()
{
   for(int i=0;i<ArgCount;i++)
      if ( ArgLab[i] )
         free(ArgLab[i]);
   if ( ArgUsed )
      free(ArgUsed);
   if ( Appel )
      free(Appel);
}
 
/**
 * \brief  checks if a parameter exists in the command line
 * @param param  label name
 * @return   0 if label is not found
 *           else, returns the number of the spot it was found last time.
 */
int ArgMgr::ArgMgrDefined( const char *param )
{
  int i;
  bool trouve;
  char *temp;
  temp = Majuscule ( param ) ;
  for ( i = ArgCount-1; i>0; i-- )
  { 
    trouve = ( strcmp( ArgLab[i], temp )==0 ) ;
    if ( trouve )
    {
      ArgUsed[i] = true ;           
      for ( int j=1; j<i; j++)
      {                     
         if ( (!ArgUsed[j])&&(!strcmp(ArgLab[i],ArgLab[j])) )
            ArgUsed[j] = i ;
      }
      return i ;
    }
  }
  return 0 ;
}

/**
 * \brief  Gets the parameter value, read on the command line
 * @param param   name of the searched parameter label
 * @return   Value, as a char array, of the parameter
 *            whose label is given.
 */
char *ArgMgr::ArgMgrValue ( const char *param )
{
   int trouve ;
   if ( (trouve = ArgMgrDefined ( param )) != false )
      return ArgStr[trouve] ;
   else
      return NULL ;
}

/**
 * \brief  Search for the first not yet used label
 * @return Pointer to the char array holding the first non used label
 */
const char *ArgMgr::ArgMgrUnused ( )
{
   int i ;
   for ( i=ArgCount-1; i>0; i-- )
   {
      if ( ! ArgUsed[i] )
      {
         ArgMgrDefined(ArgLab[i]);
         return ArgLab[i] ;
      }
  }
  return NULL ;
}

/**
 * \brief  Prints unused labels, if any
 * @return number of unused labels
 */
int ArgMgr::ArgMgrPrintUnusedLabels ()
{
   const char *label;
   int i=0;
   while ( (label=ArgMgrUnused())!=0 )
   {
      if (i==0)
         std::cout << "\n Unused Labels:" << std::endl
                   << "=============="    << std::endl;
      std::cout << "Label : " << label << " = " 
                << ArgMgrValue(label) << std::endl;
      i++;
   }
   return i;
}

/**
 * \brief  Prints program usage
 * @param usage  array of pointers to the documentation lines of the program.
 * @return exception code
 */
int ArgMgr::ArgMgrUsage(const char **usage )
{
   while ( *usage ) 
      std::cout << std::endl << *(usage++);
   std::cout << std::endl; 
   return (0);
}

/**
 * \brief Forget it, right now ... 
 * Saves a char. array in a parameter file
 *         whose name is given on command line by : PARAMOUT=???
 *         or, as a default, by ARG_DEFAULT_PARAMOUT
 * @param param  char. array that defines the parameter
 * @return   Entier correspondant au rang dans la liste de labels
 */
int ArgMgr::ArgMgrSave ( const char *param )
{
   static int   deja = 0;
   FILE         *fd;
   if ( *ArgParamOut == '\0' )
      return 0;
   if ( deja ) 
   {
      fd = fopen ( ArgParamOut, "a+" );
   }
   else
   {
      deja = 1;
      fd = fopen ( ArgParamOut, "w" );
   } 
   if ( !fd ) 
      return 0;
   fprintf ( fd, "%s\n", param );
   fclose  ( fd );
   return 1;
}

/**
 * \brief  Gets an int value passed as an argument to a program
 *         (use default value if not found)
 *         EXAMPLE:     int dimx = ArgMgrGetInt ( "DIMX", 256 );
 * @param label   label name 
 * @param defaultVal default value
 * @return parameter value
 */
int ArgMgr::ArgMgrGetInt(const char *label, int defaultVal)
{
   return ( (ArgMgrDefined(label))
            ? (atoi(ArgMgrValue(label)))
            : (defaultVal) );
}

/**
 * \brief  Gets a float value passed as an argument to a program
 *         (use default value if not found)
 *         EXAMPLE:     float scale = ArgMgrGetFloat ( "SCALE", 0.33 );
 * @param param   label name 
 * @param defaultVal default value
 * @return parameter value
 */
float ArgMgr::ArgMgrGetFloat(const char *param, float defaultVal)
{
   return     ( (ArgMgrDefined(param))
               ? ((float)atof(ArgMgrValue(param)))
               : (defaultVal) );
}

/**
 * \brief  Gets a 'string' value passed as an argument to a program
 *         (use default value if not found)
 *         EXAMPLE :  char *imageName = ArgMgrGetString( "NAME", "test.dcm" );
 * @param param   label name 
 * @param defaultVal default value
 * @return parameter value
 */
const char *ArgMgr::ArgMgrGetString(const char *param, const char *defaultVal)
{
   return    ( (ArgMgrDefined(param)) 
              ? (ArgMgrValue(param))
              : (defaultVal) );
}

/**
 * \brief  Gets a value amongst a set of values
 *         (use default value if not found) 
 *         EXAMPLE:     int nlab = ArgMgrGetLabel("CONFIRM","NO\\YES", 0); 
 * @param param   label name 
 * @param liste  character Chain describing the various values.
 *               Value are separated by '\\'.
 *               Not case sensitive.
 * @param val  number of default value
 * @return   int : range of value amongst the values list
 */
int ArgMgr::ArgMgrGetLabel (const char *param, const char *liste, int val )
{
  char *lab;
  const char *vallab;
  int i = 1;
  char *tmp;
  tmp = (char *) malloc(strlen(liste)+1);
  strcpy(tmp,liste);

  if ( (vallab = ArgMgrGetString(param,(const char *)NULL)) != 0 ) 
  { 
     for ( lab = strtok (tmp,"\\"); 
           lab != 0; 
           lab = strtok(0L,"\\"), i++ )
     { 
        // strcmp ignoring case
        if( strcasecmp(lab, vallab) == 0)
           return i;
     } 
     val=0;
   }
   free(tmp);
   return val;
}

/**
 * \brief  Demands a value amongst a set of values (abort if not found)
 *         EXaMPLE:     int nlab = ArgMgrWantLabel("CONFIRM","NO\\YES", usage); 
 * @param param   label name 
 * @param liste  character Chain describing the various values.
 *               Labels are separated by  '\\'.
 *               No case sensitive.
 *               WARNING this will be changed (not const)
 * @param usage Usage program (displayed if label not found)
 * @return   int : range of value amongst the values list
 */
int ArgMgr::ArgMgrWantLabel (const char *param, char *liste, const char **usage )
{
   char *lab;
   const char *vallab;
   int i = 1;
   if ( (vallab = ArgMgrGetString(param,0)) != 0 ) 
   {
      for ( lab = strtok (liste,"\\"); lab != 0; lab = strtok(0L,"\\"), i++ )
        if ( strcasecmp(lab,vallab)==0) 
           return i;
      return 0;
   }
   ArgMgrUsage(usage);
   return 0;
}

/**
 * \brief  Demands an int value passed as an argument to a program
 *         If not found usage is displayed and the prog aborted
 *  EXAMPLE:     int dimx = ArgMgrWantInt ( "DIMX", usage );
 * @param label   label name 
 * @param usage Usage program (displayed if label not found)
 * @return parameter value
 */
int ArgMgr::ArgMgrWantInt (const char *label, const char **usage)
{
   return        ( (ArgMgrDefined(label) ) 
                 ? (atoi(ArgMgrValue(label) ) ) 
                 : (ArgMgrUsage(usage),1) );
}

/**
 * \brief  Demands a float value passed as an argument to a program
 *         If not found usage is displayed and the prog aborted
 *  EXAMPLE:     float scale = ArgMgrWantFloat ( "SCALE", usage );
 * @param label   label name 
 * @param usage Usage program (displayed if label not found)
 * @return parameter value
 */
float ArgMgr::ArgMgrWantFloat (const char *label, const char **usage)
{
   return       ( (ArgMgrDefined(label) ) 
                ? ((float)atof(ArgMgrValue(label) ) ) 
                : (ArgMgrUsage(usage),(float)1.0) );
}

/**
 * \brief  Demands a 'string' value passed as an argument to a program
 *         If not found usage is displayed and the prog aborted
 *  EXAMPLE:     char *code = ArgMgrWantString ( "CODE", usage );
 * @param label   Parameter label
 * @param usage Usage program (displayed if label not found)
 * @return parameter value
 */
char *ArgMgr::ArgMgrWantString(const char *label, const char **usage)
{
   return      ( (ArgMgrDefined(label) ) 
               ? (ArgMgrValue(label) ) 
               : (ArgMgrUsage(usage),(char*)0) );
}

/**
 * \brief  decodes and returns an array of 'STRING'
 *  EXAMPLE:     char **codes = ArgMgrGetListOfString ( "CODES", &nbOfCodes ); 
 * @param label   label name 
 * @param number  nb of found 'STRINGs'
 * @return   Pointer to the 'STRING' array; NULL if error
 */
char **ArgMgr::ArgMgrGetListOfString ( const char *label, int *number )
{
  int taille;
  char  *value = ArgMgrValue(label);
  char **liste;
  char **elem;
  char  *chainecur; 
  if (!value)
  {
     *number = 0;
     return 0;
  }
  *number = IdStrCountChar(value,',')+1; /* nb Elements = nb Commas +1 */
  taille = *number;
  liste = (char **) malloc (sizeof(char*) * taille + strlen(value)+1);
  if ( !liste )
     return 0;
  value = strcpy( ((char*)liste)+sizeof(char*) * taille, value );
  for ( elem = liste, chainecur = strtok(value,", ");
        taille>0;
        taille--, chainecur = (chainecur) ? strtok ( 0, ", " ) : 0 )
  {
    *(elem++) = chainecur;
  }
  return liste;
}

/**
 * \brief  decodes and returns an array of 'INT'
 *  EXAMPLE:     int *points = ArgMgrGetListOfInt ( "POINTS", &nbOfPoints );  
 * @param label   label name 
 * @param number  nb of found INT
 * @return   Pointer to the INT array; NULL if error
 */
int *ArgMgr::ArgMgrGetListOfInt ( const char *label, int *number )
{
  char *value = ArgMgrValue(label);
  int *liste;
  int *elem;
  int taille;
  if (!value)
  {
     *number = 0;
     return 0;
  }
  *number = IdStrCountChar(value,',')+1; /* nb Elements = nb Commas +1 */
  taille= *number;
  liste = (int *) calloc (1,sizeof(int)*taille );
  if ( !liste )
     return 0;
  elem = liste;
  //*number = 1;

  while ( taille>0 ) 
  {
    *(elem++) = (int) strtol ( value, &value, 10 );      
    if ( *value == '\0' )
       return liste;
    if ( *(value++) != ',' ) 
    {
      free (liste);
      return 0;
    }
    taille --;
  }
return liste;
}

/**
 * \brief  decodes and returns an array of 'FLOAT'
 * @param label   label name 
 * @param number  number of found FLOATs
 * @return   Pointer to the FLOAT array; NULL if error
 */
float *ArgMgr::ArgMgrGetListOfFloat ( const char *label, int *number )
{
  char *value = ArgMgrValue(label);
  float *liste;
  float *elem;
  int taille;
  if (!value)
    return 0;
  *number = IdStrCountChar(value,',')+1; /* nb Elements = nb Commas +1 */
  taille= *number;
  liste = (float *) calloc (1,sizeof(float)*taille );
  if ( !liste )
  {
     *number = 0;
     return 0;
  }
  elem = liste;
  //*number = 1;

  while ( taille>0 ) 
  {
    *(elem++) = (float) strtod ( value, &value );      
    if ( *value == '\0' )
       return liste;
    if ( *(value++) != ',' )
    {
      free (liste);
      return 0;
    }
    taille --;
  }
return liste;
}

/**
 * \brief  decodes and returns an array of 'INT pairs', passed in decimal
 * @param param   label name 
 * @param number   nb of found pairs
 * @return        pointer to the array of 'INT pairs'; NULL if fail
 */
int *ArgMgr::ArgMgrGetIntEnum ( const char *param, int *number )
{
   char *value = ArgMgrValue(param);
   int *liste;
   if (!value) 
   {
      *number = 0; 
      return 0;
   }
   liste = IdStrIntEnum(value, number);
   return liste;
}

/**
 * \brief  decodes and returns an array of 'INT16 pairs', passed in hexadecimal
 * @param param   label name 
 * @param number   nb of found pairs
 * @return        pointer to the array of 'INT16 pairs'; NULL if fail
 */
uint16_t *ArgMgr::ArgMgrGetXInt16Enum ( const char *param, int *number )
{
   char *value = ArgMgrValue(param);
   uint16_t *liste;
   if (!value) 
   {
      *number = 0; 
      return 0;
   }
   liste = IdStrXInt16Enum(value, number);
   return liste;
}
/**
 * \brief  decodes and returns an array of 'FLOAT pairs'
 * @param param   label name 
 * @param number   nb of found pairs
 * @return        pointer to the array of 'FLOAT pairs'; NULL if fail

 */
float *ArgMgr::ArgMgrGetFloatEnum ( const char *param, int *number )
{
   char  *value = ArgMgrValue(param);
   float *liste;
   if (!value) 
   {
      *number = 0; 
      return 0;
   }
   liste = IdStrFloatEnum(value, number);
   return liste;
}

// ------------------------ Those are 'service functions' ---------------------
// ------------------------       internal use only       ---------------------

/**
 * \brief     Counts the nb of occurrences of a given charact within a 'string' 
 * @param chaine     Pointer to the 'string'
 * @param caract     charact to count
 * @return       occurence number
 */
int ArgMgr::IdStrCountChar (char *chaine, int caract)
{
  int i=0;
  char *ptr;
  for ( ptr = chaine ; *ptr!='\0' ; ptr ++ ) 
     if (*ptr==caract) 
        i++;  
  return i;
}

/**
 * \brief     returns an array of 'INT pairs'
 * @param value  char array decribing a set of 'INT pairs' (f1-l1, f2-l2, ...)
 * @param number nb of found INT pairs
 * @return       pointer to the array of 'INT pairs'
 */
int *ArgMgr::IdStrIntEnum ( char* value, int *number)
{
   int* liste;
   int taille;
   int i;

   *number = IdStrCountChar(value,',')+1; /* nb Elements = nb Commas +1 */
   taille= *number;
   liste = (int *) calloc (1,sizeof(int)*2*taille );
   if ( !liste )
   {
      return 0;
   }
   i=0;
   while ( taille>0 ) 
   {
      liste[i] = (int) strtol ( value, &value, 10 );
      if ( *value == '\0' ) 
      {
         liste[i+1]=liste[i];
         return liste;
      }
      if ( *(value++) != '-' ) 
      {
         liste[i+1]=liste[i];
         value--;
       }
       else
       {
          liste[i+1] = (int) strtol ( value, &value, 10 );
       }
       if ( *value == '\0' )
          return liste;
       if ( *(value++) != ',' )
       {
          free (liste);
          return 0;
       }
       taille --; i+=2;
   }
   return liste;
}

/**
 * \brief     returns an array of set of 'INT16 pairs', passed in Hexadecimal
 * @param value  char array decribing a set of 'INT16 pairs' (f1-l1, f2-l2, ...)
 *               coded in hexadecimal e.g. 0x0008,0x00ac
 * @param number nb of found pairs
 * @return        array of set of 'INT16 pairs'
 */
uint16_t *ArgMgr::IdStrXInt16Enum ( char *value, int *number)
{
   uint16_t *liste;
   int taille;
   int i;

   *number = IdStrCountChar(value,',')+1; /* nb Elements = nb Commas +1 */
   taille= *number;
   liste = (uint16_t *) calloc (1,sizeof(uint16_t)*2*taille );
   if ( !liste )
   {
      return 0;
   }
   i=0;
   while ( taille>0 ) 
   {
      liste[i] = (uint16_t) strtol ( value, &value, 16 );
      if ( *value == '\0' ) 
      {
         liste[i+1]=liste[i];
         return liste;
      }
      if ( *(value++) != '-' ) 
      {
         liste[i+1]=liste[i];
         value--;
       }
       else
       {
          liste[i+1] = (uint16_t) strtol ( value, &value, 16 );
       }
       if ( *value == '\0' )
          return liste;
       if ( *(value++) != ',' )
       {
          free (liste);
          return 0;
       }
       taille --; i+=2;
   }
   return liste;
} 
/**
 * \brief     returns an array of 'FLOAT pairs'
 * @param value  char array decribing a set of 'FLOAT pairs' (f1-l1, f2-l2, ...)
 * @param number nb of found pairs
 * @return       pointer to the array of 'FLOAT pairs'; NULL if fail
 */
float *ArgMgr::IdStrFloatEnum (char *value, int *number)
{
   float *liste;
   int taille;
   int i;
   *number = IdStrCountChar(value,',')+1; /* nb Elements = nb Commas +1 */
   taille= *number;
   liste = (float *) calloc (1,sizeof(float)*2*taille );
   if ( !liste )
      return 0;
   i=0;
   while ( taille>0 ) 
   {
      liste[i] = (float) strtod ( value, &value );      
      if ( *value == '\0' ) 
      {
         liste[i+1]=liste[i];
         return liste;
      }
      if ( *(value++) != '-' ) 
      {
         liste[i+1]=liste[i];
         value--;
      }
      else
      {
          liste[i+1] = (float) strtod ( value, &value );
      }
      if ( *value == '\0' ) 
         return liste;
      if ( *(value++) != ',' )
      {
         free (liste);
         return 0;
      }
      taille --; i+=2;
   }
   return liste;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

/**************************************************************************
*                                                                         *
* Nom de la fonction : Majuscule                                          *
* Role ............. : Creates a new Upper case char array.               *
* parameters ....... : Pointer to the initial char array.                 *                           *
* Valeur retournee . : Pointer to the new Upper case char array.          *
*                                                                         *
**************************************************************************/
char *ArgMgr::Majuscule (const char *chaine )
{
  char *ptr, *ptr2, *ptr3;
  ptr2 = (char *)malloc(strlen(chaine)*sizeof(char)+1);
  ptr3=ptr2;
  for ( ptr = (char *)chaine ; *ptr!='\0' ; ptr ++ ) 
   {  
       *ptr3 = toupper ( * ptr ); ptr3++; 
   }
  *ptr3='\0'; 
  return ptr2;
}

/**************************************************************************
*                                                                         *
* Nom de la fonction : FiltreLong                                         *
* Role ............. : Stops the program if argument is too long.         *
*                      ARG_LONG_MAX defines max length.                   *
* parameters ....... : Pointer to the argument.                           *
* Valeur retournee . : false if OK.                                       *
*                      true if KO.                                        *
**************************************************************************/
int ArgMgr::FiltreLong ( const char *arg  )
{
  int  n = 0 ;
  while ( (n++<ARG_LONG_MAX) && (*(arg++) != '\0') ) ;
  return (n>=ARG_LONG_MAX) ;
}

/*------------------------------------------------------------------------
 | Role       : Reads a parameter from a file
 | Return     : Type   : char *
 |              Role   : pointer to the label
 | parameters : param  : char *
 |              Role   : one where the parameter will be stored
 |              fd     : FILE *
 |              Role   : File description (assumed to be open)
 +------------------------------------------------------------------------*/
const char *ArgMgr::LoadedParam ( const char *param, FILE *fd )
{
  int    carlu;
  char  *car = (char *)param;
  int    quote = false;
  int    nbcar = 0;

  /* remove spaces at the beginning****/
  while ( isspace(carlu=fgetc (fd)) );
  if (carlu==EOF)
     return 0;
  /* Search for a " */
  if ( carlu=='\"' ) 
  {
    carlu=fgetc(fd);
    quote=true;
  /* Read all the characters */
  }
  while (  (carlu!=EOF)
        && (  ( (!quote)&&(!isspace(carlu)) )
         ||( (quote)&& !(carlu=='\"')   ) ) ) 
  {
     *(car++) = (char) carlu;
     nbcar ++;
  /* sans depasser la taille max*/
     if ( nbcar >= ARG_LONG_MAX ) 
     {
        std::cout << "\nError: Argument too long ( > "
                  << ARG_LONG_MAX << ")in parameter file."
                  << std::endl;
        break;
     }
     carlu = fgetc(fd);
  }
  *car = '\0';
  return param;
}

/*------------------------------------------------------------------------
 | Role       : Reading of arguments in a parameter file
 |              (this function is recursive).
 | Return     : Type   : int
 |              Role   : length needed to store all the parameters
 | parameters : filename : char *
 |              Role     : parameter File name
 |
 +------------------------------------------------------------------------*/
int ArgMgr::ArgLoadFromFile ( const char *filename )
{
  size_t   nbl = 0;
  char  param[ARG_LONG_MAX+1];
  FILE  *fch;

  fch = fopen ( filename, ID_RFILE_TEXT );
  while ( LoadedParam (param, fch ) )
  {
    size_t n = strlen(param);
    if ( param[0]=='@' )
    {
      nbl  += ArgLoadFromFile ( &param[1] );
    }
    else
    {
      ArgLab [ArgCount] = strcpy ((char *) malloc(n+1), param ) ;
      nbl += n + 1 ;
      ArgCount++;
      if ( ArgCount >= ARGMAXCOUNT ) 
         break;
    }
  }
  fclose ( fch );
  return static_cast< int >( nbl );
}

/*------------------------------------------------------------------------
 | Role       : Standard parameters management (on command line)
 | Return     : Type   : void
 | parameters : none
 +------------------------------------------------------------------------*/
void ArgMgr::ArgStdArgs()
{
  char *logfile;
  FILE *fd;

  if ( (ArgParamOut=ArgMgrValue((char*)ARG_LABEL_PARAMOUT))==0 )
    ArgParamOut = ARG_DEFAULT_PARAMOUT;
  if ( (logfile = ArgMgrValue((char*)ARG_LABEL_LOGFILE))!=0) 
  {
    if ( *logfile == '\0' )
       logfile = (char *)ARG_DEFAULT_LOGFILE;
    fd = fopen ( logfile, "a+" );
    if ( fd ) 
    {
      fprintf ( fd, "%s\n", Appel );
      fclose  ( fd );
    }
  }
}

/*------------------------------------------------------------------------
 | Role       : Sets in Upper Case.
 | Return     : Type   : char *
 | parameters : char *
 +------------------------------------------------------------------------*/
char *ArgMgr::maj ( char *a )
{
   char *b = a;
   while ( *b !=0 ) 
   {
      if ( *b<='z' && *b>='a' ) *b = *b+'A'-'a';
      b++;
   }
   return a;
}
//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
