/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc2_error.c
@DESCRIPTION: File containing routines to do error handling for MINC package.
              Should be called through macros in minc_private.h
@GLOBALS    :
@CALLS      :
@CREATED    : August 7, 1992 (Peter Neelin)
@MODIFIED   :
 * $Log: minc2_error.c,v $
 * Revision 6.8  2009-01-20 11:58:13  rotor
 *  * CMakeLists.txt: updated version
 *  * Updated Changelog to include releases
 *  * Warning cleanups below
 *  * conversion/dcm2mnc/minc_file.c: fixed printf type
 *  * conversion/dcm2mnc/siemens_to_dicom.c: fixed printf type
 *  * conversion/ecattominc/machine_indep.c: added string.h and fixed
 *      2 fprintf missing format args
 *  * conversion/micropet/upet2mnc.c: fixed two fprintf format args
 *  * conversion/minctoecat/ecat_write.c: added string.h
 *  * conversion/minctoecat/minctoecat.c: added missing argument to fprintf
 *  * conversion/nifti1/mnc2nii.c: fixed incorrect printf type
 *  * progs/mincview/invert_raw_image.c: added fwrite checking
 *
 * Revision 6.7  2008/04/11 05:15:00  rotor
 *  * rewrote error code  (Claude) to remove global defs that were
 *     causing build problems with DYLIB on OSX
 *
 * Revision 6.6  2008/01/17 02:33:02  rotor
 *  * removed all rcsids
 *  * removed a bunch of ^L's that somehow crept in
 *  * removed old (and outdated) BUGS file
 *
 * Revision 6.5  2008/01/12 19:08:14  stever
 * Add __attribute__ ((unused)) to all rcsid variables.
 *
 * Revision 6.4  2004/10/15 13:46:15  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.3  2004/04/27 15:47:25  bert
 * Move most message text into this file
 *
 * Revision 6.2  2001/04/17 18:40:13  neelin
 * Modifications to work with NetCDF 3.x
 * In particular, changed NC_LONG to NC_INT (and corresponding longs to ints).
 * Changed NC_UNSPECIFIED to NC_NAT.
 * A few fixes to the configure script.
 *
 * Revision 6.1  1999/10/19 14:45:09  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:24:54  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:25:53  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:07:52  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:38:04  neelin
 * Release of minc version 0.2
 *
 * Revision 1.7  94/09/28  10:37:16  neelin
 * Pre-release
 *
 * Revision 1.6  93/08/11  12:06:24  neelin
 * Added RCS logging in source.
 *
@COPYRIGHT  :
              Copyright 1993 Peter Neelin, McConnell Brain Imaging Centre,
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif //HAVE_CONFIG_H

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "minc2.h"
#include "minc2_private.h"

struct mierror_entry {
  int level;
  char *msgfmt;
};

/* MINC routine name variable, call depth counter (for keeping track of
   minc routines calling minc routines) and variable for keeping track
   of callers ncopts. All of these are for error logging. */
static char *minc_routine_name = "MINC2";
static int minc_call_depth = 0;
static int minc_trash_var = 0;

static struct mierror_entry mierror_table[] = {
  { MI2_MSG_ERROR, "Cannot uncompress the file" }, /* MI2_MSG_UNCMPFAIL */
  { MI2_MSG_ERROR, "Can't write compressed file" }, /* MI2_MSG_NOWRITECMP */
  { MI2_MSG_ERROR, "Unable to open file '%s'" }, /* MI2_MSG_OPENFILE */
  { MI2_MSG_ERROR, "Unable to create file '%s'"}, /* MI2_MSG_CREATEFILE */
  { MI2_MSG_ERROR, "Error closing file"}, /* MI2_MSG_CLOSEFILE */
  { MI2_MSG_WARNING, "Attribute '%s' not found"}, /* MI2_MSG_FINDATTR */
  { MI2_MSG_ERROR, "Attribute '%s' is non-numeric"}, /* MI2_MSG_ATTRNOTNUM */
  { MI2_MSG_ERROR, "Can't read attribute '%s'"}, /* MI2_MSG_READATTR */
  { MI2_MSG_FATAL, "No memory for attribute '%s'"}, /* MI2_MSG_NOMEMATTR */
  { MI2_MSG_ERROR, "Conversion error for attribute '%s'"}, /* MI2_MSG_CONVATTR */
  { MI2_MSG_ERROR, "Attribute '%s' is not a scalar"}, /* MI2_MSG_ATTRNOTSCALAR */
  { MI2_MSG_ERROR, "Attribute '%s' is not a string"}, /* MI2_MSG_ATTRNOTSTR */
  { MI2_MSG_ERROR, "Can't write attribute '%s'"}, /* MI2_MSG_WRITEATTR */
  { MI2_MSG_ERROR, "Can't read variable ID# %d"}, /* MI2_MSG_READVAR */
  { MI2_MSG_ERROR, "Can't write variable ID# %d"}, /* MI2_MSG_WRITEVAR */
  { MI2_MSG_ERROR, "Can't find variable ID# %d"}, /* MI2_MSG_FINDVAR */
  { MI2_MSG_ERROR, "Can't read attribute count"}, /* MI2_MSG_ATTRCOUNT */
  { MI2_MSG_ERROR, "Can't read attribute name"}, /* MI2_MSG_ATTRNAME */
  { MI2_MSG_ERROR, "Can't copy attribute '%s'"}, /* MI2_MSG_COPYATTR */
  { MI2_MSG_ERROR, "Can't read variable information"}, /* MI2_MSG_VARINQ */
  { MI2_MSG_ERROR, "Can't get unlimited dimension"}, /* MI2_MSG_UNLIMDIM */
  { MI2_MSG_ERROR, "Can't get dimension information"}, /* MI2_MSG_DIMINQ */
  { MI2_MSG_ERROR, "Variable already defined with different size"}, /* MI2_MSG_VARCONFLICT */
  { MI2_MSG_ERROR, "Can't define dimension '%s'"}, /* MI2_MSG_DIMDEF */
  { MI2_MSG_ERROR, "Can't define variable '%s'"}, /* MI2_MSG_VARDEF */
  { MI2_MSG_ERROR, "Variables do not match for value copy"}, /* MI2_MSG_VARMISMATCH */
  { MI2_MSG_ERROR, "Variables have dimensions of different size"}, /* MI2_MSG_VARDIFFSIZE */
  { MI2_MSG_ERROR, "Can't read variable count"}, /* MI2_MSG_VARCOUNT */
  { MI2_MSG_ERROR, "Variable '%s' not copied"}, /* MI2_MSG_OUTPUTVAR */
  { MI2_MSG_ERROR, "Error copying variable"}, /* MI2_MSG_COPYVAR */
  { MI2_MSG_ERROR, "Non-numeric datatype"}, /* MI2_MSG_VARNOTNUM */
  { MI2_MSG_FATAL, "Can't allocate %d bytes"}, /* MI2_MSG_OUTOFMEM */
  { MI2_MSG_ERROR, "Attribute '%s' is not a pointer"}, /* MI2_MSG_ATTRNOTPTR */
  { MI2_MSG_ERROR, "Variable '%s' is not a standard MINC variable"}, /* MI2_MSG_VARNOTSTD */
  { MI2_MSG_ERROR, "Bad dimension width suffix"}, /* MI2_MSG_DIMWIDTH */
  { MI2_MSG_ERROR, "Imagemax/min dimensions vary over image dimensions"}, /* MI2_MSG_MAXMINVARY */
  { MI2_MSG_FATAL, "Should not happen!"}, /* MI2_MSG_SNH */
  { MI2_MSG_FATAL, "Unknown integer size %d"}, /* MI2_MSG_INTSIZE */
  { MI2_MSG_FATAL, "Unknown float size %d"}, /* MI2_MSG_FLTSIZE */
  { MI2_MSG_FATAL, "Unknown type class %d"}, /* MI2_MSG_TYPECLASS */
  { MI2_MSG_ERROR, "Function '%s' not implemented"}, /* MI2_MSG_NOTIMPL */
  { MI2_MSG_FATAL, "Unknown type %d"}, /* MI2_MSG_BADTYPE */
  { MI2_MSG_ERROR, "Can't open dataset %s"}, /* MI2_MSG_OPENDSET */
  { MI2_MSG_ERROR, "Can't read dataset %s"}, /* MI2_MSG_READDSET */
  { MI2_MSG_ERROR, "Can't write dataset %s"}, /* MI2_MSG_WRITEDSET */
  { MI2_MSG_ERROR, "Can't use more than %d dimensions"}, /* MI2_MSG_TOOMANYDIMS */
  { MI2_MSG_ERROR, "Attempt to modify an attached image conversion variable"}, /* MI2_MSG_ICVATTACHED */
  { MI2_MSG_ERROR, "Illegal ICV identifier"}, /* MI2_MSG_BADICV */
  { MI2_MSG_ERROR, "Error setting ICV property: %s"}, /* MI2_MSG_BADPROP */
  { MI2_MSG_ERROR, "ICV is not attached"}, /* MI2_MSG_ICVNOTATTACHED */
  { MI2_MSG_ERROR, "Invalid ICV coordinates"}, /* MI2_MSG_ICVCOORDS */
  { MI2_MSG_ERROR, "Illegal variable access operation" }, /* MI2_MSG_BADOP */
  { MI2_MSG_ERROR, "HDF5 function %s failed" } , /*MI2_MSG_HDF5*/
  { MI2_MSG_ERROR, "Error: %s"} , /*MI2_MSG_GENERIC*/
};

int MI2_save_routine_name ( char *name )
{
  /* no idea what peter was up to here */
  /* minc_trash_var = (((minc_call_depth++)==0) ? MI_save_routine_name(name) : * MI_NOERROR)) */

  if ( ( minc_call_depth++ ) == 0 ) {
    minc_routine_name = name;
    minc_trash_var = TRUE;
  } else {
    minc_trash_var = MI_NOERROR;
  }
  return ( TRUE );
}

int MI2_return ( void )
{
  /* no idea what peter was up to here */
  /* return( (((--minc_call_depth)!=0) || MI_return()) ? (value) : (value)) */

  return ( ( ( --minc_call_depth ) != 0 ) || TRUE );
}

int MI2_return_error ( void )
{
  /* no idea what peter was up to here */
  /* return( (((--minc_call_depth)!=0) || MI_return_error()) ? (error) : (error)) */

  if ( ( --minc_call_depth ) == 0 ) {
    MI2_log_pkg_error2 ( 0, "MINC package entry point" );
  }
  return ( TRUE );
}

void MI2_log_pkg_error2 ( int p1, char *p2 )
{
  ( void ) fprintf ( stderr, "%s: ", minc_routine_name );
  ( void ) fprintf ( stderr, "%s", p2 );
  ( void ) fputc ( '\n', stderr );
  ( void ) fflush ( stderr );
}

void MI2_log_pkg_error3 ( int p1, char *p2, char *p3 )
{
  ( void ) fprintf ( stderr, "%s: ", minc_routine_name );
  ( void ) fprintf ( stderr, p2, p3 );
  ( void ) fputc ( '\n', stderr );
  ( void ) fflush ( stderr );
}

void MI2_log_sys_error1 ( char *p1 )
{
  char *message;
  int errnum = errno;

  ( void ) fprintf ( stderr, "%s", minc_routine_name );
  ( void ) fprintf ( stderr, "%s", p1 );
  if ( errnum == 0 ) {
    ( void ) fputc ( '\n', stderr );
  } else {
    message = strerror ( errnum );
    if ( message == NULL ) message = "Unknown error";
    ( void ) fprintf ( stderr, ": %s\n", message );
  }
  ( void ) fflush ( stderr );
}

/* By default, print all messages of severity error, or worse.
 */
static struct {
  int level;
  char prog[128];
  FILE *fp;
} _MI2_log = {
  MI2_MSG_ERROR, {""}, NULL
};

/** Simple function to read a user's .mincrc file, if present.
 */
static int mi2read_cfg(const char *name, char *buffer, int maxlen)
{
    FILE *fp;
    int result = 0;
    char *home_ptr = getenv("HOME");
    char path[256];

    if (home_ptr != NULL) {
      strcpy(path, home_ptr);
    }
    else {
      path[0] = '\0';
    }
    strcat(path, "/.mincrc");
    
    if ((fp = fopen(path, "r")) != NULL) {
        while (fgets(buffer, maxlen, fp)) {
            if (buffer[0] == '#') {
                continue;
            }
            if (!strncasecmp(buffer, name, strlen(name))) {
                char *tmp = strchr(buffer, '=');
                if (tmp != NULL) {
                    tmp++;
                    while (isspace(*tmp)) {
                        tmp++;
                    }
                    strncpy(buffer, tmp, maxlen);
                    result = 1;
                    break;
                }
            }
        }
        fclose(fp);
    }
    return (result);
}

static int mi2get_cfg_int(const char *name)
{
    char buffer[128];
    char *var_ptr;
    
    if ((var_ptr = getenv(name)) != NULL) {
        strncpy(buffer, var_ptr, sizeof (buffer));
    }
    else {
        if (!mi2read_cfg(name, buffer, sizeof(buffer))) {
            return (0);
        }
    }
    return (atoi(buffer));
}

static char * mi2get_cfg_str(const char *name)
{
    char buffer[256];
    char *var_ptr;

    if ((var_ptr = getenv(name)) != NULL) {
        strncpy(buffer, var_ptr, sizeof(buffer));
    }
    else {
        if (!mi2read_cfg(name, buffer, sizeof(buffer))) {
            return (NULL);
        }
    }
    return (strdup(buffer));
}

void mi2log_init ( const char *name )
{
  char *fname_str = mi2get_cfg_str ( MICFG_LOGFILE );
  int level = mi2get_cfg_int ( MICFG_LOGLEVEL );

  if ( fname_str == NULL ) {
    _MI2_log.fp = stderr;
  } else if ( !strcmp ( fname_str, "stdout" ) || !strcmp ( fname_str, "-" ) ) {
    _MI2_log.fp = stdout;
  } else {
    if ( *fname_str == '+' ) {
      _MI2_log.fp = fopen ( fname_str + 1, "w+" );
    } else {
      _MI2_log.fp = fopen ( fname_str, "w" );
    }
  }

  if ( level != 0 ) {
    _MI2_log.level = level;
  }

  strncpy ( _MI2_log.prog, name, sizeof ( _MI2_log.prog ) );

  if ( fname_str != NULL ) {
    free ( fname_str );
  }
}

int mi2log_set_verbosity ( int lvl )
{
  int lvl_prev = _MI2_log.level;
  _MI2_log.level = lvl;
  return ( lvl_prev );
}

int mi2log_message ( const char *file,int line, mi2msgcode_t code, ... )
{
  va_list ap;
  int lvl;
  const char *fmt;

  if ( _MI2_log.fp == NULL ) {
    _MI2_log.fp = stderr;
  }

  lvl = mierror_table[code - MI2_MSG_BASE].level;
  fmt = mierror_table[code - MI2_MSG_BASE].msgfmt;

  /* Log the message if the the message priority
   * is less than the configured priority.  Always log fatal errors.
   */
  if ( ( lvl <= _MI2_log.level ) || lvl == MI2_MSG_FATAL ) {
    if ( _MI2_log.prog[0] != '\0' ) {
      fprintf ( _MI2_log.fp, "%s:%d %s ", file, line, _MI2_log.prog );
    }
    fprintf ( _MI2_log.fp, "%s:%d (from %s): ", file, line, minc_routine_name );
    va_start ( ap, code );
    vfprintf ( _MI2_log.fp, fmt, ap );
    va_end ( ap );
    fprintf ( _MI2_log.fp, "\n" );
    fflush ( _MI2_log.fp );
  }

  /* For fatal messages, give up and exit.
   */
  if ( lvl == MI2_MSG_FATAL ) {
    /*exit ( -1 );  SORRRY, no exits in my programs!*/
    return ( MI_ERROR );  /* Just for convenience */
  }

  return ( MI_ERROR );  /* Just for convenience */
}

// kate: indent-mode cstyle; indent-width 2; replace-tabs on; 
