/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_error.c
@DESCRIPTION: File containing routines to do error handling for MINC package.
              Should be called through macros in minc_private.h
@GLOBALS    : 
@CALLS      : 
@CREATED    : August 7, 1992 (Peter Neelin)
@MODIFIED   : 
 * $Log: minc_error.c,v $
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

#include <errno.h>
#include <stdarg.h>
#include <minc_private.h>
#include <minc_error.h>

struct mierror_entry {
    int level;
    char *msgfmt;
};

/* MINC routine name variable, call depth counter (for keeping track of
   minc routines calling minc routines) and variable for keeping track
   of callers ncopts. All of these are for error logging. */
static char *minc_routine_name = "MINC";
static int minc_call_depth = 0;
static int minc_trash_var = 0;

static struct mierror_entry mierror_table[] = {
    { MI_MSG_ERROR, "Cannot uncompress the file" }, /* MI_MSG_UNCMPFAIL */
    { MI_MSG_ERROR, "Can't write compressed file" }, /* MI_MSG_NOWRITECMP */
    { MI_MSG_ERROR, "Unable to open file '%s'" }, /* MI_MSG_OPENFILE */
    { MI_MSG_ERROR, "Unable to create file '%s'"}, /* MI_MSG_CREATEFILE */
    { MI_MSG_ERROR, "Error closing file"}, /* MI_MSG_CLOSEFILE */
    { MI_MSG_WARNING, "Attribute '%s' not found"}, /* MI_MSG_FINDATTR */
    { MI_MSG_ERROR, "Attribute '%s' is non-numeric"}, /* MI_MSG_ATTRNOTNUM */
    { MI_MSG_ERROR, "Can't read attribute '%s'"}, /* MI_MSG_READATTR */
    { MI_MSG_FATAL, "No memory for attribute '%s'"}, /* MI_MSG_NOMEMATTR */
    { MI_MSG_ERROR, "Conversion error for attribute '%s'"}, /* MI_MSG_CONVATTR */
    { MI_MSG_ERROR, "Attribute '%s' is not a scalar"}, /* MI_MSG_ATTRNOTSCALAR */
    { MI_MSG_ERROR, "Attribute '%s' is not a string"}, /* MI_MSG_ATTRNOTSTR */
    { MI_MSG_ERROR, "Can't write attribute '%s'"}, /* MI_MSG_WRITEATTR */
    { MI_MSG_ERROR, "Can't read variable ID# %d"}, /* MI_MSG_READVAR */
    { MI_MSG_ERROR, "Can't write variable ID# %d"}, /* MI_MSG_WRITEVAR */
    { MI_MSG_ERROR, "Can't find variable ID# %d"}, /* MI_MSG_FINDVAR */
    { MI_MSG_ERROR, "Can't read attribute count"}, /* MI_MSG_ATTRCOUNT */
    { MI_MSG_ERROR, "Can't read attribute name"}, /* MI_MSG_ATTRNAME */
    { MI_MSG_ERROR, "Can't copy attribute '%s'"}, /* MI_MSG_COPYATTR */
    { MI_MSG_ERROR, "Can't read variable information"}, /* MI_MSG_VARINQ */
    { MI_MSG_ERROR, "Can't get unlimited dimension"}, /* MI_MSG_UNLIMDIM */
    { MI_MSG_ERROR, "Can't get dimension information"}, /* MI_MSG_DIMINQ */
    { MI_MSG_ERROR, "Variable already defined with different size"}, /* MI_MSG_VARCONFLICT */
    { MI_MSG_ERROR, "Can't define dimension '%s'"}, /* MI_MSG_DIMDEF */
    { MI_MSG_ERROR, "Can't define variable '%s'"}, /* MI_MSG_VARDEF */
    { MI_MSG_ERROR, "Variables do not match for value copy"}, /* MI_MSG_VARMISMATCH */
    { MI_MSG_ERROR, "Variables have dimensions of different size"}, /* MI_MSG_VARDIFFSIZE */
    { MI_MSG_ERROR, "Can't read variable count"}, /* MI_MSG_VARCOUNT */
    { MI_MSG_ERROR, "Variable '%s' not copied"}, /* MI_MSG_OUTPUTVAR */
    { MI_MSG_ERROR, "Error copying variable"}, /* MI_MSG_COPYVAR */
    { MI_MSG_ERROR, "Non-numeric datatype"}, /* MI_MSG_VARNOTNUM */
    { MI_MSG_FATAL, "Can't allocate %d bytes"}, /* MI_MSG_OUTOFMEM */
    { MI_MSG_ERROR, "Attribute '%s' is not a pointer"}, /* MI_MSG_ATTRNOTPTR */
    { MI_MSG_ERROR, "Variable '%s' is not a standard MINC variable"}, /* MI_MSG_VARNOTSTD */
    { MI_MSG_ERROR, "Bad dimension width suffix"}, /* MI_MSG_DIMWIDTH */
    { MI_MSG_ERROR, "Imagemax/min dimensions vary over image dimensions"}, /* MI_MSG_MAXMINVARY */
    { MI_MSG_FATAL, "Should not happen!"}, /* MI_MSG_SNH */
    { MI_MSG_FATAL, "Unknown integer size %d"}, /* MI_MSG_INTSIZE */
    { MI_MSG_FATAL, "Unknown float size %d"}, /* MI_MSG_FLTSIZE */
    { MI_MSG_FATAL, "Unknown type class %d"}, /* MI_MSG_TYPECLASS */
    { MI_MSG_ERROR, "Function '%s' not implemented"}, /* MI_MSG_NOTIMPL */
    { MI_MSG_FATAL, "Unknown type %d"}, /* MI_MSG_BADTYPE */
    { MI_MSG_ERROR, "Can't open dataset %s"}, /* MI_MSG_OPENDSET */
    { MI_MSG_ERROR, "Can't read dataset %s"}, /* MI_MSG_READDSET */
    { MI_MSG_ERROR, "Can't write dataset %s"}, /* MI_MSG_WRITEDSET */
    { MI_MSG_ERROR, "Can't use more than %d dimensions"}, /* MI_MSG_TOOMANYDIMS */
    { MI_MSG_ERROR, "Attempt to modify an attached image conversion variable"}, /* MI_MSG_ICVATTACHED */
    { MI_MSG_ERROR, "Illegal ICV identifier"}, /* MI_MSG_BADICV */
    { MI_MSG_ERROR, "Error setting ICV property: %s"}, /* MI_MSG_BADPROP */
    { MI_MSG_ERROR, "ICV is not attached"}, /* MI_MSG_ICVNOTATTACHED */
    { MI_MSG_ERROR, "Invalid ICV coordinates"}, /* MI_MSG_ICVCOORDS */
    { MI_MSG_ERROR, "Illegal variable access operation" }, /* MI_MSG_BADOP */
    { MI_MSG_ERROR, "ncopts stack overflow" }, /* MI_MSG_NCOPTS_STACK_OVER */
    { MI_MSG_ERROR, "ncopts stack underflow" } /* MI_MSG_NCOPTS_STACK_UNDER */
};

SEMIPRIVATE int MI_save_routine_name(char *name)
{
   /* no idea what peter was up to here */
   /* minc_trash_var = (((minc_call_depth++)==0) ? MI_save_routine_name(name) : * MI_NOERROR)) */

   if( (minc_call_depth++)==0 ) {
     minc_routine_name = name; 
     minc_trash_var = TRUE;
   } else {
     minc_trash_var = MI_NOERROR;
   }
   return(TRUE);
}

SEMIPRIVATE int MI_return(void)
{ 
   /* no idea what peter was up to here */
   /* return( (((--minc_call_depth)!=0) || MI_return()) ? (value) : (value)) */

   return( ((--minc_call_depth)!=0) || TRUE );
}

SEMIPRIVATE int MI_return_error(void)
{ 
   /* no idea what peter was up to here */
   /* return( (((--minc_call_depth)!=0) || MI_return_error()) ? (error) : (error)) */

   if( (--minc_call_depth)==0 ) {
     MI_LOG_PKG_ERROR2(0, "MINC package entry point"); 
   }
   return( TRUE );
}
SEMIPRIVATE void MI_log_pkg_error2(int p1, char *p2)
{
  (void) fprintf(stderr, "%s: ", minc_routine_name);
  (void) fprintf(stderr, "%s", p2);
  (void) fputc('\n', stderr);
  (void) fflush(stderr);
}
SEMIPRIVATE void MI_log_pkg_error3(int p1, char *p2, char *p3)
{ 
  (void) fprintf(stderr, "%s: ", minc_routine_name);
  (void) fprintf(stderr, p2, p3);
  (void) fputc('\n', stderr);
  (void) fflush(stderr);
}
SEMIPRIVATE void MI_log_sys_error1(char *p1)
{
   char *message;
   int errnum = errno;

   (void) fprintf(stderr, "%s", minc_routine_name);
   (void) fprintf(stderr, "%s", p1);
   if (errnum == 0) {
     (void) fputc('\n', stderr);
   }
   else {
     message = strerror(errnum);
     if (message == NULL) message = "Unknown error";
     (void) fprintf(stderr, ": %s\n", message);
   }
   (void) fflush(stderr);
}

/* By default, print all messages of severity error, or worse.
 */
static struct {
    int level;
    char prog[128];
    FILE *fp;
} _MI_log = {
    MI_MSG_ERROR, {""}, NULL
};

MNCAPI void milog_init(const char *name)
{
    const char *fname_str = miget_cfg_str(MICFG_LOGFILE);
    int level = miget_cfg_int(MICFG_LOGLEVEL);

    if (!strlen(fname_str)) {
	_MI_log.fp = stderr;
    }
    else if (!strcmp(fname_str, "stdout") || !strcmp(fname_str, "-")) {
	_MI_log.fp = stdout;
    }
    else {
	if (*fname_str == '+') {
	    _MI_log.fp = fopen(fname_str + 1, "w+");
	}
	else {
	    _MI_log.fp = fopen(fname_str, "w");
	}
    }

    if (level != 0) {
        _MI_log.level = level;
    }

    strncpy(_MI_log.prog, name, sizeof(_MI_log.prog) - 1 );
}

MNCAPI int milog_set_verbosity(int lvl)
{
    int lvl_prev = _MI_log.level;
    _MI_log.level = lvl;
    return (lvl_prev);
}

MNCAPI int milog_message(mimsgcode_t code, ...)
{
    va_list ap;
    int lvl;
    const char *fmt;

    if (_MI_log.fp == NULL) {
	_MI_log.fp = stderr;
    }

    lvl = mierror_table[code-MI_MSG_BASE].level;
    fmt = mierror_table[code-MI_MSG_BASE].msgfmt;

    /* Log the message if the the message priority
     * is less than the configured priority.  Always log fatal errors.
     */
    if ((lvl <= _MI_log.level) || lvl == MI_MSG_FATAL) {
	if (_MI_log.prog[0] != '\0') {
	    fprintf(_MI_log.fp, "%s ", _MI_log.prog);
	}
	fprintf(_MI_log.fp, "(from %s): ", minc_routine_name);
	va_start(ap, code);
	vfprintf(_MI_log.fp, fmt, ap);
	va_end(ap);
	fprintf(_MI_log.fp, "\n");
	fflush(_MI_log.fp);
    }

    /* For fatal messages, give up and exit.
     */
    if (lvl == MI_MSG_FATAL) {
	exit(-1);
    }

    return (MI_ERROR);		/* Just for convenience */
}

