/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_error.h
@DESCRIPTION: File containing error codes for minc package.
@GLOBALS    : 
@CALLS      : 
@CREATED    : 17 Feburary, 2004 (Robert Vincent)
@MODIFIED   : 
 *
 * $Log: minc_error.h,v $
 * Revision 6.4  2008-04-11 05:15:00  rotor
 *  * rewrote error code  (Claude) to remove global defs that were
 *     causing build problems with DYLIB on OSX
 *
 * Revision 6.3  2004/12/03 21:52:35  bert
 * Minor changes for Windows build
 *
 * Revision 6.2  2004/10/15 13:46:15  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.1  2004/04/27 15:42:47  bert
 * Define MINC logging codes
 *
 * 
@COPYRIGHT  :
              Copyright 2004 Robert Vincent, McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */
#ifndef MINC_ERROR_H
#define MINC_ERROR_H

/* minc1 message levels */
#define MI_MSG_FATAL 0
#define MI_MSG_ERROR 1
#define MI_MSG_WARNING 2
#define MI_MSG_INFO 3
#define MI_MSG_DEBUG 4

/* minc2 message levels */
#define MI2_MSG_FATAL 0
#define MI2_MSG_ERROR 1
#define MI2_MSG_WARNING 2
#define MI2_MSG_INFO 3
#define MI2_MSG_DEBUG 4

#define MI_MSG_BASE (10000)

typedef enum mimsgcode {
    MI_MSG_UNCMPFAIL = MI_MSG_BASE,
    MI_MSG_NOWRITECMP,
    MI_MSG_OPENFILE,
    MI_MSG_CREATEFILE,
    MI_MSG_CLOSEFILE,
    MI_MSG_FINDATTR,
    MI_MSG_ATTRNOTNUM,
    MI_MSG_READATTR,
    MI_MSG_NOMEMATTR,
    MI_MSG_CONVATTR,
    MI_MSG_ATTRNOTSCALAR,
    MI_MSG_ATTRNOTSTR,
    MI_MSG_WRITEATTR,
    MI_MSG_READVAR,
    MI_MSG_WRITEVAR,
    MI_MSG_FINDVAR,
    MI_MSG_ATTRCOUNT,
    MI_MSG_ATTRNAME,
    MI_MSG_COPYATTR,
    MI_MSG_VARINQ,
    MI_MSG_UNLIMDIM,
    MI_MSG_DIMINQ,
    MI_MSG_VARCONFLICT,
    MI_MSG_DIMDEF,
    MI_MSG_VARDEF,
    MI_MSG_VARMISMATCH,
    MI_MSG_VARDIFFSIZE,
    MI_MSG_VARCOUNT,
    MI_MSG_OUTPUTVAR,
    MI_MSG_COPYVAR,
    MI_MSG_VARNOTNUM,
    MI_MSG_OUTOFMEM,
    MI_MSG_ATTRNOTPTR,
    MI_MSG_VARNOTSTD,
    MI_MSG_DIMWIDTH,
    MI_MSG_MAXMINVARY,
    MI_MSG_SNH,
    MI_MSG_INTSIZE,
    MI_MSG_FLTSIZE,
    MI_MSG_TYPECLASS,
    MI_MSG_NOTIMPL,
    MI_MSG_BADTYPE,
    MI_MSG_OPENDSET,
    MI_MSG_READDSET,
    MI_MSG_WRITEDSET,
    MI_MSG_TOOMANYDIMS,
    MI_MSG_ICVATTACHED,
    MI_MSG_BADICV,
    MI_MSG_BADPROP,
    MI_MSG_ICVNOTATTACHED,
    MI_MSG_ICVCOORDS,
    MI_MSG_BADOP,
    MI_MSG_NCOPTS_STACK_OVER,
    MI_MSG_NCOPTS_STACK_UNDER,
    MI_MSG_VOLUME_IO,

    MI2_MSG_UNCMPFAIL,
    MI2_MSG_NOWRITECMP,
    MI2_MSG_OPENFILE,
    MI2_MSG_CREATEFILE,
    MI2_MSG_CLOSEFILE,
    MI2_MSG_FINDATTR,
    MI2_MSG_ATTRNOTNUM,
    MI2_MSG_READATTR,
    MI2_MSG_NOMEMATTR,
    MI2_MSG_CONVATTR,
    MI2_MSG_ATTRNOTSCALAR,
    MI2_MSG_ATTRNOTSTR,
    MI2_MSG_WRITEATTR,
    MI2_MSG_READVAR,
    MI2_MSG_WRITEVAR,
    MI2_MSG_FINDVAR,
    MI2_MSG_ATTRCOUNT,
    MI2_MSG_ATTRNAME,
    MI2_MSG_COPYATTR,
    MI2_MSG_VARINQ,
    MI2_MSG_UNLIMDIM,
    MI2_MSG_DIMINQ,
    MI2_MSG_VARCONFLICT,
    MI2_MSG_DIMDEF,
    MI2_MSG_VARDEF,
    MI2_MSG_VARMISMATCH,
    MI2_MSG_VARDIFFSIZE,
    MI2_MSG_VARCOUNT,
    MI2_MSG_OUTPUTVAR,
    MI2_MSG_COPYVAR,
    MI2_MSG_VARNOTNUM,
    MI2_MSG_OUTOFMEM,
    MI2_MSG_ATTRNOTPTR,
    MI2_MSG_VARNOTSTD,
    MI2_MSG_DIMWIDTH,
    MI2_MSG_MAXMINVARY,
    MI2_MSG_SNH,
    MI2_MSG_INTSIZE,
    MI2_MSG_FLTSIZE,
    MI2_MSG_TYPECLASS,
    MI2_MSG_NOTIMPL,
    MI2_MSG_BADTYPE,
    MI2_MSG_OPENDSET,
    MI2_MSG_READDSET,
    MI2_MSG_WRITEDSET,
    MI2_MSG_TOOMANYDIMS,
    MI2_MSG_ICVATTACHED,
    MI2_MSG_BADICV,
    MI2_MSG_BADPROP,
    MI2_MSG_ICVNOTATTACHED,
    MI2_MSG_ICVCOORDS,
    MI2_MSG_BADOP,
    MI2_MSG_HDF5,
    MI2_MSG_GENERIC
} mimsgcode_t;

int milog_message(mimsgcode_t code, ...);
int mi2log_message(const char *file,int line, mimsgcode_t code, ...);

int  MI2_save_routine_name(char *name);
int  MI2_return(void);
int  MI2_return_error(void);
void MI2_log_pkg_error2(int p1, char *p2);
void MI2_log_pkg_error3(int p1, char *p2, char *p3);
void MI2_log_sys_error1(char *p1);
void mi2log_init(const char *name);
int  mi2log_set_verbosity ( int lvl );

int MI_save_routine_name(char *name);
int MI_return(void);
int MI_return_error(void);
void MI_log_pkg_error2(int p1, char *p2);
void MI_log_pkg_error3(int p1, char *p2, char *p3);
void MI_log_sys_error1(char *p1);

  
#define MI_LOG_ERROR(code,...) mi2log_message(__FILE__, __LINE__, code, ##__VA_ARGS__ )
#define MI_CHECK_HDF_CALL(var,call) {if((var)<0) MI_LOG_ERROR(MI2_MSG_HDF5,call);}
#define MI_CHECK_HDF_CALL_RET(var,call) {if((var)<0) return MI_LOG_ERROR(MI2_MSG_HDF5,call);}


/* Macros for logging errors. All routines should start with MI_SAVE_ROUTINE
   and exit with MI_RETURN (which includes MI_RETURN_ERROR and 
   MI_CHK_ERROR). All the macros except MI_CHK_ERROR are single line
   commands. MI_CHK_ERROR is in a block and so should not be followed by
   a ';' */
#define MI_SAVE_ROUTINE_NAME(name) MI_save_routine_name(name)
#define MI_RETURN(value) \
   return( MI_return() ? (value) : (value) )
#define MI_RETURN_ERROR(error) \
   return( MI_return_error() ? (error) : (error) )
#define MI_LOG_PKG_ERROR2(p1,p2) MI_log_pkg_error2(p1, p2)
#define MI_LOG_PKG_ERROR3(p1,p2,p3) MI_log_pkg_error3(p1, p2, p3)
#define MI_LOG_SYS_ERROR1(p1) MI_log_sys_error1(p1)
#define MI_CHK_ERR(expr) {if ((expr)<0) MI_RETURN_ERROR(MI_ERROR);}

/* NetCDF routine name variable (for error logging) */
/*extern char *cdf_routine_name ;*/ /* defined in globdef.c */
/*#define MI_NC_ROUTINE_VAR cdf_routine_name*/


#endif /* MINC_ERROR_H not defined */
