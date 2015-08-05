/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc2_error.h
@DESCRIPTION: File containing error codes for libminc package.
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
#ifndef MINC2_ERROR_H
#define MINC2_ERROR_H

/* message levels */
#define MI2_MSG_FATAL 0
#define MI2_MSG_ERROR 1
#define MI2_MSG_WARNING 2
#define MI2_MSG_INFO 3
#define MI2_MSG_DEBUG 4

#define MI2_MSG_BASE (10000)

typedef enum mi2msgcode {
    MI2_MSG_UNCMPFAIL = MI2_MSG_BASE,
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
} mi2msgcode_t;

int  mi2log_message(const char *file,int line, mi2msgcode_t code, ...);
int  MI2_save_routine_name(char *name);
int  MI2_return(void);
int  MI2_return_error(void);
void MI2_log_pkg_error2(int p1, char *p2);
void MI2_log_pkg_error3(int p1, char *p2, char *p3);
void MI2_log_sys_error1(char *p1);
void mi2log_init(const char *name);
int mi2log_set_verbosity ( int lvl );

#define MI_LOG_ERROR(code,...) mi2log_message(__FILE__,__LINE__,code , ##__VA_ARGS__ )
#define MI_CHECK_HDF_CALL(var,call) {if((var)<0) MI_LOG_ERROR(MI2_MSG_HDF5,call);}
#define MI_CHECK_HDF_CALL_RET(var,call) {if((var)<0) return MI_LOG_ERROR(MI2_MSG_HDF5,call);}

#endif /* MINC2_ERROR_H */
