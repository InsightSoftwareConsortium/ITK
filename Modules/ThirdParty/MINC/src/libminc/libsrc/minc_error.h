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

/* message levels */
#define MI_MSG_FATAL 0
#define MI_MSG_ERROR 1
#define MI_MSG_WARNING 2
#define MI_MSG_INFO 3
#define MI_MSG_DEBUG 4

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
    MI_MSG_NCOPTS_STACK_UNDER
} mimsgcode_t;

MNCAPI int milog_message(mimsgcode_t code, ...);

#endif /* MINC_ERROR_H not defined */
