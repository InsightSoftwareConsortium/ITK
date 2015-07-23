/* ----------------------------- MNI Header -----------------------------------
@NAME       : time_stamp.h
@DESCRIPTION: Header file for time_stamp.c
@GLOBALS    : 
@CREATED    : February 1, 1993 (Peter Neelin)
@MODIFIED   : 
 * $Log: time_stamp.h,v $
 * Revision 6.2  2004-10-15 13:46:51  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.1  2002/01/14 21:28:26  neelin
 * Moved nd_loop, voxel_loop, ParseArgv and time_stamp from ../progs/Proglib
 * in order to include them in the main minc library.
 *
 * Revision 6.2  1999/10/19 15:57:18  neelin
 * Fixed log message containing log substitution
 *
 * Revision 6.1  1999/10/19 14:45:14  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:23:41  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:24:41  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:00:50  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:31:35  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:34:31  neelin
 * Release of minc version 0.2
 *
 * Revision 1.4  94/09/28  10:34:20  neelin
 * Pre-release
 * 
 * Revision 1.3  93/08/04  13:04:00  neelin
 * Added RCS Log to keep track of modifications in source
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

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

char *time_stamp(int argc, char *argv[]);

#ifdef __cplusplus
}
#endif /* __cplusplus */

