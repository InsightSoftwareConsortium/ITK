/* ----------------------------- MNI Header -----------------------------------
@NAME       : nd_loop.h
@DESCRIPTION: Header file for nd_loop.c
@METHOD     : Use these routines in the following way:

   Set start, end and increment vectors to define looping;

   nd_begin_looping(start, current, ndims);
   while (!nd_end_of_loop(current, end, ndims)) {
      nd_update_current_count(current, increment, end,
                              current_count, ndims);

      Use current and current_count to work on hyperslab;

      nd_increment_loop(current, start, increment, end, ndims);
   }

@GLOBALS    : 
@CREATED    : December 2, 1994 (Peter Neelin)
@MODIFIED   : 
 * $Log: nd_loop.h,v $
 * Revision 6.2  2004-10-15 13:47:13  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.1  2002/01/14 21:28:26  neelin
 * Moved nd_loop, voxel_loop, ParseArgv and time_stamp from ../progs/Proglib
 * in order to include them in the main minc library.
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
 * Revision 1.3  1994/12/02  09:40:37  neelin
 * Fixed arguments to nd_end_of_loop.
 *
 * Revision 1.2  94/12/02  09:20:17  neelin
 * Added comments to clarify use of routines.
 * 
 * Revision 1.1  94/12/02  08:40:31  neelin
 * Initial revision
 * 
@COPYRIGHT  :
              Copyright 1994 Peter Neelin, McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */

#include "minc.h"

#if defined(__cplusplus)
extern "C" {
#endif

MNCAPI void nd_begin_looping(long start[], long current[], int ndims);
MNCAPI int nd_end_of_loop(long current[], long end[], int ndims);
MNCAPI void nd_update_current_count(long current[], 
                                    long increment[], long end[],
                                    long current_count[],
                                    int ndims);
MNCAPI void nd_increment_loop(long current[], 
                              long start[], long increment[], long end[],
                              int ndims);

#if defined(__cplusplus)
}
#endif
