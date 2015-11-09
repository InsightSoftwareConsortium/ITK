#ifndef VOL_IO_PROGRESS_H
#define VOL_IO_PROGRESS_H

/* ----------------------------------------------------------------------------
@COPYRIGHT  :
              Copyright 1993,1994,1995 David MacDonald,
              McConnell Brain Imaging Centre,
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/progress.h,v 1.10 2005-05-19 21:19:28 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : progress.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Defines type used for progress reporting.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#include  <volume_io/basic.h>

typedef  struct
{
    VIO_BOOL   force_one_line;
    VIO_BOOL   first_msg_displayed;
    VIO_BOOL   one_line_flag;
    int        n_steps;
    int        n_dots_so_far;
    int        total_n_dots;
    VIO_Real   start_time;
    VIO_Real   previous_time;
    VIO_Real   update_rate;
    VIO_Real   sum_xy;
    VIO_Real   sum_xx;
    VIO_STR    title;

    VIO_Real   last_check_time;
    int        check_every;
    int        next_check_step;
    int        last_check_step;
} VIO_progress_struct;


#endif /* VOL_IO_PROGRESS_H */
