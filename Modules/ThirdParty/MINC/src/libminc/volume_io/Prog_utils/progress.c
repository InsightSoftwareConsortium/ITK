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
---------------------------------------------------------------------------- */

#include  <internal_volume_io.h>

#define  FIRST_MESSAGE_THRESHOLD   5.0

#define  ONE_LINE_THRESHOLD        160.0

#define  LINE_LENGTH               77

#define  MIN_UPDATE_RATE           20.0    /* seconds */
#define  UPDATE_RATE_FACTOR        0.05

#define  RATIO_FOR_LINEAR          0.5

#define  DOUBLE_THRESHOLD          0.01
#define  HALF_THRESHOLD            0.5

static  void  show_one_line_progress(
    VIO_progress_struct    *progress,
    int                current_step );

static  void  show_multi_line_progress(
    VIO_progress_struct    *progress,
    int                current_step,
    VIO_Real               time_so_far,
    VIO_Real               est_total_time );

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_progress_report
@INPUT      : one_line_only        - whether line of dots is desired
            : n_steps
            : title
@OUTPUT     : progress             - structure is filled in
@RETURNS    : 
@DESCRIPTION: Initializes the progress report, which is either a line of dots
            : crossing the screen, or if the progress is too slow, a line
            : every 20 seconds or so indicating the amount of time left.
            : If one_line_only is true, then it is always a single line of dots.
            : n_steps is the total number of items or times through the loop.
            : If it is really fast, no messages at all are displayed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  initialize_progress_report(
    VIO_progress_struct   *progress,
    VIO_BOOL           one_line_only,
    int               n_steps,
    VIO_STR            title )
{
    progress->force_one_line = one_line_only;
    progress->first_msg_displayed = FALSE;
    progress->one_line_flag = TRUE;
    progress->n_steps = n_steps;
    progress->title = create_string( title );
    progress->start_time = current_realtime_seconds();
    progress->previous_time = progress->start_time;
    progress->last_check_time = progress->start_time;
    progress->last_check_step = 0;
    progress->next_check_step = 1;
    progress->check_every = 1;
    progress->update_rate = MIN_UPDATE_RATE;
    progress->sum_xy = 0.0;
    progress->sum_xx = 0.0;
    progress->n_dots_so_far = 0;
    progress->total_n_dots = LINE_LENGTH - string_length( progress->title );

    if( progress->total_n_dots < 1 )
        progress->total_n_dots = 2;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : update_progress_report
@INPUT      : progress
            : current_step   (an integer between 1 and n_steps)
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Checks the current time and determines if it is time to output
            : a progress message.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                 David MacDonald
@MODIFIED   : Sep.  1, 1995   D. MacDonald - changed update rate to be relative
                                             to time so far
---------------------------------------------------------------------------- */

VIOAPI  void  update_progress_report(
    VIO_progress_struct   *progress,
    int               current_step )
{
    VIO_Real    current_time, constant, n_seconds_per;
    VIO_Real    time_so_far, est_total_time;

    if( current_step < 1 || current_step < progress->next_check_step )
        return;

    if( current_step > progress->n_steps )
        current_step = progress->n_steps;

    current_time = current_realtime_seconds();

    n_seconds_per = (VIO_Real) progress->check_every *
                    (current_time - progress->last_check_time) /
                    (VIO_Real) (current_step - progress->last_check_step);

    if( n_seconds_per < DOUBLE_THRESHOLD )
        progress->check_every *= 2;
    else if( n_seconds_per > HALF_THRESHOLD && progress->check_every > 1 )
        progress->check_every /= 2;

    progress->last_check_time = current_time;
    progress->last_check_step = current_step;
    progress->next_check_step = current_step + progress->check_every;
    if( progress->next_check_step > progress->n_steps )
        progress->next_check_step = progress->n_steps;

    time_so_far = current_time - progress->start_time;

    progress->sum_xy = RATIO_FOR_LINEAR * progress->sum_xy +
                        (VIO_Real) current_step * time_so_far;
    progress->sum_xx = RATIO_FOR_LINEAR * progress->sum_xx +
                        (VIO_Real) current_step * (VIO_Real) current_step;

    if( time_so_far > FIRST_MESSAGE_THRESHOLD )
    {
        constant = progress->sum_xy / progress->sum_xx;
        est_total_time = (VIO_Real) progress->n_steps * constant;

        if( est_total_time <= time_so_far )
        {
            est_total_time = time_so_far * (VIO_Real) progress->n_steps /
                             (VIO_Real) current_step;
        }

        if( progress->force_one_line ||
            (progress->one_line_flag && est_total_time < ONE_LINE_THRESHOLD) )
        {
            show_one_line_progress( progress, current_step );
            progress->first_msg_displayed = TRUE;
        }
        else
        {
            if( progress->first_msg_displayed && progress->one_line_flag )
                print( "\n" );

            progress->one_line_flag = FALSE;

            if( current_time - progress->previous_time >= progress->update_rate)
            {
                show_multi_line_progress( progress, current_step, time_so_far,
                                          est_total_time );
                progress->first_msg_displayed = TRUE;
                progress->previous_time = current_time;

                progress->update_rate = (current_time - progress->start_time) *
                                        UPDATE_RATE_FACTOR;
                if( progress->update_rate < MIN_UPDATE_RATE )
                    progress->update_rate = MIN_UPDATE_RATE;
            }
        }
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : show_one_line_progress
@INPUT      : progress
            : current_step
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Given the current_step, and the total number, ensures that the
            : number of dots on the line is representative.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  show_one_line_progress(
    VIO_progress_struct    *progress,
    int                current_step )
{
    long     i, n_dots;

    n_dots = VIO_ROUND( (VIO_Real) current_step / (VIO_Real) progress->n_steps *
                    (VIO_Real) progress->total_n_dots );

    if( n_dots > progress->total_n_dots )
        handle_internal_error( "show_one_line_progress" );

    if( n_dots > progress->n_dots_so_far )
    {
        if( progress->n_dots_so_far == 0 )
        {
            print( "%s: ", progress->title );
        }

        for_less( i, progress->n_dots_so_far, n_dots )
        {
            print( "." );
        }

        (void) flush_file( stdout );

        progress->n_dots_so_far = n_dots;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : show_multi_line_progress
@INPUT      : progress
            : current_step
            : time_so_far
            : est_total_time
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Displays report about time so far, estimated time left, etc.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  show_multi_line_progress(
    VIO_progress_struct    *progress,
    int                current_step,
    VIO_Real               time_so_far,
    VIO_Real               est_total_time )
{
    long     percent_done;
    VIO_STR  time_so_far_str, est_total_time_str;

    percent_done = VIO_ROUND( 100.0 * (VIO_Real) current_step /
                          (VIO_Real) progress->n_steps );

    time_so_far_str = format_time( "%g %s", time_so_far );
    est_total_time_str = format_time( "%g %s", est_total_time );

    print( "%s: %3ld%% done. (%d/%d)   Time: %s out of approx %s\n",
           progress->title, percent_done, current_step, progress->n_steps,
           time_so_far_str, est_total_time_str );

    delete_string( time_so_far_str );
    delete_string( est_total_time_str );

    (void) flush_file( stdout );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : terminate_progress_report
@INPUT      : progress
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Terminates the progress report.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  terminate_progress_report(
    VIO_progress_struct   *progress )
{
    VIO_Real    total_time;
    VIO_STR  time_str;

    if( progress->first_msg_displayed )
    {
        if( progress->one_line_flag )
        {
            show_one_line_progress( progress, progress->n_steps );
            print( "\n" );
        }
        else
        {
            total_time = current_realtime_seconds() - progress->start_time;

            time_str = format_time( "%g %s", total_time );

            print( "%s: DONE in %s\n", progress->title, time_str );

            delete_string( time_str );
        }
    }

    delete_string( progress->title );
}
