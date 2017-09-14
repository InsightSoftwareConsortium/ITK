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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/


#include  <internal_volume_io.h>

#include  <sys/types.h>

#if HAVE_GETTIMEOFDAY && HAVE_SYS_TIME_H
# include <sys/time.h>
#endif 

#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_UNISTD_H
#include  <unistd.h>
#endif

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/*#ifndef CLK_TCK
#define CLK_TCK CLOCKS_PER_SEC
#endif*/

#if !defined(HAVE_SLEEP)

void sleep(unsigned milliseconds)
{
  fprintf(stderr,"Unfortunately sleep is not implemented!\n");
}

#endif /*HAVE_SLEEP*/

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_clock_ticks_per_second
@INPUT      : 
@OUTPUT     : 
@RETURNS    : number clock ticks per second
@DESCRIPTION: Returns the number of clock ticks per second in a system
              independent fashion
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jul 3, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_Real  get_clock_ticks_per_second( void )
{
#if defined(CLOCKS_PER_SEC)
  return( (VIO_Real) CLOCKS_PER_SEC );
#elif defined(CLK_TCK)
  return (VIO_Real) CLK_TCK;
#endif
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : current_cpu_seconds
@INPUT      : 
@OUTPUT     : 
@RETURNS    : # seconds
@DESCRIPTION: Returns the number of cpu seconds used by the program to date.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Real  current_cpu_seconds( void )
{
    static VIO_BOOL first_call = TRUE;
    static clock_t first;
    clock_t current;
    VIO_Real secs;

    if (first_call)
    {
        first_call = FALSE;
        first = clock();
    }
    current = clock();
    secs = (VIO_Real) (current - first) / get_clock_ticks_per_second();
    return (secs);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : current_realtime_seconds
@INPUT      : 
@OUTPUT     : 
@RETURNS    : # seconds
@DESCRIPTION: Returns the number of seconds since the first invocation of this
            : function.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Real  current_realtime_seconds( void )
{
    static VIO_Real first_seconds = -1.0;
    VIO_Real current_seconds;
#if HAVE_CLOCK_GETTIME
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) < 0) {
      fprintf(stderr, "ERROR: clock_gettime failed.\n");
      current_seconds = first_seconds;
    }
    else {
      current_seconds = ts.tv_sec + (ts.tv_nsec / 1.0e9);
    }
#elif HAVE_GETTIMEOFDAY
    struct timeval tv;
    if (gettimeofday(&tv, NULL) < 0) {
      fprintf(stderr, "ERROR: gettimeofday failed.\n");
      current_seconds = first_seconds;
    }
    else {
      current_seconds = tv.tv_sec + (tv.tv_usec / 1.0e6);
    }
#else
    /* This case is actually INCORRECT, in that users of the function
     * assume a fractional number of seconds is returned, but this
     * method can only return a whole number. This means that tests
     * that assume less than a second's precision will be unreliable.
     */
    current_seconds = (VIO_Real) time(NULL);
#endif
    
    if( first_seconds < 0.0 )
    {
        first_seconds = current_seconds;
    }
    return ( current_seconds - first_seconds );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : format_time
@INPUT      : format
            : seconds
@OUTPUT     : str
@RETURNS    : 
@DESCRIPTION: Decides what time unit to use and displays the seconds value
            : in str, using format.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_STR  format_time(
    VIO_STR   format,
    VIO_Real     seconds )
{
    int      i;
    static   char   *units[] = { "us", "ms", "sec", "min", "hrs",
                                 "days", "years"
                               };
    static   VIO_Real   scales[] = { 1000.0, 1000.0, 60.0, 60.0, 24.0, 365.0 };
    char     buffer[VIO_EXTREMELY_LARGE_STRING_SIZE];
    VIO_BOOL  negative;

    negative = seconds < 0.0;
    if( negative )  seconds = -seconds;

    seconds *= 1.0e6;

    for_less( i, 0, VIO_SIZEOF_STATIC_ARRAY(units)-1 )
    {
        if( seconds > 2.0 * scales[i] )
        {
            seconds /= scales[i];
        }
        else
        {
            break;
        }
    }

    seconds = (VIO_Real) VIO_ROUND( 10.0 * seconds ) / 10.0;

    if( negative )  seconds = -seconds;

    (void) sprintf( buffer, format, seconds, units[i] );

    return( create_string( buffer ) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : print_time
@INPUT      : format
            : seconds
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Prints out the time in suitable units.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  print_time(
    VIO_STR   format,
    VIO_Real     seconds )
{
    VIO_STR  str;

    str = format_time( format, seconds );

    print( "%s", str );

    delete_string( str );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_clock_time
@INPUT      : 
@OUTPUT     : time_str
@RETURNS    : 
@DESCRIPTION: Stores the current time of day in the "time_str".
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_STR  get_clock_time( void )
{
    time_t           clock_time;
    struct  tm       *time_tm;
    char             *str;

    (void) time( &clock_time );

    time_tm = localtime( &clock_time );

    str = asctime( time_tm );

    return( create_string( str ) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : sleep_program
@INPUT      : seconds
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Make the program sleep for the specified number of seconds.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  sleep_program( VIO_Real seconds )
{
#if HAVE_SELECT
    struct  timeval  timeout;

    timeout.tv_sec = (long) seconds;
    timeout.tv_usec = (long) (1.0e6 * (seconds - (VIO_Real) timeout.tv_sec) + 0.5);

    (void) select( 0, NULL, NULL, NULL, &timeout );
#else
    sleep((unsigned int) seconds);
#endif
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_date
@INPUT      : 
@OUTPUT     : date_str
@RETURNS    : 
@DESCRIPTION: Fills in the date into the string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_STR  get_date( void )
{
    time_t           clock_time;
    struct  tm       *time_tm;
    char             *str;

    (void) time( &clock_time );

    time_tm = localtime( &clock_time );

    str = asctime( time_tm );

    return( create_string( str ) );
}
