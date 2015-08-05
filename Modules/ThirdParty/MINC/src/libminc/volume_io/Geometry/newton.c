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

#define  STEP_RATIO  1.0

/* ----------------------------- MNI Header -----------------------------------
@NAME       : newton_root_find
@INPUT      : n_dimensions                - dimensionality of domain and range
              function                    - function to find the solution to
                                          - takes as arguments the function_data
                                            pointer, the position to evaluate,
                                            and passes back the values[n_dim]
                                            and derivatives[n_dim][n_dim]
              function_data
              initial_guess[n_dimensions]
              desired_values[n_dimensions]
              function_tolerance
              delta_tolerance
              max_iterations
@OUTPUT     : 
              solution[n_dimensions]
@RETURNS    : TRUE if successful
@DESCRIPTION: Performs a newton root find of a function by taking steps of
              x' = (desired - f(x)) / grad(f(x)),
              where x starts at initial_guess and
              is updated until the f(x) is close to zero.  x is passed back
              in solution.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  newton_root_find(
    int    n_dimensions,
    void   (*function) ( void *, VIO_Real [],  VIO_Real [], VIO_Real ** ),
    void   *function_data,
    VIO_Real   initial_guess[],
    VIO_Real   desired_values[],
    VIO_Real   solution[],
    VIO_Real   function_tolerance,
    VIO_Real   delta_tolerance,
    int    max_iterations )
{
    int       iter, dim;
    VIO_Real      *values, **derivatives, *delta, error, best_error, *position;
    VIO_Real      step_size;
    VIO_BOOL   success;

    ALLOC( position, n_dimensions );
    ALLOC( values, n_dimensions );
    ALLOC( delta, n_dimensions );
    VIO_ALLOC2D( derivatives, n_dimensions, n_dimensions );

    /*--- initialize function position to the initial guess */

    for_less( dim, 0, n_dimensions )
        position[dim] = initial_guess[dim];

    iter = 0;
    success = FALSE;
    best_error = 0.0;

    while( max_iterations < 0 || iter < max_iterations )
    {
        ++iter;

        /*--- evaluate the function and derivatives */

        (*function) ( function_data, position, values, derivatives );

        /*--- compute the error between the desired values and the current */

        error = 0.0;
        for_less( dim, 0, n_dimensions )
        {
            values[dim] = desired_values[dim] - values[dim];
            error += VIO_FABS( values[dim] );
        }

        /*--- if this is best so far, record it */

        if( iter == 1 || error < best_error )
        {
            best_error = error;
            for_less( dim, 0, n_dimensions )
                solution[dim] = position[dim];

            if( error < function_tolerance )
            {
                success = TRUE;
                break;
            }
        }

        /*--- find the step (delta) to solve the linear system of function
              value and derivatives */

        if( !solve_linear_system( n_dimensions, derivatives, values, delta ) )
            break;

        /*--- compute the size of the step to see if it is small */

        step_size = 0.0;
        for_less( dim, 0, n_dimensions )
        {
            position[dim] += STEP_RATIO * delta[dim];
            step_size += VIO_FABS( delta[dim] );
        }

        if( step_size < delta_tolerance )
        {
            success = TRUE;
            break;
        }
    }

    /*--- free memory */

    FREE( values );
    FREE( delta );
    VIO_FREE2D( derivatives );
    FREE( position );

    return( success );
}
