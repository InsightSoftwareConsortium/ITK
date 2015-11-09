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

#include  "internal_volume_io.h"

/* ----------------------------- MNI Header -----------------------------------
@NAME       : scaled_maximal_pivoting_gaussian_elimination
@INPUT      : n                 size of matrix, n by n
              a                 matrix
              n_values          number of values to solve for
@OUTPUT     : row               permutation array filled in by this function
              solution          on input, the values, on output the solution,
                                size n by n_values
@RETURNS    : TRUE if successful
@DESCRIPTION: Performs scaled maximal pivoting gaussian elimination as a
              numerically robust method to solve systems of linear equations.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_BOOL  scaled_maximal_pivoting_gaussian_elimination(
    int   n,
    int   row[],
    VIO_Real  **a,
    int   n_values,
    VIO_Real  **solution )
{
    int       i, j, k, p, v, tmp;
    VIO_Real      *s, val, best_val, m, scale_factor;
    VIO_BOOL   success;

    ALLOC( s, n );

    for_less( i, 0, n )
        row[i] = i;

    for_less( i, 0, n )
    {
        s[i] = VIO_FABS( a[i][0] );
        for_less( j, 1, n )
        {
            if( VIO_FABS(a[i][j]) > s[i] )
               s[i] = VIO_FABS(a[i][j]);
        }

        if( s[i] == 0.0 )
        {
            FREE( s );

            return( FALSE );
        }
    }

    success = TRUE;

    for_less( i, 0, n-1 )
    {
        p = i;
        best_val = a[row[i]][i] / s[row[i]];
        best_val = VIO_FABS( best_val );
        for_less( j, i+1, n )
        {
            val = a[row[j]][i] / s[row[j]];
            val = VIO_FABS( val );
            if( val > best_val )
            {
                best_val = val;
                p = j;
            }
        }

        if( a[row[p]][i] == 0.0 )
        {
            success = FALSE;
            break;
        }

        if( i != p )
        {
            tmp = row[i];
            row[i] = row[p];
            row[p] = tmp;
        }

        for_less( j, i+1, n )
        {
            if( a[row[i]][i] == 0.0 )
            {
                success = FALSE;
                break;
            }

            m = a[row[j]][i] / a[row[i]][i];
            for_less( k, i+1, n )
                a[row[j]][k] -= m * a[row[i]][k];
            for_less( v, 0, n_values )
                solution[row[j]][v] -= m * solution[row[i]][v];
        }

        if( !success )
            break;
    }

    if( success && a[row[n-1]][n-1] == 0.0 )
        success = FALSE;

    if( success )
    {
        for( i = n-1;  i >= 0;  --i )
        {
            for_less( j, i+1, n )
            {
                scale_factor = a[row[i]][j];
                for_less( v, 0, n_values )
                    solution[row[i]][v] -= scale_factor * solution[row[j]][v];
            }

            for_less( v, 0, n_values )
                solution[row[i]][v] /= a[row[i]][i];
        }
    }

    FREE( s );

    return( success );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : scaled_maximal_pivoting_gaussian_elimination_real
@INPUT      : n
              coefs
              n_values
              values
@OUTPUT     : values has the solution on output
@RETURNS    : TRUE if successful
@DESCRIPTION: Performs gaussian elimination on a type-VIO_Real matrix, first
              copying it into temporary storage, which is modified as
              the gaussian elimination is performed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_BOOL scaled_maximal_pivoting_gaussian_elimination_real(
    int   n,
    VIO_Real  **coefs,
    int   n_values,
    VIO_Real  **values )
{
    int       i, j, v, *row;
    VIO_Real      **a, **solution;
    VIO_BOOL   success;

    ALLOC( row, n );
    VIO_ALLOC2D( a, n, n );
    VIO_ALLOC2D( solution, n, n_values );

    for_less( i, 0, n )
    {
        for_less( j, 0, n )
            a[i][j] = coefs[i][j];
        for_less( v, 0, n_values )
            solution[i][v] = values[v][i];
    }

    success = scaled_maximal_pivoting_gaussian_elimination( n, row, a, n_values,
                                                            solution );

    if( success )
    {
        for_less( i, 0, n )
        {
            for_less( v, 0, n_values )
                values[v][i] = solution[row[i]][v];
        }
    }

    VIO_FREE2D( a );
    VIO_FREE2D( solution );
    FREE( row );

    return( success );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : solve_linear_system
@INPUT      : n
              coefs      - n by n matrix
              values     - size n list
@OUTPUT     : solution   - size n list
@RETURNS    : TRUE if successful
@DESCRIPTION: Solves a linear system of equations, finding the solution
                                                         t          t
              vector that satisfies  [coefs] * [solution] = [values]
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  solve_linear_system(
    int   n,
    VIO_Real  **coefs,
    VIO_Real  values[],
    VIO_Real  solution[] )
{
    int       i;

    for_less( i, 0, n )
        solution[i] = values[i];

    return( scaled_maximal_pivoting_gaussian_elimination_real( n, coefs, 1,
                                                               &solution ) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : invert_square_matrix
@INPUT      : n
              matrix   - n by n matrix
@OUTPUT     : inverse
@RETURNS    : TRUE if successful
@DESCRIPTION: Computes the inverse of a square matrix.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  invert_square_matrix(
    int   n,
    VIO_Real  **matrix,
    VIO_Real  **inverse )
{
    VIO_Real      tmp;
    VIO_BOOL   success;
    int       i, j;

    for_less( i, 0, n )
    {
        for_less( j, 0, n )
            inverse[i][j] = 0.0;
        inverse[i][i] = 1.0;
    }

    success = scaled_maximal_pivoting_gaussian_elimination_real( n, matrix,
                                                                 n, inverse );

    if( success )
    {
        for_less( i, 0, n-1 )
        {
            for_less( j, i+1, n )
            {
                tmp = inverse[i][j];
                inverse[i][j] = inverse[j][i];
                inverse[j][i] = tmp;
            }
        }
    }

    return( success );
}
