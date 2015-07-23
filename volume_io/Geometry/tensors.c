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

/* ----------------------------- MNI Header -----------------------------------
@NAME       : multiply_basis_matrices
@INPUT      : n_derivs
              n_degs
              m1
              m2
@OUTPUT     : prod
@RETURNS    : 
@DESCRIPTION: Performs a matrix multiply of the basis matrix with the
              powers of the u's positions.  Steps through the
              matrices in the appropriate strides.  Could use the more
              general multiply_matrices below, but is done this way for speed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static void multiply_basis_matrices(
    int    n_derivs,
    int    n_degs,
    VIO_Real   m1[],
    VIO_Real   m2[],
    VIO_Real   prod[] )
{
    int   i, j, k, m2_inc;
    VIO_Real  *m1_ptr, *m1_ptr1;
    VIO_Real  *m2_ptr;
    VIO_Real  sum, *prod_ptr;

    m1_ptr = m1;
    prod_ptr = prod;
    m2_inc = 1 - n_degs * n_degs;
    for_less( i, 0, n_derivs )
    {
        m2_ptr = m2;
        for_less( j, 0, n_degs )
        {
            sum = 0.0;
            m1_ptr1 = m1_ptr;
            for_less( k, 0, n_degs )
            {
                sum += (*m1_ptr1) * (*m2_ptr);
                ++m1_ptr1;
                m2_ptr += n_degs;
            }
            m2_ptr += m2_inc;
            *prod_ptr = sum;
            ++prod_ptr;
        }

        m1_ptr += n_degs;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : multiply_matrices
@INPUT      : x1           - x size of first matrix
              y1           - y size of first matrix
              m1           - first matrix
              sa1          - x stride of first matrix
              sb1          - y stride of first matrix
            : y2           - y size of second matrix (x size must be y1)
              m2           - second matrix
              sa2          - x stride of second matrix
              sb2          - y stride of second matrix
              sap          - x stride of product matrix
              sbp          - y stride of product matrix
@OUTPUT     : prod         - product of m1 * m2
@RETURNS    : 
@DESCRIPTION: Multiplies the two matrices m1 and m2, placing the results in
              prod.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan. 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static void multiply_matrices(
    int    x1,
    int    y1,
    VIO_Real   m1[],
    int    sa1,
    int    sb1,
    int    y2,
    VIO_Real   m2[],
    int    sa2,
    int    sb2,
    VIO_Real   prod[],
    int    sap,
    int    sbp )
{
    int   i, j, k;
    VIO_Real  *m1_ptr, *m1_ptr1, *m2_ptr;
    VIO_Real  sum, *prod_ptr;

    m1_ptr = m1;
    prod_ptr = prod;
    sb2 -= y1 * sa2;
    sap -= y2 * sbp;
    for_less( i, 0, x1 )
    {
        m2_ptr = m2;
        for_less( j, 0, y2 )
        {
            sum = 0.0;
            m1_ptr1 = m1_ptr;
            for_less( k, 0, y1 )
            {
                sum += (*m1_ptr1) * (*m2_ptr);
                m1_ptr1 += sb1;
                m2_ptr += sa2;
            }

            *prod_ptr = sum;

            prod_ptr += sbp;
            m2_ptr += sb2;
        }
        m1_ptr += sa1;
        prod_ptr += sap;
    }
}

#define  MAX_DEGREE        4
#define  MAX_DIMS          10
#define  MAX_TOTAL_VALUES  4000

/* ----------------------------- MNI Header -----------------------------------
@NAME       : spline_tensor_product
@INPUT      : n_dims
              positions[n_dims]
              degrees[n_dims]
              bases[n_dims][degrees[dim]*degrees[dim]]
              n_values
              coefs [n_values*degrees[0]*degrees[1]*...]
              n_derivs[n_dims]
@OUTPUT     : results[n_values*n_derivs[0]*n_derivs[1]*...]
@RETURNS    : 
@DESCRIPTION: Performs the spline tensor product necessary to evaluate.
              Takes as input the number of dimensions, the position to
              evaluate, the basis matrices defining the interpolation method,
              and the control vertices (coefs).  The resulting values and
              derivatives are placed in the 1D array results, conceptually as a
              (1+n_dims)-D array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  spline_tensor_product(
    int     n_dims,
    VIO_Real    positions[],
    int     degrees[],
    VIO_Real    *bases[],
    int     n_values,
    VIO_Real    coefs[],
    int     n_derivs[],
    VIO_Real    results[] )
{
    int       deriv, d, k, total_values, src;
    int       ind, prev_ind, max_degree, n_derivs_plus_1, deg;
    int       static_indices[MAX_DIMS];
    int       *indices, total_derivs;
    VIO_Real      *input_coefs, u_power, u;
    VIO_Real      static_us[MAX_DEGREE*MAX_DEGREE];
    VIO_Real      static_weights[MAX_DEGREE*MAX_DEGREE];
    VIO_Real      *us, *weights;
    VIO_Real      *tmp_results[2], *r;
    VIO_Real      static_tmp_results[2][MAX_TOTAL_VALUES];
    VIO_BOOL   results_alloced;

    /*--- check arguments */

    max_degree = 2;
    total_values = n_values;
    total_derivs = 0;
    for_less( d, 0, n_dims )
    {
        if( degrees[d] < 2  )
        {
            print_error(
                  "spline_tensor_product: Degree %d must be greater than 1.\n",
                  degrees[d] );
            return;
        }
        if( degrees[d] > max_degree )
            max_degree = degrees[d];
        if( n_derivs[d] > total_derivs )
            total_derivs = n_derivs[d];

        total_values *= degrees[d];
    }

    /*--- determine if fixed size storage is large enough,
          if not allocate memory */

    if( n_dims > MAX_DIMS )
    {
        ALLOC( indices, n_dims );
    }
    else
    {
        indices = static_indices;
    }

    if( max_degree > MAX_DEGREE )
    {
        ALLOC( us, max_degree * max_degree );
        ALLOC( weights, max_degree * max_degree );
    }
    else
    {
        us = static_us;
        weights = static_weights;
    }

    if( total_values > MAX_TOTAL_VALUES )
    {
        ALLOC( tmp_results[0], total_values );
        ALLOC( tmp_results[1], total_values );
        results_alloced = TRUE;
    }
    else
    {
        tmp_results[0] = static_tmp_results[0];
        tmp_results[1] = static_tmp_results[1];
        results_alloced = FALSE;
    }

    input_coefs = coefs;

    src = 0;

    /*--- do each dimension */

    for_less( d, 0, n_dims )
    {
        deg = degrees[d];
        n_derivs_plus_1 = 1 + n_derivs[d];

        /*--- fill in the top row of matrix of powers of u
              = [1 u u^2 u^3 ...] for evaluating values */

        u = positions[d];
        u_power = 1.0;
        us[0] = 1.0;
        for_less( k, 1, deg )
        {
            u_power *= u;
            us[k] = u_power;
        }

        /*--- fill in the rest of the n_derivs_plus_1 by degrees[d] matrix:
              1  u   u^2  u^3 ...
              0  1  2u   3u^2 ...
              0  0   2   6u   ...
                    ...             */

        ind = deg;
        for_less( deriv, 1, n_derivs_plus_1 )
        {
            for_less( k, 0, deriv )
            {
                us[ind] = 0.0;
                ++ind;
            }
   
            prev_ind = VIO_IJ( deriv-1, deriv-1, deg );
            for_less( k, deriv, deg )
            {
                us[ind] = us[prev_ind] * (VIO_Real) k;
                ++ind;
                ++prev_ind;
            }
        }

        /*--- multiply the u's matrix by the spline basis to create weights */

        multiply_basis_matrices( n_derivs_plus_1, deg, us, bases[d], weights );

        total_values /= deg;

        if( d == n_dims-1 )
            r = results;
        else
            r = tmp_results[1-src];

        /*--- multiply coefficient weights by the coefficients */

        multiply_matrices( n_derivs_plus_1, deg, weights, deg, 1,
                           total_values, input_coefs, total_values, 1,
                           r, 1, n_derivs_plus_1 );

        src = 1 - src;
        input_coefs = tmp_results[src];

        total_values *= n_derivs_plus_1;
    }

    /*--- check to free memory */

    if( n_dims > MAX_DIMS )
    {
        FREE( indices );
    }

    if( max_degree > MAX_DEGREE )
    {
        FREE( us );
        FREE( weights );
    }

    if( results_alloced )
    {
        FREE( tmp_results[0] );
        FREE( tmp_results[1] );
    }
}
