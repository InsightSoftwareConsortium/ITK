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

/*--- Weighting functions which define the splines, all of which are
      interpolation splines, with the exception of the quadratic spline */

static VIO_Real   constant_coefs[1][1] = {   { 1.0 }  };

static VIO_Real   linear_coefs[2][2] = {
                                           {  1.0,  0.0 },
                                           { -1.0,  1.0 }
                                      };

static VIO_Real   quadratic_coefs[3][3] = {
                                           {  0.5,  0.5,  0.0 },
                                           { -1.0,  1.0,  0.0 },
                                           {  0.5, -1.0,  0.5 }
                                        };

static VIO_Real   cubic_coefs[4][4] = {
                                        {  0.0,  1.0,  0.0,  0.0 },
                                        { -0.5,  0.0,  0.5,  0.0 },
                                        {  1.0, -2.5,  2.0, -0.5 },
                                        { -0.5,  1.5, -1.5,  0.5 }
                                    };

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_linear_spline_coefs
@INPUT      : 
@OUTPUT     : coefs     2 by 2 array of coefficients
@RETURNS    : 
@DESCRIPTION: Passes back the basis matrix of the linear spline.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_linear_spline_coefs(
    VIO_Real  **coefs )
{
    int    i, j;

    for_less( i, 0, 2 )
    for_less( j, 0, 2 )
        coefs[i][j] = linear_coefs[i][j];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_quadratic_spline_coefs
@INPUT      : 
@OUTPUT     : coefs     3 by 3 array of coefficients
@RETURNS    : 
@DESCRIPTION: Passes back the basis matrix of the quadratic spline.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_quadratic_spline_coefs(
    VIO_Real  **coefs )
{
    int    i, j;

    for_less( i, 0, 3 )
    for_less( j, 0, 3 )
        coefs[i][j] = quadratic_coefs[i][j];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_cubic_spline_coefs
@INPUT      : 
@OUTPUT     : coefs     4 by 4 array of coefficients
@RETURNS    : 
@DESCRIPTION: Passes back the basis matrix of the cubic interpolating
              (Catmull-Romm) spline.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_cubic_spline_coefs(
    VIO_Real  **coefs )
{
    int    i, j;

    for_less( i, 0, 4 )
    for_less( j, 0, 4 )
        coefs[i][j] = cubic_coefs[i][j];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : cubic_interpolate
@INPUT      : u        - position to evaluate, between 0 and 1
              v0       - four control vertices
              v1
              v2
              v3
@OUTPUT     : 
@RETURNS    : interpolated value
@DESCRIPTION: Performs cubic interpolation, where a value of u = 0 returns
              v1, a value of u = 1 returns v2, and intermediate values
              smoothly interpolate.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Real  cubic_interpolate(
    VIO_Real   u,
    VIO_Real   v0,
    VIO_Real   v1,
    VIO_Real   v2,
    VIO_Real   v3 )
{
    VIO_Real   coefs[4], value;

    coefs[0] = v0;
    coefs[1] = v1;
    coefs[2] = v2;
    coefs[3] = v3;

    evaluate_univariate_interpolating_spline( u, 4, coefs, 0, &value );

    return( value );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : evaluate_univariate_interpolating_spline
@INPUT      : u
              degree        - 2,3,4 for linear, quadratic, or cubic
              coefs[degree] - control vertices
              n_derivs      - number of derivatives to compute
@OUTPUT     : derivs        - 1 + n_derivs values and derivatives
@RETURNS    : 
@DESCRIPTION: Passes back the interpolated value and n_derivs derivatives
              in the derivs array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  evaluate_univariate_interpolating_spline(
    VIO_Real    u,
    int     degree,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] )
{
    evaluate_interpolating_spline( 1, &u, degree, 1, coefs, n_derivs, derivs );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : evaluate_bivariate_interpolating_spline
@INPUT      : u             - position to evaluate
              v
              degree        - 2,3,4 for linear, quadratic, or cubic
              coefs         - control vertices, size degree * degree
              n_derivs      - number of derivatives to compute
@OUTPUT     : derivs        - (1 + n_derivs) * (1 + n_derivs)
                              values and derivatives
@RETURNS    : 
@DESCRIPTION: Passes back the interpolated value and derivatives
              in the derivs array.  derivs is a 1D array that is conceptually
              2 dimensional, indexed by dx and dy, where dx and dy range
              from 0 to n_derivs, indicating which value or derivative.
              For example 0,0 refers to the interpolated value
              whereas 1,0 refers to the derivative of the function wrt u.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  evaluate_bivariate_interpolating_spline(
    VIO_Real    u,
    VIO_Real    v,
    int     degree,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] )
{
    VIO_Real   positions[2];

    positions[0] = u;
    positions[1] = v;

    evaluate_interpolating_spline( 2, positions, degree, 1, coefs,
                                   n_derivs, derivs );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : evaluate_trivariate_interpolating_spline
@INPUT      : u             - position to evaluate
              v
              w
              degree        - 2,3,4 for linear, quadratic, or cubic
              coefs         - control vertices, size degree * degree * degree
              n_derivs      - number of derivatives to compute
@OUTPUT     : derivs        - (1 + n_derivs) * (1 + n_derivs) * (1 + n_derivs)
                              values and derivatives
@RETURNS    : 
@DESCRIPTION: Passes back the interpolated value and derivatives
              in the derivs array.  derivs is a 1D array that is conceptually
              3 dimensional, indexed by dx, dy, and dz, where dx, dy, and dz
              each range from 0 to n_derivs, indicating which value or
              derivative.   For example 0,0,0 refers to the interpolated value
              whereas 1,0,1 refers to the derivative of the function wrt u and
              w.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  evaluate_trivariate_interpolating_spline(
    VIO_Real    u,
    VIO_Real    v,
    VIO_Real    w,
    int     degree,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] )
{
    VIO_Real   positions[3];

    positions[0] = u;
    positions[1] = v;
    positions[2] = w;

    evaluate_interpolating_spline( 3, positions, degree, 1, coefs,
                                   n_derivs, derivs );
}

#define  MAX_DIMS  100

/* ----------------------------- MNI Header -----------------------------------
@NAME       : evaluate_interpolating_spline
@INPUT      : n_dims        - dimensionality of the spline, >= 1
              parameters    - u, v, w,... position of spline
              degree        - 2,3,4 for linear, quadratic, or cubic
              n_values      - number of values to interpolate at the point
              coefs         - [n_values]*[degree]*[degree]... control vertices
              n_derivs      - number of derivatives to compute
@OUTPUT     : derivs        - (n_values) *
                              (1 + n_derivs) * (1 + n_derivs) * ...
                              values and derivatives
@RETURNS    : 
@DESCRIPTION: Passes back the interpolated value and derivatives
              in the derivs array.  derivs is a 1D array that is conceptually
              multi-dimensional, indexed by v, dx, dy, dz, etc., where
              dx, dy, dz, etc. each range from 0 to n_derivs, and v ranges
              from 0 to n_values-1.
              For example, if n_dims is 3 and n_values is 4, then the
              4D index of derivs[2,0,0,0] refers to the interpolated value
              of the 3rd component of the 4 valued function.  derivs[1,0,1,1]
              refers to the derivative of the 2nd component of the 4 valued
              function with respect to v and w.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Jan 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  evaluate_interpolating_spline(
    int     n_dims,
    VIO_Real    parameters[],
    int     degree,
    int     n_values,
    VIO_Real    coefs[],
    int     n_derivs,
    VIO_Real    derivs[] )
{
    int    d, degrees[MAX_DIMS], n_derivs_list[MAX_DIMS];
    VIO_Real   *bases[MAX_DIMS];

    if( degree < 1 || degree > 4 )
    {
        print_error( "evaluate_interpolating_spline: invalid degree: %d\n",
                     degree );
        return;
    }

    if( n_dims < 1 || n_dims > MAX_DIMS )
    {
        print_error( "evaluate_interpolating_spline: invalid n dims: %d\n",
                     n_dims );
        return;
    }

    switch( degree )
    {
    case 1:   bases[0] = &constant_coefs[0][0];    break;
    case 2:   bases[0] = &linear_coefs[0][0];      break;
    case 3:   bases[0] = &quadratic_coefs[0][0];   break;
    case 4:   bases[0] = &cubic_coefs[0][0];       break;
    }

    for_less( d, 1, n_dims )
        bases[d] = bases[0];

    for_less( d, 0, n_dims )
    {
        degrees[d] = degree;
        n_derivs_list[d] = n_derivs;
    }

    spline_tensor_product( n_dims, parameters, degrees, bases, n_values, coefs,
                           n_derivs_list, derivs );
}
