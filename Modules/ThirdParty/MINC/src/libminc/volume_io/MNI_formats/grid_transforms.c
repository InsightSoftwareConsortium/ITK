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

#define   DEGREES_CONTINUITY         2    /* -1 = Nearest; 0 = Linear; 1 = Quadratic; 2 = Cubic interpolation */
#define   SPLINE_DEGREE         ((DEGREES_CONTINUITY) + 2)

#define   N_COMPONENTS   VIO_N_DIMENSIONS /* displacement vector has 3 components */

#define   FOUR_DIMS      4

#ifdef USE_NEWTONS_METHOD
#define   INVERSE_FUNCTION_TOLERANCE     0.01
#define   INVERSE_DELTA_TOLERANCE        1.0e-5
#define   MAX_INVERSE_ITERATIONS         20
#endif

static void   evaluate_grid_volume(
    VIO_Volume         volume,
    VIO_Real           x,
    VIO_Real           y,
    VIO_Real           z,
    int                degrees_continuity,
    VIO_Real           values[],
    VIO_Real           deriv_x[],
    VIO_Real           deriv_y[],
    VIO_Real           deriv_z[] );

/* ----------------------------- MNI Header -----------------------------------
@NAME       : grid_transform_point
@INPUT      : transform
              x
              y
              z
@OUTPUT     : x_transformed
              y_transformed
              z_transformed
@RETURNS    : 
@DESCRIPTION: Applies a grid transform to the point
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Feb. 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  grid_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed )
{
    VIO_Real    displacements[N_COMPONENTS];
    VIO_Volume  volume;

    /* --- the volume that defines the transform is an offset vector,
           so evaluate the volume at the given position and add the
           resulting offset to the given position */

    if(!transform->displacement_volume) 
      return VIO_ERROR;
    
    volume = (VIO_Volume) transform->displacement_volume;

    evaluate_grid_volume( volume, x, y, z, DEGREES_CONTINUITY, displacements,
                          NULL, NULL, NULL );

    *x_transformed = x + displacements[VIO_X];
    *y_transformed = y + displacements[VIO_Y];
    *z_transformed = z + displacements[VIO_Z];
    
    return VIO_OK;
}

#ifdef USE_NEWTONS_METHOD
/* ----------------------------- MNI Header -----------------------------------
@NAME       : forward_function
@INPUT      : function_data  - contains transform info
              parameters     - x,y,z position
@OUTPUT     : values         - where x,y,z, maps to
              derivatives    - the 3 by 3 derivatives of the mapping
@RETURNS    : 
@DESCRIPTION: This function does the same thing as grid_transform_point(),
              but also gets derivatives.  This function is passed to the
              newton function solution routine to perform the inverse mapping
              of the grid transformation.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Feb.   , 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  forward_function(
    void   *function_data,
    VIO_Real   parameters[],
    VIO_Real   values[],
    VIO_Real   **derivatives )
{
    int                c;
    VIO_General_transform  *transform;
    VIO_Real               deriv_x[N_COMPONENTS], deriv_y[N_COMPONENTS];
    VIO_Real               deriv_z[N_COMPONENTS];
    VIO_Volume             volume;

    transform = (VIO_General_transform *) function_data;

    /* --- store the offset vector in values[0-2] */

    volume = (VIO_Volume) transform->displacement_volume;

    evaluate_grid_volume( volume, parameters[X], parameters[Y], parameters[Z],
                          DEGREES_CONTINUITY, values,
                          deriv_x, deriv_y, deriv_z );

    for_less( c, 0, N_COMPONENTS )
    {
        values[c] += parameters[c];   /* to get x',y',z', add offset to x,y,z */

        /*--- given the derivatives of the offset, compute the
              derivatives of (x,y,z) + offset, with respect to x,y,z */

        derivatives[c][X] = deriv_x[c];
        derivatives[c][Y] = deriv_y[c];
        derivatives[c][Z] = deriv_z[c];

        derivatives[c][c] += 1.0;    /* deriv of (x,y,z) w.r.t. x or y or z  */
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : grid_inverse_transform_point
@INPUT      : transform
              x
              y
              z
@OUTPUT     : x_transformed
              y_transformed
              z_transformed
@RETURNS    : VIO_Status
@DESCRIPTION: Applies the inverse grid transform to the point.  This is done
              by using newton-rhapson steps to find the point which maps to
              the parameters (x,y,z).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Feb. 21, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------

     There are two different versions of the grid inverse function.  I would
have hoped that my version worked best, since it uses first derivatives and
Newton's method.  However, Louis' version seems to work better, perhaps since
it matches the code he uses in minctracc to generate the grid transforms.

- David MacDonald

---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  grid_inverse_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed )
{
    VIO_Real   solution[VIO_N_DIMENSIONS];
    VIO_Real   initial_guess[VIO_N_DIMENSIONS];
    VIO_Real   desired_values[VIO_N_DIMENSIONS];

    /* --- fill in the initial guess */

    initial_guess[X] = x;
    initial_guess[Y] = y;
    initial_guess[Z] = z;

    /* --- define what the desired function values are */

    desired_values[X] = x;
    desired_values[Y] = y;
    desired_values[Z] = z;

    /* --- find the x,y,z that are mapped to the desired values */

    if( newton_root_find( VIO_N_DIMENSIONS, forward_function,
                          (void *) transform,
                          initial_guess, desired_values,
                          solution, INVERSE_FUNCTION_TOLERANCE, 
                          INVERSE_DELTA_TOLERANCE, MAX_INVERSE_ITERATIONS ))
    {
        *x_transformed = solution[X];
        *y_transformed = solution[Y];
        *z_transformed = solution[Z];
        return VIO_OK;
    }
    else  /* --- if no solution found, not sure what is reasonable to return */
    {
        *x_transformed = x;
        *y_transformed = y;
        *z_transformed = z;
        return VIO_ERROR;
    }
    return VIO_ERROR;
}
#endif

/* ----------------------------- MNI Header -----------------------------------
@NAME       : grid_inverse_transform_point_with_input_steps
@INPUT      : transform
              x
              y
              z
              input_volume_steps
@OUTPUT     : x_transformed
              y_transformed
              z_transformed
@RETURNS    : 
@DESCRIPTION: Transforms the point by the inverse of the grid transform.
              Approximates the solution using a simple iterative step
              method.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993?   Louis Collins
@MODIFIED   : 1994    David MacDonald
@MODIFIED   : 2013 June 10, Matthijs van Eede, added the possibility to pass
                            along the step sizes of the input file which is 
                            being resampled to determine the appropriate error
                            margin (ftol)
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  grid_inverse_transform_point_with_input_steps(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *input_volume_steps,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed )
{
#define  NUMBER_TRIES  10
    int    tries;
    VIO_Real   best_x, best_y, best_z;
    VIO_Real   tx, ty, tz;
    VIO_Real   gx, gy, gz;
    VIO_Real   error_x, error_y, error_z, error, smallest_e;
    VIO_Real   ftol;
    VIO_Status status=VIO_ERROR;
    int    sizes[VIO_MAX_DIMENSIONS];
    VIO_Real   steps[VIO_MAX_DIMENSIONS];
    VIO_Volume volume;
    short d, vector_dim = -1;
    int i;

    if((status=grid_transform_point( transform, x, y, z, &tx, &ty, &tz ))!=VIO_OK)
      return status;
    tx = x - (tx - x);
    ty = y - (ty - y);
    tz = z - (tz - z);

    if((status=grid_transform_point( transform, tx, ty, tz, &gx, &gy, &gz ))!=VIO_OK)
    return status;

    error_x = x - gx;
    error_y = y - gy;
    error_z = z - gz;

    tries = 0;

    smallest_e = VIO_FABS(error_x) + VIO_FABS(error_y) + VIO_FABS(error_z);
    best_x = tx;
    best_y = ty;
    best_z = tz;

    // Adapt ftol to grid step sizes. For 1mm stx volume with grid 4mm, we
    // are using ftol=0.05 (=4mm/80). For histology data at grid 0.125mm,
    // then use ftol=0.125/80=0.0015625, which is fine on 0.01mm volume. 
    // ftol = 0.05;   // good for MNI space at 1mm (grid 2mm or 4mm)
    // ftol = 0.001;  // acceptable for big brain slice (grid at 0.125, voxel at 0.01mm)

    // This is ok too:
    // Make the error a fraction of the initial residual.
    // ftol = 0.05 * smallest_e + 0.0001;


    volume = (VIO_Volume) transform->displacement_volume;
    get_volume_sizes( volume, sizes );
    get_volume_separations( volume, steps );

    /*--- find which of 4 dimensions is the vector dimension */
    
    for_less( vector_dim, 0, FOUR_DIMS ) {
      for_less( d, 0, VIO_N_DIMENSIONS ) {
        if( volume->spatial_axes[d] == vector_dim ) break;
      }
      if( d == VIO_N_DIMENSIONS ) break;
    }

    // If we have information about the step sizes of the input volume that is 
    // being resampled, base the tolerance on those instead of the step sizes
    // of the deformation grid; there can be a significant difference.
    
    ftol = -1.0;
    if( input_volume_steps != NULL){
      for_less( i, 0, 3 ) {
        if( ftol < 0 ) ftol = input_volume_steps[i];
        if( input_volume_steps[i] < ftol ) ftol = input_volume_steps[i];
      }
    }
    else {
      for_less( d, 0, FOUR_DIMS ) {
        if( d == vector_dim ) continue;
        if( sizes[d] == 1 ) continue;
        if( ftol < 0 ) ftol = steps[d];
        if( steps[d] < ftol ) ftol = steps[d];
      }
    }
    
    ftol = ftol / 80.0;
    if( ftol > 0.05 ) ftol = 0.05;   // just to be sure for large grids

    while( ++tries < NUMBER_TRIES && smallest_e > ftol ) {
        tx += 0.95 * error_x;
        ty += 0.95 * error_y;
        tz += 0.95 * error_z;

        if((status=grid_transform_point( transform, tx, ty, tz, &gx, &gy, &gz ))!=VIO_OK)
          return status;

        error_x = x - gx;
        error_y = y - gy;
        error_z = z - gz;
    
        error = VIO_FABS(error_x) + VIO_FABS(error_y) + VIO_FABS(error_z);

        if( error < smallest_e ) {
            smallest_e = error;
            best_x = tx;
            best_y = ty;
            best_z = tz;
        }
    }

    *x_transformed = best_x;
    *y_transformed = best_y;
    *z_transformed = best_z;
    return VIO_OK;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : grid_inverse_transform_point
@INPUT      : transform
              x
              y
              z
@OUTPUT     : x_transformed
              y_transformed
              z_transformed
@RETURNS    : 
@DESCRIPTION: Transforms the point by the inverse of the grid transform.
              Approximates the solution using a simple iterative step
              method.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993?   Louis Collins
@MODIFIED   : 1994    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  grid_inverse_transform_point(
    VIO_General_transform   *transform,
    VIO_Real                x,
    VIO_Real                y,
    VIO_Real                z,
    VIO_Real                *x_transformed,
    VIO_Real                *y_transformed,
    VIO_Real                *z_transformed )
{
    return grid_inverse_transform_point_with_input_steps(transform, x, y, z, 
                                           NULL,
                                           x_transformed, y_transformed, z_transformed );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : evaluate_grid_volume
@INPUT      : volume
              voxel
              degrees_continuity
@OUTPUT     : values
              derivs  (if non-NULL)
@RETURNS    : 
@DESCRIPTION: Takes a voxel space position and evaluates the value within
              the volume by nearest_neighbour, linear, quadratic, or
              cubic interpolation.  Rather than use the generic evaluate_volume
              function, this special purpose function is a bit faster.
@CREATED    : Mar. 16, 1995           David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void   evaluate_grid_volume(
    VIO_Volume         volume,
    VIO_Real           x,
    VIO_Real           y,
    VIO_Real           z,
    int            degrees_continuity,
    VIO_Real           values[],
    VIO_Real           deriv_x[],
    VIO_Real           deriv_y[],
    VIO_Real           deriv_z[] )
{
    VIO_Real     voxel[VIO_MAX_DIMENSIONS], voxel_vector[VIO_MAX_DIMENSIONS];
    int      inc0, inc1, inc2, inc3, inc[VIO_MAX_DIMENSIONS], derivs_per_value;
    int      ind0, vector_dim;
    int      start0, start1, start2, start3, inc_so_far;
    int      end0, end1, end2, end3;
    int      v0, v1, v2, v3;
    int      v, d, id, sizes[VIO_MAX_DIMENSIONS];
    int      start[VIO_MAX_DIMENSIONS];
    int      end[VIO_MAX_DIMENSIONS];
    VIO_Real     fraction[VIO_MAX_DIMENSIONS], bound, pos;
    VIO_Real     coefs[SPLINE_DEGREE*SPLINE_DEGREE*SPLINE_DEGREE*N_COMPONENTS];
    VIO_Real     values_derivs[N_COMPONENTS + N_COMPONENTS * VIO_N_DIMENSIONS];
    int is_2dslice = -1;


    convert_world_to_voxel( volume, x, y, z, voxel );

    if( get_volume_n_dimensions(volume) != FOUR_DIMS )
        handle_internal_error( "evaluate_grid_volume" );

    /*--- find which of 4 dimensions is the vector dimension */

    for_less( vector_dim, 0, FOUR_DIMS ) {
        for_less( d, 0, VIO_N_DIMENSIONS ) {
            if( volume->spatial_axes[d] == vector_dim )
                break;
        }
        if( d == VIO_N_DIMENSIONS )
            break;
    }

    get_volume_sizes( volume, sizes );

    /*--- if a 2-d slice, do best interpolation in the plane */

    for_less( d, 0, FOUR_DIMS ) {
      if( d == vector_dim ) continue;
      if( sizes[d] == 1 ) {
        is_2dslice = d;
      }
    }

    bound = (VIO_Real) degrees_continuity / 2.0;

    /*--- if near the edges, reduce the degrees of continuity.
          This is very important. Doing cubic (with a shifted
          stencil) near a boundary will cause trouble because
          the stencil needs to be centered. This is why quadratic
          is also disabled (not symmetric stencil). CL. */

    for_less( d, 0, FOUR_DIMS ) {
      if( d == is_2dslice ) continue;
      if( d == vector_dim ) continue;
      while( degrees_continuity >= -1 &&
             (voxel[d] < bound  ||
              voxel[d] > (VIO_Real) sizes[d] - 1.0 - bound ||
              bound == (VIO_Real) sizes[d] - 1.0 - bound ) ) {
        --degrees_continuity;
        if( degrees_continuity == 1 )
          degrees_continuity = 0;
        bound = (VIO_Real) degrees_continuity / 2.0;
      }
    }

    /*--- check to fill in the first derivative */

    if( degrees_continuity < 0 && deriv_x != NULL ) {
        for_less( v, 0, N_COMPONENTS ) {
            deriv_x[v] = 0.0;
            deriv_y[v] = 0.0;
            deriv_z[v] = 0.0;
        }
    }

    /*--- check if outside */

    for( d = 0; d < FOUR_DIMS; d++ ) {
      if( d == vector_dim ) continue;
      if( voxel[d] < -0.5 || voxel[d] > sizes[d]-0.5 ) {
        for_less( v, 0, N_COMPONENTS ) {
           values[v] = 0.0;
        }
        return;
      }
    }

    /*--- determine the starting positions in the volume to grab control
          vertices */

    id = 0;
    for_less( d, 0, FOUR_DIMS ) {
        if( d == vector_dim ) continue;
        if( d == is_2dslice ) {
            start[d] = 0;
            end[d] = 1;
        } else {
            pos = voxel[d] - bound;
            start[d] = VIO_FLOOR( pos );
            if( start[d] < 0 ) {
                start[d] = 0;
            } else if( start[d]+degrees_continuity+1 >= sizes[d] ) {
                start[d] = sizes[d] - degrees_continuity - 2;
            }
            end[d] = start[d] + degrees_continuity + 2;
            fraction[id] = pos - (double) start[d];
            ++id;
        }
    }

    /*--- create the strides */

    start[vector_dim] = 0;
    end[vector_dim] = N_COMPONENTS;

    inc_so_far = N_COMPONENTS;
    for_down( d, FOUR_DIMS-1, 0 ) {
        if( d != vector_dim ) {
            inc[d] = inc_so_far;
            inc_so_far *= ( end[d] - start[d] );
        }
    }

    /*--- copy stride arrays to variables for speed */

    inc[vector_dim] = 1;

    start0 = start[0];
    start1 = start[1];
    start2 = start[2];
    start3 = start[3];

    end0 = end[0];
    end1 = end[1];
    end2 = end[2];
    end3 = end[3];

    inc0 = inc[0] - inc[1] * (end1 - start1);
    inc1 = inc[1] - inc[2] * (end2 - start2);
    inc2 = inc[2] - inc[3] * (end3 - start3);
    inc3 = inc[3];

    /*--- extract values from volume */

    ind0 = 0;

    for_less( v0, start0, end0 ) {
        for_less( v1, start1, end1 ) {
            for_less( v2, start2, end2 ) {
                for_less( v3, start3, end3 ) {
                    GET_VALUE_4D_TYPED( coefs[ind0], (VIO_Real), volume,
                                        v0, v1, v2, v3 );
                    ind0 += inc3;
                }
                ind0 += inc2;
            }
            ind0 += inc1;
        }
        ind0 += inc0;
    }

    /*--- interpolate values */

    if( degrees_continuity == -1 ) {
        for_less( v, 0, N_COMPONENTS )
            values[v] = coefs[v];
    } else {
        if( is_2dslice == -1 ) {
          evaluate_interpolating_spline( VIO_N_DIMENSIONS, fraction,
                                         degrees_continuity + 2, 
                                         N_COMPONENTS, coefs, 0, values_derivs );
        } else {
          evaluate_interpolating_spline( VIO_N_DIMENSIONS-1, fraction,
                                         degrees_continuity + 2, 
                                         N_COMPONENTS, coefs, 0, values_derivs );
        }

        /*--- extract values and derivatives from values_derivs */

        if( deriv_x != NULL )
            derivs_per_value = 8;
        else
            derivs_per_value = 1;

        for_less( v, 0, N_COMPONENTS ) {
            values[v] = values_derivs[v*derivs_per_value];
        }

        if( deriv_x != NULL )
        {
            for_less( v, 0, N_COMPONENTS )
            {
                id = 0;
                for_less( d, 0, FOUR_DIMS )
                {
                    if( d != vector_dim )
                    {
                        voxel_vector[d] = values_derivs[v*8 + (4>>id)];
                        ++id;
                    }
                    else
                        voxel_vector[d] = 0.0;
                }
             
                convert_voxel_normal_vector_to_world( volume, voxel_vector,
                                    &deriv_x[v], &deriv_y[v], &deriv_z[v] );
            }
        }
    }
}
