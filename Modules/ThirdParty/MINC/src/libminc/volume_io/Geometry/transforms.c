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
@NAME       : make_identity_transform
@INPUT      : 
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Fills in the transform with the identity matrix.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  make_identity_transform( VIO_Transform   *transform )
{
    Transform_elem( *transform, 0, 0 ) = 1.0;
    Transform_elem( *transform, 0, 1 ) = 0.0;
    Transform_elem( *transform, 0, 2 ) = 0.0;
    Transform_elem( *transform, 0, 3 ) = 0.0;
    Transform_elem( *transform, 1, 0 ) = 0.0;
    Transform_elem( *transform, 1, 1 ) = 1.0;
    Transform_elem( *transform, 1, 2 ) = 0.0;
    Transform_elem( *transform, 1, 3 ) = 0.0;
    Transform_elem( *transform, 2, 0 ) = 0.0;
    Transform_elem( *transform, 2, 1 ) = 0.0;
    Transform_elem( *transform, 2, 2 ) = 1.0;
    Transform_elem( *transform, 2, 3 ) = 0.0;
    Transform_elem( *transform, 3, 0 ) = 0.0;
    Transform_elem( *transform, 3, 1 ) = 0.0;
    Transform_elem( *transform, 3, 2 ) = 0.0;
    Transform_elem( *transform, 3, 3 ) = 1.0;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : close_to_identity
@INPUT      : transform
@OUTPUT     : 
@RETURNS    : TRUE if transform is close to identity
@DESCRIPTION: 
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  close_to_identity(
    VIO_Transform   *transform )
{
#define   TOLERANCE   0.001
    VIO_BOOL    close;
    VIO_Real       expected_val;
    int        i, j;

    close = TRUE;

    for_less( i, 0, 4 )
    {
        for_less( j, 0, 4 )
        {
            if( i == j )
                expected_val = 1.0;
            else
                expected_val = 0.0;

            if( Transform_elem(*transform,i,j) < expected_val - TOLERANCE ||
                Transform_elem(*transform,i,j) > expected_val + TOLERANCE )
            {
                close = FALSE;
            }
        }
    }

    return( close );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_origin
@INPUT      : transform
@OUTPUT     : origin
@RETURNS    : 
@DESCRIPTION: Passes back the origin of the transform, i.e., where the 
              point (0,0,0) would be transformed to.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_origin(
    VIO_Transform   *transform,
    VIO_Point       *origin )
{
    fill_Point( *origin,
                Transform_elem(*transform,0,3),
                Transform_elem(*transform,1,3),
                Transform_elem(*transform,2,3) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_origin
@INPUT      : origin
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the origin of the transform, i.e., where the
              point (0,0,0) would be transformed to.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_origin(
    VIO_Transform   *transform,
    VIO_Point       *origin )
{
    Transform_elem(*transform,0,3) = (VIO_Transform_elem_type) Point_x(*origin);
    Transform_elem(*transform,1,3) = (VIO_Transform_elem_type) Point_y(*origin);
    Transform_elem(*transform,2,3) = (VIO_Transform_elem_type) Point_z(*origin);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_origin_real
@INPUT      : transform
@OUTPUT     : origin
@RETURNS    : 
@DESCRIPTION: Passes back the origin of the transform, i.e., where the 
              point (0,0,0) would be transformed to.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_origin_real(
    VIO_Transform   *transform,
    VIO_Real        origin[] )
{
    origin[VIO_X] = Transform_elem(*transform,0,3);
    origin[VIO_Y] = Transform_elem(*transform,1,3);
    origin[VIO_Z] = Transform_elem(*transform,2,3);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_x_axis
@INPUT      : transform
@OUTPUT     : x_axis
@RETURNS    : 
@DESCRIPTION: Passes back the x axis of the transform, i.e., the vector
              to which the vector (1,0,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_x_axis(
    VIO_Transform   *transform,
    VIO_Vector      *x_axis )
{
    fill_Vector( *x_axis,
                 Transform_elem(*transform,0,0),
                 Transform_elem(*transform,1,0),
                 Transform_elem(*transform,2,0) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_x_axis_real
@INPUT      : transform
@OUTPUT     : x_axis
@RETURNS    : 
@DESCRIPTION: Passes back the x axis of the transform, i.e., the vector
              to which the vector (1,0,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_x_axis_real(
    VIO_Transform   *transform,
    VIO_Real        x_axis[] )
{
    x_axis[VIO_X] = Transform_elem(*transform,0,0);
    x_axis[VIO_Y] = Transform_elem(*transform,1,0);
    x_axis[VIO_Z] = Transform_elem(*transform,2,0);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_x_axis
@INPUT      : x_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the x axis of the transform, i.e., the vector
              to which the vector (1,0,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_x_axis(
    VIO_Transform   *transform,
    VIO_Vector      *x_axis )
{
    Transform_elem(*transform,0,0) = (VIO_Transform_elem_type) Vector_x(*x_axis);
    Transform_elem(*transform,1,0) = (VIO_Transform_elem_type) Vector_y(*x_axis);
    Transform_elem(*transform,2,0) = (VIO_Transform_elem_type) Vector_z(*x_axis);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_x_axis_real
@INPUT      : x_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the x axis of the transform, i.e., the vector
              to which the vector (1,0,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_x_axis_real(
    VIO_Transform   *transform,
    VIO_Real        x_axis[] )
{
    Transform_elem(*transform,0,0) = (VIO_Transform_elem_type) x_axis[VIO_X];
    Transform_elem(*transform,1,0) = (VIO_Transform_elem_type) x_axis[VIO_Y];
    Transform_elem(*transform,2,0) = (VIO_Transform_elem_type) x_axis[VIO_Z];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_y_axis
@INPUT      : transform
@OUTPUT     : y_axis
@RETURNS    : 
@DESCRIPTION: Passes back the y axis of the transform, i.e., the vector
              to which the vector (0,1,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_y_axis(
    VIO_Transform   *transform,
    VIO_Vector      *y_axis )
{
    fill_Vector( *y_axis,
                 Transform_elem(*transform,0,1),
                 Transform_elem(*transform,1,1),
                 Transform_elem(*transform,2,1) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_y_axis_real
@INPUT      : transform
@OUTPUT     : y_axis
@RETURNS    : 
@DESCRIPTION: Passes back the y axis of the transform, i.e., the vector
              to which the vector (0,1,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_y_axis_real(
    VIO_Transform   *transform,
    VIO_Real        y_axis[] )
{
    y_axis[VIO_X] = Transform_elem(*transform,0,1);
    y_axis[VIO_Y] = Transform_elem(*transform,1,1);
    y_axis[VIO_Z] = Transform_elem(*transform,2,1);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_y_axis
@INPUT      : y_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the y axis of the transform, i.e., the vector
              to which the vector (0,1,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_y_axis(
    VIO_Transform   *transform,
    VIO_Vector      *y_axis )
{
    Transform_elem(*transform,0,1) = (VIO_Transform_elem_type) Vector_x(*y_axis);
    Transform_elem(*transform,1,1) = (VIO_Transform_elem_type) Vector_y(*y_axis);
    Transform_elem(*transform,2,1) = (VIO_Transform_elem_type) Vector_z(*y_axis);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_y_axis_real
@INPUT      : y_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the y axis of the transform, i.e., the vector
              to which the vector (0,1,0) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_y_axis_real(
    VIO_Transform   *transform,
    VIO_Real        y_axis[] )
{
    Transform_elem(*transform,0,1) = (VIO_Transform_elem_type) y_axis[VIO_X];
    Transform_elem(*transform,1,1) = (VIO_Transform_elem_type) y_axis[VIO_Y];
    Transform_elem(*transform,2,1) = (VIO_Transform_elem_type) y_axis[VIO_Z];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_z_axis
@INPUT      : transform
@OUTPUT     : z_axis
@RETURNS    : 
@DESCRIPTION: Passes back the z axis of the transform, i.e., the vector
              to which the vector (0,0,1) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_z_axis(
    VIO_Transform   *transform,
    VIO_Vector      *z_axis )
{
    fill_Vector( *z_axis,
                 Transform_elem(*transform,0,2),
                 Transform_elem(*transform,1,2),
                 Transform_elem(*transform,2,2) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_transform_z_axis_real
@INPUT      : transform
@OUTPUT     : z_axis
@RETURNS    : 
@DESCRIPTION: Passes back the z axis of the transform, i.e., the vector
              to which the vector (0,0,1) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  get_transform_z_axis_real(
    VIO_Transform   *transform,
    VIO_Real        z_axis[] )
{
    z_axis[VIO_X] = Transform_elem(*transform,0,2);
    z_axis[VIO_Y] = Transform_elem(*transform,1,2);
    z_axis[VIO_Z] = Transform_elem(*transform,2,2);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_z_axis
@INPUT      : z_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the z axis of the transform, i.e., the vector
              to which the vector (0,0,1) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_z_axis(
    VIO_Transform   *transform,
    VIO_Vector      *z_axis )
{
    Transform_elem(*transform,0,2) = (VIO_Transform_elem_type) Vector_x(*z_axis);
    Transform_elem(*transform,1,2) = (VIO_Transform_elem_type) Vector_y(*z_axis);
    Transform_elem(*transform,2,2) = (VIO_Transform_elem_type) Vector_z(*z_axis);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_transform_z_axis_real
@INPUT      : z_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Sets the z axis of the transform, i.e., the vector
              to which the vector (0,0,1) would be transformed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 20, 1997            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_transform_z_axis_real(
    VIO_Transform   *transform,
    VIO_Real        z_axis[] )
{
    Transform_elem(*transform,0,2) = (VIO_Transform_elem_type) z_axis[VIO_X];
    Transform_elem(*transform,1,2) = (VIO_Transform_elem_type) z_axis[VIO_Y];
    Transform_elem(*transform,2,2) = (VIO_Transform_elem_type) z_axis[VIO_Z];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : make_change_to_bases_transform
@INPUT      : origin
              x_axis
              y_axis
              z_axis
@OUTPUT     : transform
@RETURNS    : 
@DESCRIPTION: Creates a transform that translates the point (0,0,0) to the
              specified origin.  The point (1,0,0) is transformed to the
              specified origin plus the specified x_axis.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void   make_change_to_bases_transform(
    VIO_Point      *origin,
    VIO_Vector     *x_axis,
    VIO_Vector     *y_axis,
    VIO_Vector     *z_axis,
    VIO_Transform  *transform )
{
    Transform_elem( *transform,0,0 ) = (VIO_Transform_elem_type)Vector_x( *x_axis );
    Transform_elem( *transform,0,1 ) = (VIO_Transform_elem_type)Vector_x( *y_axis );
    Transform_elem( *transform,0,2 ) = (VIO_Transform_elem_type)Vector_x( *z_axis );
    Transform_elem( *transform,0,3 ) = (VIO_Transform_elem_type)Point_x( *origin );
    Transform_elem( *transform,1,0 ) = (VIO_Transform_elem_type)Vector_y( *x_axis );
    Transform_elem( *transform,1,1 ) = (VIO_Transform_elem_type)Vector_y( *y_axis );
    Transform_elem( *transform,1,2 ) = (VIO_Transform_elem_type)Vector_y( *z_axis );
    Transform_elem( *transform,1,3 ) = (VIO_Transform_elem_type)Point_y( *origin );
    Transform_elem( *transform,2,0 ) = (VIO_Transform_elem_type)Vector_z( *x_axis );
    Transform_elem( *transform,2,1 ) = (VIO_Transform_elem_type)Vector_z( *y_axis );
    Transform_elem( *transform,2,2 ) = (VIO_Transform_elem_type)Vector_z( *z_axis );
    Transform_elem( *transform,2,3 ) = (VIO_Transform_elem_type)Point_z( *origin );
    Transform_elem( *transform,3,0 ) = 0.0;
    Transform_elem( *transform,3,1 ) = 0.0;
    Transform_elem( *transform,3,2 ) = 0.0;
    Transform_elem( *transform,3,3 ) = 1.0;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : make_change_from_bases_transform
@INPUT      : origin
              x_axis
              y_axis
              z_axis
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Makes a change of bases transform, so that points are transformed
              to be relative to the given axes.  For instance the origin is
              transformed by the change of bases transform to be point (0,0,0).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void   make_change_from_bases_transform(
    VIO_Point      *origin,
    VIO_Vector     *x_axis,
    VIO_Vector     *y_axis,
    VIO_Vector     *z_axis,
    VIO_Transform  *transform )
{
    Transform_elem(*transform,0,0) = (VIO_Transform_elem_type)Vector_x( *x_axis );
    Transform_elem(*transform,0,1) = (VIO_Transform_elem_type)Vector_y( *x_axis );
    Transform_elem(*transform,0,2) = (VIO_Transform_elem_type)Vector_z( *x_axis );
    Transform_elem(*transform,0,3) = (VIO_Transform_elem_type)
                                        - DOT_POINT_VECTOR( *origin, *x_axis );
    Transform_elem(*transform,1,0) = (VIO_Transform_elem_type)Vector_x( *y_axis );
    Transform_elem(*transform,1,1) = (VIO_Transform_elem_type)Vector_y( *y_axis );
    Transform_elem(*transform,1,2) = (VIO_Transform_elem_type)Vector_z( *y_axis );
    Transform_elem(*transform,1,3) = (VIO_Transform_elem_type)
                                        - DOT_POINT_VECTOR( *origin, *y_axis );
    Transform_elem(*transform,2,0) = (VIO_Transform_elem_type)Vector_x( *z_axis );
    Transform_elem(*transform,2,1) = (VIO_Transform_elem_type)Vector_y( *z_axis );
    Transform_elem(*transform,2,2) = (VIO_Transform_elem_type)Vector_z( *z_axis );
    Transform_elem(*transform,2,3) = (VIO_Transform_elem_type)
                                        - DOT_POINT_VECTOR( *origin, *z_axis );
    Transform_elem(*transform,3,0) = 0.0;
    Transform_elem(*transform,3,1) = 0.0;
    Transform_elem(*transform,3,2) = 0.0;
    Transform_elem(*transform,3,3) = 1.0;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : concat_transforms
@INPUT      : t1
              t2
@OUTPUT     : result
@RETURNS    : 
@DESCRIPTION: Concatenates the two transforms returning the result in the
              argument, 'result'.  Correctly handles the case where the
              result transform is also one of the operands.  Transforming
              a point by the 'result' transform will give the same point
              as first transforming the point by 't1', then by 't2'.
@METHOD     : Multiplies result = t2 * t1
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void   concat_transforms(
    VIO_Transform   *result,
    VIO_Transform   *t1,
    VIO_Transform   *t2 )
{
    int         i, j, k;
    VIO_Real        sum;
    VIO_BOOL     result_is_also_an_arg;
    VIO_Transform   tmp, *t;

    /*--- check if the result transform is same as one of the arguments */

    if( result == t1 || result == t2 )
    {
        result_is_also_an_arg = TRUE;
        t = &tmp;
    }
    else
    {
        result_is_also_an_arg = FALSE;
        t = result;
    }

    /*--- perform multiplication */

    for_less( i, 0, 4 )
    {
        for_less( j, 0, 4 )
        {
            sum = 0.0;
            for_less( k, 0, 4 )
            {
                sum += Transform_elem( *t2, i, k ) *
                       Transform_elem( *t1, k, j );
            }
            Transform_elem( *t, i, j ) = sum;
        }
    }

    if( result_is_also_an_arg )
        *result = tmp;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : homogenous_transform_point
@INPUT      : transform
              x
              y
              z
              w
@OUTPUT     : x_trans
              y_trans
              z_trans
@RETURNS    : VIO_Status
@DESCRIPTION: Transforms the point (x,y,z,w) by the homogenous transform
              matrix, resulting in (x_trans,y_trans,z_trans).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  homogenous_transform_point(
    VIO_Transform  *transform,
    VIO_Real       x,
    VIO_Real       y,
    VIO_Real       z,
    VIO_Real       w,
    VIO_Real       *x_trans,
    VIO_Real       *y_trans,
    VIO_Real       *z_trans )
{
    VIO_Real       w_trans;

    *x_trans = Transform_elem(*transform,0,0) * x +
               Transform_elem(*transform,0,1) * y +
               Transform_elem(*transform,0,2) * z +
               Transform_elem(*transform,0,3) * w;

    *y_trans = Transform_elem(*transform,1,0) * x +
               Transform_elem(*transform,1,1) * y +
               Transform_elem(*transform,1,2) * z +
               Transform_elem(*transform,1,3) * w;

    *z_trans = Transform_elem(*transform,2,0) * x +
               Transform_elem(*transform,2,1) * y +
               Transform_elem(*transform,2,2) * z +
               Transform_elem(*transform,2,3) * w;

    w_trans =  Transform_elem(*transform,3,0) * x +
               Transform_elem(*transform,3,1) * y +
               Transform_elem(*transform,3,2) * z +
               Transform_elem(*transform,3,3) * w;

    if( w_trans != 0.0 && w_trans != 1.0 )
    {
        *x_trans /= w_trans;
        *y_trans /= w_trans;
        *z_trans /= w_trans;
    }
    return VIO_OK;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : transform_point
@INPUT      : transform
              x
              y
              z
@OUTPUT     : x_trans
              y_trans
              z_trans
@RETURNS    : VIO_Status
@DESCRIPTION: Transforms the point (x,y,z) by the transform matrix, resulting
              in (x_trans,y_trans,z_trans).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  transform_point(
    VIO_Transform  *transform,
    VIO_Real       x,
    VIO_Real       y,
    VIO_Real       z,
    VIO_Real       *x_trans,
    VIO_Real       *y_trans,
    VIO_Real       *z_trans )
{
    return homogenous_transform_point( transform, x, y, z, 1.0,
                                x_trans, y_trans, z_trans );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : transform_vector
@INPUT      : transform
              x
              y
              z
@OUTPUT     : x_trans
              y_trans
              z_trans
@RETURNS    : 
@DESCRIPTION: Transforms the vector by the specified transform.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  transform_vector(
    VIO_Transform  *transform,
    VIO_Real       x,
    VIO_Real       y,
    VIO_Real       z,
    VIO_Real       *x_trans,
    VIO_Real       *y_trans,
    VIO_Real       *z_trans )
{
    return homogenous_transform_point( transform, x, y, z, 0.0,
                                x_trans, y_trans, z_trans );
}
