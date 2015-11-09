#ifndef VOL_IO_GEOMETRY_H
#define VOL_IO_GEOMETRY_H

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/geometry.h,v 1.12 2004-10-04 20:23:51 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : geometry.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Macros for performing geometric operations on points and vectors.
              For most macros, the return value is the first argument.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#include  <volume_io/basic.h>
#include  <volume_io/geom_structs.h>

#define  Point_coord_cast(x) ((VIO_Point_coord_type) (x))
#define  Real_cast(x) ((VIO_Real) (x))

#define  RPoint_x( p )  Real_cast(Point_x(p))
#define  RPoint_y( p )  Real_cast(Point_y(p))
#define  RPoint_z( p )  Real_cast(Point_z(p))
#define  RPoint_coord( p, c )  Real_cast(Point_coord(p,c))

#define  RVector_x( p )  Real_cast(Vector_x(p))
#define  RVector_y( p )  Real_cast(Vector_y(p))
#define  RVector_z( p )  Real_cast(Point_z(p))
#define  RVector_coord( p, c )  Real_cast(Vector_coord(p,c))

/* --- private point and vector operations for use by public routines */

#define  POINT_SCALAR_EXP( result, v, op, scalar ) \
         { \
           Point_x(result) = Point_coord_cast( RPoint_x(v) op Real_cast((scalar)) ); \
           Point_y(result) = Point_coord_cast( RPoint_y(v) op Real_cast((scalar)) ); \
           Point_z(result) = Point_coord_cast( RPoint_z(v) op Real_cast((scalar)) ); \
         }

#define  POINT_EXP2( result, v1, op, v2 ) \
         { \
           Point_x(result) = Point_coord_cast( RPoint_x(v1) op RPoint_x(v2) ); \
           Point_y(result) = Point_coord_cast( RPoint_y(v1) op RPoint_y(v2) ); \
           Point_z(result) = Point_coord_cast( RPoint_z(v1) op RPoint_z(v2) ); \
         }

#define  POINT_VECTOR_EXP2( result, p, op, v ) \
         { \
           Point_x(result) = Point_coord_cast( RPoint_x(p) op RVector_x(v) ); \
           Point_y(result) = Point_coord_cast( RPoint_y(p) op RVector_y(v) ); \
           Point_z(result) = Point_coord_cast( RPoint_z(p) op RVector_z(v) ); \
         }

#define  VECTOR_SCALAR_EXP( result, v, op, scalar ) \
         { \
           Vector_x(result) = Point_coord_cast( RVector_x(v) op Real_cast(scalar) ); \
           Vector_y(result) = Point_coord_cast( RVector_y(v) op Real_cast(scalar) ); \
           Vector_z(result) = Point_coord_cast( RVector_z(v) op Real_cast(scalar) ); \
         }

#define  VECTOR_EXP2( result, v1, op, v2 ) \
         { \
           Vector_x(result) = Point_coord_cast( RVector_x(v1) op RVector_x(v2) ); \
           Vector_y(result) = Point_coord_cast( RVector_y(v1) op RVector_y(v2) ); \
           Vector_z(result) = Point_coord_cast( RVector_z(v1) op RVector_z(v2) ); \
         }

/* --- interpolate between two points, 0 results in p1, 1 results in p2 */

#define  INTERPOLATE_POINTS( interp, p1, p2, alpha ) \
         { \
           Point_x(interp) = Point_coord_cast( (1.0-Real_cast(alpha))*RPoint_x(p1) + \
                                     Real_cast(alpha)*RPoint_x(p2) );\
           Point_y(interp) = Point_coord_cast( (1.0-Real_cast(alpha))*RPoint_y(p1) + \
                                     Real_cast(alpha)*RPoint_y(p2) );\
           Point_z(interp) = Point_coord_cast( (1.0-Real_cast(alpha))*RPoint_z(p1) + \
                                     Real_cast(alpha)*RPoint_z(p2) );\
         }

/* --- interpolate between two vectors, 0 results in v1, 1 results in v2 */

#define  INTERPOLATE_VECTORS( interp, v1, v2, alpha ) \
         { \
           Vector_x(interp)= Point_coord_cast( (1.0-Real_cast(alpha))*RVector_x(v1) + \
                                      Real_cast(alpha)*RVector_x(v2) );\
           Vector_y(interp)= Point_coord_cast( (1.0-Real_cast(alpha))*RVector_y(v1) + \
                                      Real_cast(alpha)*RVector_y(v2) );\
           Vector_z(interp)= Point_coord_cast( (1.0-Real_cast(alpha))*RVector_z(v1) + \
                                      Real_cast(alpha)*RVector_z(v2) );\
         }

/* --- get a point on a ray, returning it as 'point' */

#define  GET_POINT_ON_RAY( point, origin, direction, distance ) \
         { \
           Vector_x(point)= Point_coord_cast( RPoint_x(origin) + \
                               Real_cast(distance)*RVector_x(direction) );\
           Vector_y(point)= Point_coord_cast( RPoint_y(origin) + \
                               Real_cast(distance)*RVector_y(direction) );\
           Vector_z(point)= Point_coord_cast( RPoint_z(origin) + \
                               Real_cast(distance)*RVector_z(direction) );\
         }

/* --- add and subtract points and vectors, returning correct type */

#define  ADD_POINTS( sum, p1, p2 )           POINT_EXP2( sum, p1, +, p2 )
#define  ADD_VECTORS( sum, v1, v2 )          VECTOR_EXP2( sum, v1, +, v2 )
#define  ADD_POINT_VECTOR( sum, p, v )       POINT_VECTOR_EXP2( sum, p, +, v )
#define  SUB_POINTS( diff, p1, p2 )          POINT_EXP2( diff, p1, -, p2 )
#define  SUB_VECTORS( diff, v1, v2 )         VECTOR_EXP2( diff, v1, -, v2 )
#define  SUB_POINT_VECTOR( diff, p, v )      POINT_VECTOR_EXP2( diff, p, -, v )

/* --- return the dot product of two points, two vectors, or one of each */

#define  DOT_POINTS( p1, p2 ) \
            ( RPoint_x(p1)*RPoint_x(p2) + \
              RPoint_y(p1)*RPoint_y(p2) + \
              RPoint_z(p1)*RPoint_z(p2) )

#define  DOT_VECTORS( v1, v2 ) \
            ( RVector_x(v1)*RVector_x(v2) + \
              RVector_y(v1)*RVector_y(v2) + \
              RVector_z(v1)*RVector_z(v2) )

#define  DOT_POINT_VECTOR( p, v ) \
            ( RPoint_x(p)*RVector_x(v) + \
              RPoint_y(p)*RVector_y(v) + \
              RPoint_z(p)*RVector_z(v) )

/* --- component-wise scaling of points and vectors, result is in ps/vs */

#define  SCALE_POINT( ps, p, scale )  POINT_SCALAR_EXP( ps, p, *, scale )

#define  SCALE_VECTOR( vs, v, scale )  VECTOR_SCALAR_EXP( vs, v, *, scale )

/* --- return the magnitude of a vector */

#define  MAGNITUDE( v ) Real_cast(sqrt( DOT_VECTORS(v,v) ))

/* --- set vn to the value of the vector v, normalized to length 1 */

#define  NORMALIZE_VECTOR( vn, v ) \
         { \
             VIO_Real  _mag_; \
 \
             _mag_ = MAGNITUDE( v ); \
             if( _mag_ != 0.0 ) \
                 SCALE_VECTOR( vn, v, 1.0 / _mag_ ); \
         }

/* --- set c to the cross product of vectors v1 and v2 */

#define  CROSS_VECTORS( c, v1, v2 ) \
         { \
             Vector_x(c) = Point_coord_cast( RVector_y(v1) * RVector_z(v2) - \
                              RVector_y(v2) * RVector_z(v1) ); \
             Vector_y(c) = Point_coord_cast( RVector_z(v1) * RVector_x(v2) - \
                              RVector_z(v2) * RVector_x(v1) ); \
             Vector_z(c) = Point_coord_cast( RVector_x(v1) * RVector_y(v2) - \
                              RVector_x(v2) * RVector_y(v1) ); \
         }

/* --- returns TRUE if the points/vectors are equal */

#define  EQUAL_POINTS( p1, p2 ) \
         ( Point_x(p1) == Point_x(p2) && \
           Point_y(p1) == Point_y(p2) && \
           Point_z(p1) == Point_z(p2) )

#define  EQUAL_VECTORS( v1, v2 ) \
         ( Vector_x(v1) == Vector_x(v2) && \
           Vector_y(v1) == Vector_y(v2) && \
           Vector_z(v1) == Vector_z(v2) )

/* --- given a plane normal, plane distance, and a point, returns the distance
       of the point from the plane, in fractions of the normal length */

#define  DIST_FROM_PLANE( normal, d, point ) \
         ( DOT_POINT_VECTOR(point,normal) - Real_cast(d) )

/* --- converts the 'point' to a 'vector' */

#define  CONVERT_POINT_TO_VECTOR( vector, point ) \
            fill_Vector( vector, \
                         Point_x(point), Point_y(point), Point_z(point) )

/* --- converts the 'vector' to a 'point' */

#define  CONVERT_VECTOR_TO_POINT( point, vector ) \
            fill_Point( point, \
                        Vector_x(vector), Vector_y(vector), Vector_z(vector) )

#endif /* VOL_IO_GEOMETRY_H */
