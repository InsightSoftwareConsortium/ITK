#ifndef VOL_IO_MULTI_DIM_H
#define VOL_IO_MULTI_DIM_H 

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/multidim.h,v 1.7 2005-05-19 21:19:27 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : multidim.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Multidimensional variable type arrays.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 14, 1995   David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_MAX_DIMENSIONS     5

/* -------------------------- Data_types ------------------------- */

typedef  enum  { VIO_NO_DATA_TYPE,
                 VIO_UNSIGNED_BYTE,
                 VIO_SIGNED_BYTE,
                 VIO_UNSIGNED_SHORT,
                 VIO_SIGNED_SHORT,
                 VIO_UNSIGNED_INT,
                 VIO_SIGNED_INT,
                 VIO_FLOAT,
                 VIO_DOUBLE,
                 VIO_MAX_DATA_TYPE }   VIO_Data_types;

typedef  struct
{
    int                     n_dimensions;
    int                     sizes[VIO_MAX_DIMENSIONS];
    VIO_Data_types          data_type;
    void                    *data;
} VIO_multidim_array;

/* ------------------------- set value ---------------------------- */

/* --- private macros */

#define  SET_ONE( array, type, asterisks, subscripts, value )   \
         (((type asterisks) ((array).data))  subscripts = (type) (value))

#define  SET_GIVEN_DIM( array, asterisks, subscripts, value )   \
         switch( (array).data_type )  \
         {  \
         case VIO_UNSIGNED_BYTE:  \
             SET_ONE( array, unsigned char, asterisks, subscripts, value);\
             break;  \
         case VIO_SIGNED_BYTE:  \
             SET_ONE( array, signed char, asterisks, subscripts, value);\
             break;  \
         case VIO_UNSIGNED_SHORT:  \
             SET_ONE( array, unsigned short, asterisks, subscripts, value);\
             break;  \
         case VIO_SIGNED_SHORT:  \
             SET_ONE( array, signed short, asterisks, subscripts, value);\
             break;  \
         case VIO_UNSIGNED_INT:  \
             SET_ONE( array, unsigned int, asterisks, subscripts, value);\
             break;  \
         case VIO_SIGNED_INT:  \
             SET_ONE( array, signed int, asterisks, subscripts, value);\
             break;  \
         case VIO_FLOAT:  \
             SET_ONE( array, float, asterisks, subscripts, value);\
             break;  \
         default: \
         case VIO_DOUBLE:  \
             SET_ONE( array, double, asterisks, subscripts, value);\
             break;  \
         }

#define  GET_MULTIDIM_TYPE_1D( array, type, v0 )   \
         (((type *) ((array).data)) [v0])

#define  GET_MULTIDIM_TYPE_2D( array, type, v0, v1 )   \
         (((type **) ((array).data)) [v0][v1])

#define  GET_MULTIDIM_TYPE_3D( array, type, v0, v1, v2 )   \
         (((type ***) ((array).data)) [v0][v1][v2])

#define  GET_MULTIDIM_TYPE_4D( array, type, v0, v1, v2, v3 )   \
         (((type ****) ((array).data)) [v0][v1][v2][v3])

#define  GET_MULTIDIM_TYPE_5D( array, type, v0, v1, v2, v3, v4 )   \
         (((type *****) ((array).data)) [v0][v1][v2][v3][v4])

#define  SET_MULTIDIM_TYPE_1D( array, type, v0, value )   \
           (GET_MULTIDIM_TYPE_1D( array, type, v0 ) = (type) (value))

#define  SET_MULTIDIM_TYPE_2D( array, type, v0, v1, value )   \
           (GET_MULTIDIM_TYPE_2D( array, type, v0, v1 ) = (type) (value))

#define  SET_MULTIDIM_TYPE_3D( array, type, v0, v1, v2, value )   \
           (GET_MULTIDIM_TYPE_3D( array, type, v0, v1, v2 ) = (type) (value))

#define  SET_MULTIDIM_TYPE_4D( array, type, v0, v1, v2, v3, value )   \
        (GET_MULTIDIM_TYPE_4D( array, type, v0, v1, v2, v3 ) = (type) (value))

#define  SET_MULTIDIM_TYPE_5D( array, type, v0, v1, v2, v3, v4, value )   \
      (GET_MULTIDIM_TYPE_5D( array, type, v0, v1, v2, v3, v4 ) = (type) (value))

/* --- public macros to set the [x][y]... voxel of 'array' to 'value' */

#define  SET_MULTIDIM_1D( array, x, value )       \
           SET_GIVEN_DIM( array, *, [x], value )

#define  SET_MULTIDIM_2D( array, x, y, value )       \
           SET_GIVEN_DIM( array, **, [x][y], value )

#define  SET_MULTIDIM_3D( array, x, y, z, value )       \
           SET_GIVEN_DIM( array, ***, [x][y][z], value )

#define  SET_MULTIDIM_4D( array, x, y, z, t, value )       \
           SET_GIVEN_DIM( array, ****, [x][y][z][t], value )

#define  SET_MULTIDIM_5D( array, x, y, z, t, v, value )       \
           SET_GIVEN_DIM( array, *****, [x][y][z][t][v], value )

/* --- same as previous, but don't have to know dimensions of volume */

#define  SET_MULTIDIM( array, x, y, z, t, v, value )       \
         switch( (array).n_dimensions ) \
         { \
         default: \
         case 1:  SET_MULTIDIM_1D( array, x, value );              break; \
         case 2:  SET_MULTIDIM_2D( array, x, y, value );           break; \
         case 3:  SET_MULTIDIM_3D( array, x, y, z, value );        break; \
         case 4:  SET_MULTIDIM_4D( array, x, y, z, t, value );     break; \
         case 5:  SET_MULTIDIM_5D( array, x, y, z, t, v, value );  break; \
         }

/* ------------------------- get multidim value ------------------------ */

/* --- private macros */

#define  GET_ONE( value, vtype, array, type, asterisks, subscripts )   \
         (value) = vtype (((type asterisks) ((array).data))  subscripts)

#define  GET_GIVEN_DIM( value, vtype, array, asterisks, subscripts )   \
         switch( (array).data_type )  \
         {  \
         case VIO_UNSIGNED_BYTE:  \
             GET_ONE( value, vtype, array, unsigned char, asterisks, subscripts );\
             break;  \
         case VIO_SIGNED_BYTE:  \
             GET_ONE( value, vtype, array, signed char, asterisks, subscripts );\
             break;  \
         case VIO_UNSIGNED_SHORT:  \
             GET_ONE( value, vtype, array, unsigned short, asterisks, subscripts );\
             break;  \
         case VIO_SIGNED_SHORT:  \
             GET_ONE( value, vtype, array, signed short, asterisks, subscripts );\
             break;  \
         case VIO_UNSIGNED_INT:  \
             GET_ONE( value, vtype, array, unsigned int, asterisks, subscripts );\
             break;  \
         case VIO_SIGNED_INT:  \
             GET_ONE( value, vtype, array, signed int, asterisks, subscripts );\
             break;  \
         case VIO_FLOAT:  \
             GET_ONE( value, vtype, array, float, asterisks, subscripts );\
             break;  \
         default: \
         case VIO_DOUBLE:  \
             GET_ONE( value, vtype, array, double, asterisks, subscripts );\
             break;  \
         }

/* --- public macros to place the [x][y]...'th voxel of 'array' in 'value' */

#define  GET_MULTIDIM_1D( value, vtype, array, x )       \
           GET_GIVEN_DIM( value, vtype, array, *, [x] )

#define  GET_MULTIDIM_2D( value, vtype, array, x, y )       \
           GET_GIVEN_DIM( value, vtype, array, **, [x][y] )

#define  GET_MULTIDIM_3D( value, vtype, array, x, y, z )       \
           GET_GIVEN_DIM( value, vtype, array, ***, [x][y][z] )

#define  GET_MULTIDIM_4D( value, vtype, array, x, y, z, t )       \
           GET_GIVEN_DIM( value, vtype, array, ****, [x][y][z][t] )

#define  GET_MULTIDIM_5D( value, vtype, array, x, y, z, t, v )       \
           GET_GIVEN_DIM( value, vtype, array, *****, [x][y][z][t][v] )

/* --- same as previous, but no need to know array dimensions */

#define  GET_MULTIDIM( value, vtype, array, x, y, z, t, v )       \
         switch( (array).n_dimensions ) \
         { \
         default: \
         case 1:  GET_MULTIDIM_1D( value, vtype, array, x );              break; \
         case 2:  GET_MULTIDIM_2D( value, vtype, array, x, y );           break; \
         case 3:  GET_MULTIDIM_3D( value, vtype, array, x, y, z );        break; \
         case 4:  GET_MULTIDIM_4D( value, vtype, array, x, y, z, t );     break; \
         case 5:  GET_MULTIDIM_5D( value, vtype, array, x, y, z, t, v );  break; \
         }

/* ------------------------- get multidim ptr ------------------------ */

/* --- private macros */

#define  GET_ONE_PTR( ptr, array, type, asterisks, subscripts )   \
         (ptr) = (void *) (&(((type asterisks) ((array).data))  subscripts))

#define  GET_GIVEN_DIM_PTR( ptr, array, asterisks, subscripts )   \
         switch( (array).data_type )  \
         {  \
         case VIO_UNSIGNED_BYTE:  \
             GET_ONE_PTR( ptr, array, unsigned char, asterisks, subscripts );\
             break;  \
         case VIO_SIGNED_BYTE:  \
             GET_ONE_PTR( ptr, array, signed char, asterisks, subscripts );\
             break;  \
         case VIO_UNSIGNED_SHORT:  \
             GET_ONE_PTR( ptr, array, unsigned short, asterisks, subscripts );\
             break;  \
         case VIO_SIGNED_SHORT:  \
             GET_ONE_PTR( ptr, array, signed short, asterisks, subscripts );\
             break;  \
         case VIO_UNSIGNED_INT:  \
             GET_ONE_PTR( ptr, array, unsigned int, asterisks, subscripts );\
             break;  \
         case VIO_SIGNED_INT:  \
             GET_ONE_PTR( ptr, array, signed int, asterisks, subscripts );\
             break;  \
         case VIO_FLOAT:  \
             GET_ONE_PTR( ptr, array, float, asterisks, subscripts );\
             break;  \
         default: \
         case VIO_DOUBLE:  \
             GET_ONE_PTR( ptr, array, double, asterisks, subscripts );\
             break;  \
         }

/* --- public macros to return a pointer to the [x][y]'th voxel of the
       'array', and place it in 'ptr' */

#define  GET_MULTIDIM_PTR_1D( ptr, array, x )       \
           GET_GIVEN_DIM_PTR( ptr, array, *, [x] )

#define  GET_MULTIDIM_PTR_2D( ptr, array, x, y )       \
           GET_GIVEN_DIM_PTR( ptr, array, **, [x][y] )

#define  GET_MULTIDIM_PTR_3D( ptr, array, x, y, z )       \
           GET_GIVEN_DIM_PTR( ptr, array, ***, [x][y][z] )

#define  GET_MULTIDIM_PTR_4D( ptr, array, x, y, z, t )       \
           GET_GIVEN_DIM_PTR( ptr, array, ****, [x][y][z][t] )

#define  GET_MULTIDIM_PTR_5D( ptr, array, x, y, z, t, v )       \
           GET_GIVEN_DIM_PTR( ptr, array, *****, [x][y][z][t][v] )

/* --- same as previous, but no need to know array dimensions */

#define  GET_MULTIDIM_PTR( ptr, array, x, y, z, t, v )       \
         switch( (array).n_dimensions ) \
         { \
         default: \
         case 1:  GET_MULTIDIM_PTR_1D( ptr, array, x );              break; \
         case 2:  GET_MULTIDIM_PTR_2D( ptr, array, x, y );           break; \
         case 3:  GET_MULTIDIM_PTR_3D( ptr, array, x, y, z );        break; \
         case 4:  GET_MULTIDIM_PTR_4D( ptr, array, x, y, z, t );     break; \
         case 5:  GET_MULTIDIM_PTR_5D( ptr, array, x, y, z, t, v );  break; \
         }

#endif /* __MULTIDIM_H__*/
