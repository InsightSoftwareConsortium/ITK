#ifndef VOL_IO_VOLUME_H
#define VOL_IO_VOLUME_H

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/volume.h,v 1.57 2006-04-07 14:47:15 rotor Exp $
---------------------------------------------------------------------------- */


/* ----------------------------- MNI Header -----------------------------------
@NAME       : volume.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Types for use in dealing with volumes.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#ifdef HAVE_MINC1
#include  <minc.h>
#endif /*HAVE_MINC1*/

#ifdef HAVE_MINC2
#include <minc2.h>
#endif

#include  <volume_io/transforms.h>
#include  <volume_io/multidim.h>

typedef  struct
{
    VIO_Real global_image_range[2];
    VIO_STR  dimension_names[VIO_MAX_DIMENSIONS];
    VIO_BOOL use_starts_set;
    VIO_BOOL use_volume_starts_and_steps;
    VIO_BOOL is_labels;
    /*Mostly for debugging*/
    VIO_BOOL    prefer_minc2_api;
} minc_output_options;

extern  VIO_STR   XYZ_dimension_names[];
extern  VIO_STR   File_order_dimension_names[];

/* -------------------------- volume struct --------------------- */

#define  ANY_SPATIAL_DIMENSION   "any_spatial_dimension"

#define MI_UNKNOWN_SPACE    "unknown___"

#ifndef HAVE_MINC1
/*Varios definitions to compile Volume_io without MINC1 API*/
#define MI_ORIGINAL_TYPE 0
typedef int nc_type;
#define MAX_VAR_DIMS       100
#define MIxspace           "xspace"
#define MIyspace           "yspace"
#define MIzspace           "zspace"
#define MItime             "time"
#define MItfrequency       "tfrequency"
#define MIxfrequency       "xfrequency"
#define MIyfrequency       "yfrequency"
#define MIzfrequency       "zfrequency"
#define MIvector_dimension "vector_dimension"

/*TODO: use minc2 definitions here?*/
#define NC_BYTE   1
#define NC_CHAR   2
#define NC_SHORT  3
#define NC_INT    4
#define NC_FLOAT  5
#define NC_DOUBLE 6

#endif /*HAVE_MINC1*/

#include  <volume_io/volume_cache.h>


typedef  struct
{
    VIO_BOOL                is_cached_volume;
    VIO_volume_cache_struct cache;

    VIO_multidim_array      array;

    VIO_STR                 dimension_names[VIO_MAX_DIMENSIONS];
    int                     spatial_axes[VIO_N_DIMENSIONS];
    nc_type                 nc_data_type;
    VIO_BOOL                signed_flag;
    VIO_BOOL                is_rgba_data;

    VIO_Real                voxel_min;
    VIO_Real                voxel_max;
    VIO_BOOL                real_range_set;
    VIO_Real                real_value_scale;
    VIO_Real                real_value_translation;

    VIO_Real                separations[VIO_MAX_DIMENSIONS];
    VIO_Real                starts[VIO_MAX_DIMENSIONS];
    VIO_Real                direction_cosines[VIO_MAX_DIMENSIONS][VIO_N_DIMENSIONS];

    VIO_BOOL                voxel_to_world_transform_uptodate;
    VIO_General_transform   voxel_to_world_transform;

    VIO_STR                 coordinate_system_name;

    VIO_Real               *irregular_starts[VIO_MAX_DIMENSIONS];
    VIO_Real               *irregular_widths[VIO_MAX_DIMENSIONS];
    VIO_BOOL               is_labels;
} volume_struct;

typedef  volume_struct  *VIO_Volume;

/* ---- macro for stepping through entire volume */

#define  BEGIN_ALL_VOXELS( volume, v0, v1, v2, v3, v4 )                       \
         {                                                                    \
             int  _i_, _sizes_[VIO_MAX_DIMENSIONS];                           \
             int  _size0_, _size1_, _size2_, _size3_, _size4_;                \
                                                                              \
             get_volume_sizes( volume, _sizes_ );                             \
             for_less( _i_, get_volume_n_dimensions(volume), VIO_MAX_DIMENSIONS ) \
                 _sizes_[_i_] = 1;                                            \
             _size0_ = _sizes_[0];                                            \
             _size1_ = _sizes_[1];                                            \
             _size2_ = _sizes_[2];                                            \
             _size3_ = _sizes_[3];                                            \
             _size4_ = _sizes_[4];                                            \
                                                                              \
             for_less( v0, 0, _size0_ )                                       \
             for_less( v1, 0, _size1_ )                                       \
             for_less( v2, 0, _size2_ )                                       \
             for_less( v3, 0, _size3_ )                                       \
             for_less( v4, 0, _size4_ )                                       \
             {

#define  END_ALL_VOXELS                                                       \
             }                                                                \
         }

/* ------------------------- set voxel value ------------------------ */

/* --- public macros to set the [x][y]... voxel of 'volume' to 'value' */

#define  SET_VOXEL_1D( volume, x, value )       \
           if( (volume)->is_cached_volume ) \
               set_cached_volume_voxel( volume, x, 0, 0, 0, 0, (VIO_Real) value ); \
           else \
               SET_MULTIDIM_1D( (volume)->array, x, value )

#define  SET_VOXEL_2D( volume, x, y, value )       \
           if( (volume)->is_cached_volume ) \
               set_cached_volume_voxel( volume, x, y, 0, 0, 0, (VIO_Real) value ); \
           else \
               SET_MULTIDIM_2D( (volume)->array, x, y, value )

#define  SET_VOXEL_3D( volume, x, y, z, value )       \
           if( (volume)->is_cached_volume ) \
               set_cached_volume_voxel( volume, x, y, z, 0, 0, (VIO_Real) value ); \
           else \
               SET_MULTIDIM_3D( (volume)->array, x, y, z, value )

#define  SET_VOXEL_4D( volume, x, y, z, t, value )       \
           if( (volume)->is_cached_volume ) \
               set_cached_volume_voxel( volume, x, y, z, t, 0, (VIO_Real) value ); \
           else \
               SET_MULTIDIM_4D( (volume)->array, x, y, z, t, value )

#define  SET_VOXEL_5D( volume, x, y, z, t, v, value )       \
           if( (volume)->is_cached_volume ) \
               set_cached_volume_voxel( volume, x, y, z, t, v, (VIO_Real) value ); \
           else \
               SET_MULTIDIM_5D( (volume)->array, x, y, z, t, v, value )

/* --- same as previous, but don't have to know dimensions of volume */

#define  SET_VOXEL( volume, x, y, z, t, v, value )       \
           if( (volume)->is_cached_volume ) \
               set_cached_volume_voxel( volume, x, y, z, t, v, (VIO_Real) value ); \
           else \
               SET_MULTIDIM( (volume)->array, x, y, z, t, v, value )

/* --- public macros to place the [x][y]...'th voxel of 'volume' in 'value' */

#define  GET_VOXEL_1D_TYPED( value, vtype, volume, x )       \
           if( (volume)->is_cached_volume ) \
               (value) = vtype get_cached_volume_voxel( volume, x, 0, 0, 0, 0 ); \
           else \
               GET_MULTIDIM_1D( value, vtype, (volume)->array, x )

#define  GET_VOXEL_2D_TYPED( value, vtype, volume, x, y )       \
           if( (volume)->is_cached_volume ) \
               (value) = vtype get_cached_volume_voxel( volume, x, y, 0, 0, 0 ); \
           else \
               GET_MULTIDIM_2D( value, vtype, (volume)->array, x, y )

#define  GET_VOXEL_3D_TYPED( value, vtype, volume, x, y, z )       \
           if( (volume)->is_cached_volume ) \
               (value) = vtype get_cached_volume_voxel( volume, x, y, z, 0, 0 ); \
           else \
               GET_MULTIDIM_3D( value, vtype, (volume)->array, x, y, z )

#define  GET_VOXEL_4D_TYPED( value, vtype, volume, x, y, z, t )       \
           if( (volume)->is_cached_volume ) \
               (value) = vtype get_cached_volume_voxel( volume, x, y, z, t, 0 ); \
           else \
               GET_MULTIDIM_4D( value, vtype, (volume)->array, x, y, z, t )

#define  GET_VOXEL_5D_TYPED( value, vtype, volume, x, y, z, t, v )       \
           if( (volume)->is_cached_volume ) \
               (value) = vtype get_cached_volume_voxel( volume, x, y, z, t, v ); \
           else \
               GET_MULTIDIM_5D( value, vtype, (volume)->array, x, y, z, t, v )

/* --- same as previous, but no need to know volume dimensions */

#define  GET_VOXEL_TYPED( value, vtype, volume, x, y, z, t, v )       \
           if( (volume)->is_cached_volume ) \
               (value) = vtype get_cached_volume_voxel( volume, x, y, z, t, v ); \
           else \
               GET_MULTIDIM( value, vtype, (volume)->array, x, y, z, t, v )

/* --- public macros to place the [x][y]...'th voxel of 'volume' in 'value' */

#define  GET_VOXEL_1D( value, volume, x )       \
         GET_VOXEL_1D_TYPED( value, , volume, x )

#define  GET_VOXEL_2D( value, volume, x, y )       \
         GET_VOXEL_2D_TYPED( value, , volume, x, y )

#define  GET_VOXEL_3D( value, volume, x, y, z )       \
         GET_VOXEL_3D_TYPED( value, , volume, x, y, z )

#define  GET_VOXEL_4D( value, volume, x, y, z, t )       \
         GET_VOXEL_4D_TYPED( value, , volume, x, y, z, t )

#define  GET_VOXEL_5D( value, volume, x, y, z, t, v )       \
         GET_VOXEL_5D_TYPED( value, , volume, x, y, z, t, v )

/* --- same as previous, but no need to know volume dimensions */

#define  GET_VOXEL( value, volume, x, y, z, t, v )       \
         GET_VOXEL_TYPED( value, , volume, x, y, z, t, v )

/* ------------------------- get voxel ptr ------------------------ */

/* --- public macros to return a pointer to the [x][y]'th voxel of the
       'volume', and place it in 'ptr' */

#define  GET_VOXEL_PTR_1D( ptr, volume, x )       \
           if( (volume)->is_cached_volume ) \
/*              handle_internal_error( "Cannot get pointer to cached VIO_Volume.\n");\
           else */ \
              GET_MULTIDIM_PTR_1D( ptr, (volume)->array, x )

#define  GET_VOXEL_PTR_2D( ptr, volume, x, y )       \
           if( (volume)->is_cached_volume ) \
              handle_internal_error( "Cannot get pointer to cached VIO_Volume.\n");\
           else \
              GET_MULTIDIM_PTR_2D( ptr, (volume)->array, x, y )

#define  GET_VOXEL_PTR_3D( ptr, volume, x, y, z )       \
           if( (volume)->is_cached_volume ) \
              handle_internal_error( "Cannot get pointer to cached VIO_Volume.\n");\
           else \
              GET_MULTIDIM_PTR_3D( ptr, (volume)->array, x, y, z )

#define  GET_VOXEL_PTR_4D( ptr, volume, x, y, z, t )       \
           if( (volume)->is_cached_volume ) \
              handle_internal_error( "Cannot get pointer to cached VIO_Volume.\n");\
           else \
              GET_MULTIDIM_PTR_4D( ptr, (volume)->array, x, y, z, t )

#define  GET_VOXEL_PTR_5D( ptr, volume, x, y, z, t, v )       \
           if( (volume)->is_cached_volume ) \
              handle_internal_error( "Cannot get pointer to cached VIO_Volume.\n");\
           else \
              GET_MULTIDIM_PTR_5D( ptr, (volume)->array, x, y, z, t, v )

/* --- same as previous, but no need to know voxel dimensions */

#define  GET_VOXEL_PTR( ptr, volume, x, y, z, t, v )       \
           if( (volume)->is_cached_volume ) \
              handle_internal_error( "Cannot get pointer to cached VIO_Volume.\n");\
           else \
              GET_MULTIDIM_PTR( ptr, (volume)->array, x, y, z, t, v )

/* --- returns the conversion of the 'voxel' value to a real value */

#define  CONVERT_VOXEL_TO_VALUE( volume, voxel )    \
            convert_voxel_to_value( volume, voxel )

/* --- returns the conversion of the 'real' value to a voxel value */

#define  CONVERT_VALUE_TO_VOXEL( volume, value )    \
            convert_value_to_voxel( volume, value )

/* --- assigns 'value' the value of the [x][y]...'th voxel of 'volume' */

#define  GET_VALUE_1D_TYPED( value, vtype, volume, x )       \
         { \
             GET_VOXEL_1D_TYPED( value, vtype, volume, x ); \
             value = CONVERT_VOXEL_TO_VALUE( volume, value ); \
         }

#define  GET_VALUE_2D_TYPED( value, vtype, volume, x, y )       \
         { \
             GET_VOXEL_2D_TYPED( value, vtype, volume, x, y ); \
             value = CONVERT_VOXEL_TO_VALUE( volume, value ); \
         }

#define  GET_VALUE_3D_TYPED( value, vtype, volume, x, y, z )       \
         { \
             GET_VOXEL_3D_TYPED( value, vtype, volume, x, y, z ); \
             value = CONVERT_VOXEL_TO_VALUE( volume, value ); \
         }

#define  GET_VALUE_4D_TYPED( value, vtype, volume, x, y, z, t )       \
         { \
             GET_VOXEL_4D_TYPED( value, vtype, volume, x, y, z, t ); \
             value = CONVERT_VOXEL_TO_VALUE( volume, value ); \
         }

#define  GET_VALUE_5D_TYPED( value, vtype, volume, x, y, z, t, v )       \
         { \
             GET_VOXEL_5D_TYPED( value, vtype, volume, x, y, z, t, v ); \
             value = CONVERT_VOXEL_TO_VALUE( volume, value ); \
         }

/* --- same as previous, without knowing number of dimensions of volume */

#define  GET_VALUE_TYPED( value, vtype, volume, x, y, z, t, v )       \
         switch( (volume)->n_dimensions ) \
         { \
         case 1:  GET_VALUE_1D_TYPED( value, vtype, volume, x );              break; \
         case 2:  GET_VALUE_2D_TYPED( value, vtype, volume, x, y );           break; \
         case 3:  GET_VALUE_3D_TYPED( value, vtype, volume, x, y, z );        break; \
         case 4:  GET_VALUE_4D_TYPED( value, vtype, volume, x, y, z, t );     break; \
         case 5:  GET_VALUE_5D_TYPED( value, vtype, volume, x, y, z, t, v );  break; \
         }

/* --- assigns 'value' the value of the [x][y]...'th voxel of 'volume' */

#define  GET_VALUE_1D( value, volume, x )       \
         GET_VALUE_1D_TYPED( value, , volume, x )

#define  GET_VALUE_2D( value, volume, x, y )       \
         GET_VALUE_2D_TYPED( value, , volume, x, y )

#define  GET_VALUE_3D( value, volume, x, y, z )       \
         GET_VALUE_3D_TYPED( value, , volume, x, y, z )

#define  GET_VALUE_4D( value, volume, x, y, z, t )       \
         GET_VALUE_4D_TYPED( value, , volume, x, y, z, t )

#define  GET_VALUE_5D( value, volume, x, y, z, t, v )       \
         GET_VALUE_5D_TYPED( value, , volume, x, y, z, t, v )

/* --- same as previous, without knowing number of dimensions of volume */

#define  GET_VALUE( value, volume, x, y, z, t, v )       \
         GET_VALUE_TYPED( value, , volume, x, y, z, t, v )

/* -------------------- minc file struct -------------------- */

typedef  struct
{
    int         arent_any_yet;
} volume_creation_options;

typedef  struct
{
    VIO_BOOL    promote_invalid_to_zero_flag;
    VIO_BOOL    convert_vector_to_scalar_flag;
    VIO_BOOL    convert_vector_to_colour_flag;
    int         dimension_size_for_colour_data;
    int         max_dimension_size_for_colour_data;
    int         rgba_indices[4];
    double      user_real_range[2];
    /*mostly for debugging*/
    VIO_BOOL    prefer_minc2_api;
} minc_input_options;

typedef  struct
{
    VIO_BOOL           file_is_being_read;

    /* input and output */

    int                cdfid;
    int                img_var;
    int                n_file_dimensions;
    long               sizes_in_file[MAX_VAR_DIMS];
    long               indices[MAX_VAR_DIMS];
    VIO_STR            dim_names[MAX_VAR_DIMS];
    VIO_Volume         volume;
    int                to_volume_index[MAX_VAR_DIMS];
    int                to_file_index[VIO_MAX_DIMENSIONS];
    int                minc_icv;
    VIO_STR            filename;

    /* input only */

    VIO_BOOL           end_volume_flag;
    VIO_BOOL           converting_to_colour;
    int                rgba_indices[4];
    int                n_volumes_in_file;

    int                valid_file_axes[VIO_MAX_DIMENSIONS];

    int                n_slab_dims;

    int                spatial_axes[VIO_N_DIMENSIONS];
    VIO_General_transform  voxel_to_world_transform;
    minc_input_options original_input_options;

    /* output only */

    int                img_var_id;
    int                min_id;
    int                max_id;
    double             image_range[2];
    VIO_BOOL           end_def_done;
    VIO_BOOL           ignoring_because_cached;
    VIO_BOOL           variables_written;
    int                dim_ids[MAX_VAR_DIMS];
    VIO_BOOL           outputting_in_order;
    VIO_BOOL           entire_file_written;
    nc_type            nc_data_type;
    VIO_BOOL           signed_flag;
    double             valid_range[2];
    int                image_dims[MAX_VAR_DIMS];
    int                src_cdfid;
    int                src_img_var;
    
#ifdef HAVE_MINC2
    mihandle_t         minc2id;
#else
    void*              minc2id; /*just in case*/
#endif /*HAVE_MINC2*/
    VIO_BOOL           using_minc2_api;
} minc_file_struct;

typedef  minc_file_struct  *Minc_file;

#define   MNC_ENDING   "mnc"

/* --- recognized file formats */

typedef  enum  { MNC_FORMAT, FREE_FORMAT, MNC2_FORMAT, MGH_FORMAT, NII_FORMAT, NRRD_FORMAT }       Volume_file_formats;

typedef struct
{
    Volume_file_formats  file_format;

    Minc_file            minc_file;

    /* for non-minc format files only */

    FILE                 *volume_file;
    int                  slice_index;
    long                 sizes_in_file[VIO_MAX_DIMENSIONS];
    int                  axis_index_from_file[VIO_MAX_DIMENSIONS];
    VIO_Data_types       file_data_type;
    VIO_BOOL             one_file_per_slice;
    VIO_STR              directory;
    VIO_STR              *slice_filenames;
    int                  *slice_byte_offsets;
    unsigned char        *byte_slice_buffer;
    unsigned short       *short_slice_buffer;
    void                 *generic_slice_buffer;
    VIO_Real             min_value, max_value;
    void                 *header_info;
    /*Mostly for debugging right now*/
    VIO_BOOL             prefer_minc2_api; 
} volume_input_struct;

/* --------------------- filter types -------------------------------- */


typedef enum {
               NEAREST_NEIGHBOUR,
               LINEAR_INTERPOLATION,
               BOX_FILTER,
               TRIANGLE_FILTER,
               GAUSSIAN_FILTER } VIO_Filter_types;

#endif /* VOL_IO_VOLUME_H */
