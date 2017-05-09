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



#include <internal_volume_io.h>
#include <minc_config.h>

#ifdef HAVE_MINC1
#include  <minc_basic.h>

#define  INVALID_AXIS   -1

static  VIO_BOOL  match_dimension_names(
    int               n_volume_dims,
    VIO_STR            volume_dimension_names[],
    int               n_file_dims,
    VIO_STR            file_dimension_names[],
    int               to_volume_index[] );

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_minc_file_n_dimensions
@INPUT      : filename
@OUTPUT     : 
@RETURNS    : n_dims
@DESCRIPTION: Returns the number of dimensions in the minc file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Nov. 4, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int   get_minc_file_n_dimensions(
    VIO_STR   filename )
{
    int       cdfid, img_var, n_dims;
    int       dim_vars[MAX_VAR_DIMS];
    nc_type   file_datatype;
    VIO_STR    expanded;

    set_ncopts(NC_VERBOSE);

    expanded = expand_filename( filename );

    cdfid = miopen( expanded, NC_NOWRITE );

    if( cdfid == MI_ERROR )
    {
        print_error( "Error opening %s\n", expanded );

        delete_string( expanded );

        return( -1 );
    }

    delete_string( expanded );

    img_var = ncvarid( cdfid, MIimage );

    ncvarinq( cdfid, img_var, (char *) NULL, &file_datatype,
              &n_dims, dim_vars, (int *) NULL );

    (void) miclose( cdfid );

    return( n_dims );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_minc_input_from_minc_id
@INPUT      : minc_id
              volume
              options
@OUTPUT     : 
@RETURNS    : Minc_file
@DESCRIPTION: Initializes input of volumes from an already opened MINC file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Nov. 15, 1996   D. MacDonald - added handling of space type
@MODIFIED   : May  20, 1997   D. MacDonald - removed float arithmetic in
                                             transformations and made volumes
                                             store starts/steps/dircos
---------------------------------------------------------------------------- */

VIOAPI  Minc_file  initialize_minc_input_from_minc_id(
    int                  minc_id,
    VIO_Volume           volume,
    minc_input_options   *options )
{
    minc_file_struct    *file;
    int                 dim_vars[MAX_VAR_DIMS], n_vol_dims;
    int                 i, slab_size, prev_sizes[MAX_VAR_DIMS];
    nc_type             prev_nc_type;
    VIO_BOOL             different;
    VIO_BOOL             range_specified;
    double              valid_range[2];
    long                long_size;
    VIO_BOOL             converted_sign, space_type_consensus;
    nc_type             converted_type;
    char                signed_flag[MI_MAX_ATTSTR_LEN+1], *ptr;
    char                dim_name[MI_MAX_ATTSTR_LEN+1];
    char                prev_space_type[MI_MAX_ATTSTR_LEN+1];
    char                space_type[MI_MAX_ATTSTR_LEN+1];
    nc_type             file_datatype;
    int                 sizes[MAX_VAR_DIMS];
    double              file_separations[MAX_VAR_DIMS];
    VIO_Real                volume_separations[MAX_VAR_DIMS];
    VIO_Real                volume_starts[MAX_VAR_DIMS];
    VIO_Real                voxel_zero;
    double              start_position[MAX_VAR_DIMS];
    double              dir_cosines[MAX_VAR_DIMS][MI_NUM_SPACE_DIMS];
    double              tmp_cosines[MI_NUM_SPACE_DIMS];
    VIO_BOOL             spatial_dim_flags[MAX_VAR_DIMS];
    double              real_min, real_max;
    int                 d, dimvar, which_valid_axis, axis;
    int                 spatial_axis_indices[MAX_VAR_DIMS];
    minc_input_options  default_options;
    VIO_BOOL             no_volume_data_type;
    char                spacing_type[MI_MAX_ATTSTR_LEN+1];
    double              *irr_starts[MAX_VAR_DIMS];
    double              *irr_widths[MAX_VAR_DIMS];

    ALLOC( file, 1 );

    file->cdfid = minc_id;
    file->file_is_being_read = TRUE;
    file->volume = volume;
    file->using_minc2_api = FALSE;

    if( options == (minc_input_options *) NULL )
    {
        set_default_minc_input_options( &default_options );
        set_default_minc_input_options( &file->original_input_options );
        options = &default_options;
    }
    else
        file->original_input_options = *options;

    get_volume_sizes( volume, prev_sizes );
    prev_nc_type = volume->nc_data_type;

    /* --- find the image variable */

    file->img_var = ncvarid( file->cdfid, MIimage );

    ncvarinq( file->cdfid, file->img_var, (char *) NULL, &file_datatype,
              &file->n_file_dimensions, dim_vars, (int *) NULL );

    for_less( d, 0, file->n_file_dimensions )
    {
        (void) ncdiminq( file->cdfid, dim_vars[d], dim_name, &long_size );
        file->dim_names[d] = create_string( dim_name );
        file->sizes_in_file[d] = long_size;
    }

    file->converting_to_colour = FALSE;

    if( equal_strings( file->dim_names[file->n_file_dimensions-1],
                       MIvector_dimension ) )
    {
        if( options->convert_vector_to_colour_flag &&
            file->sizes_in_file[file->n_file_dimensions-1] >=
            (long) options->dimension_size_for_colour_data &&
            file->sizes_in_file[file->n_file_dimensions-1] <=
            (long) options->max_dimension_size_for_colour_data )
        {
            for_less( i, 0, 4 )
            {
                file->rgba_indices[i] = options->rgba_indices[i];

                if( (long) options->rgba_indices[i] >=
                    file->sizes_in_file[file->n_file_dimensions-1] )
                {
                    file->rgba_indices[i] = -1;
                    if( i != 3 )
                        print_error( "Warning: rgba indices out of range.\n" );
                }
            }

            set_volume_type( volume, NC_INT, FALSE, 0.0, 0.0 );
            set_rgb_volume_flag( volume, TRUE );
            file->converting_to_colour = TRUE;
            delete_string( file->dim_names[file->n_file_dimensions-1] );
            --file->n_file_dimensions;
        }
        else if( options->convert_vector_to_scalar_flag )
        {
            delete_string( file->dim_names[file->n_file_dimensions-1] );
            --file->n_file_dimensions;
        }
    }

    /* Set the number of dimensions iff the file has fewer dimensions
     * than the initially created volume.
     */
    if (get_volume_n_dimensions( volume ) > file->n_file_dimensions)
    {
        set_volume_n_dimensions( volume, file->n_file_dimensions );
    }

    n_vol_dims = get_volume_n_dimensions( volume );
    if( file->n_file_dimensions < n_vol_dims )
    {
        print_error( "Error: MINC file has only %d dims, volume requires %d.\n",
               file->n_file_dimensions, n_vol_dims );
        FREE( file );
        return( (Minc_file) 0 );
    }
    else if( file->n_file_dimensions > MAX_VAR_DIMS )
    {
        print_error( "Error: MINC file has %d dims, can only handle %d.\n",
               file->n_file_dimensions, MAX_VAR_DIMS );
        FREE( file );
        return( (Minc_file) NULL );
    }

    /* --- match the dimension names of the volume with those in the file */

    if( !match_dimension_names( get_volume_n_dimensions(volume),
                                volume->dimension_names,
                                file->n_file_dimensions, file->dim_names,
                                file->to_volume_index ) )
    {
        print_error( "Error:  dimension names did not match: \n" );
        
        print_error( "\n" );
        print_error( "Requested:\n" );
        for_less( d, 0, n_vol_dims )
            print_error( "%d: %s\n", d+1, volume->dimension_names[d] );

        print_error( "\n" );
        print_error( "In File:\n" );
        for_less( d, 0, file->n_file_dimensions )
            print_error( "%d: %s\n", d+1, file->dim_names[d] );

        FREE( file );
        return( (Minc_file) NULL );
    }

    for_less( d, 0, n_vol_dims )
        file->to_file_index[d] = INVALID_AXIS;

    for_less( d, 0, file->n_file_dimensions )
    {
        if( file->to_volume_index[d] != INVALID_AXIS )
            file->to_file_index[file->to_volume_index[d]] = d;
    }

    file->n_volumes_in_file = 1;

    /* --- find the spatial axes (x,y,z) */

    which_valid_axis = 0;

    for_less( d, 0, VIO_N_DIMENSIONS )
    {
        volume->spatial_axes[d] = INVALID_AXIS;
        file->spatial_axes[d] = INVALID_AXIS;
    }

    for_less( d, 0, file->n_file_dimensions )
    {
        if( convert_dim_name_to_spatial_axis( file->dim_names[d], &axis ) )
        {
            spatial_axis_indices[d] = axis;
            file->spatial_axes[axis] = d;
        }
        else
            spatial_axis_indices[d] = INVALID_AXIS;

        spatial_dim_flags[d] = (spatial_axis_indices[d] != INVALID_AXIS);

        if( file->to_volume_index[d] != INVALID_AXIS )
        {
            file->valid_file_axes[which_valid_axis] = d;

            if( spatial_dim_flags[d] )
            {
                volume->spatial_axes[spatial_axis_indices[d]] =
                                        file->to_volume_index[d];
            }

            ++which_valid_axis;
        }
    }

    /* --- get the spatial axis info, slice separation, start pos, etc. */

    prev_space_type[0] = (char) 0;
    space_type_consensus = TRUE;

    for_less( d, 0, file->n_file_dimensions )
    {
        file_separations[d] = 1.0;
        start_position[d] = 0.0;

        irr_starts[d] = NULL;
        irr_widths[d] = NULL;

        if( spatial_dim_flags[d] )
        {
            dir_cosines[d][0] = 0.0;
            dir_cosines[d][1] = 0.0;
            dir_cosines[d][2] = 0.0;
            dir_cosines[d][spatial_axis_indices[d]] = 1.0;
        }

        dimvar = ncvarid( file->cdfid, file->dim_names[d] );
        if( dimvar != MI_ERROR )
        {
            (void) miattget1( file->cdfid, dimvar, MIstep, NC_DOUBLE,
                              (void *) (&file_separations[d]) );

            if( spatial_dim_flags[d] )
            {
                if( miattget1( file->cdfid, dimvar, MIstart, NC_DOUBLE,
                               (void *) (&start_position[d]) ) == MI_ERROR )
                    start_position[d] = 0.0;

                if( miattget( file->cdfid, dimvar, MIdirection_cosines,
                                 NC_DOUBLE, MI_NUM_SPACE_DIMS,
                                 (void *) tmp_cosines, (int *) NULL )
                     != MI_ERROR )
                {
                    dir_cosines[d][0] = tmp_cosines[0];
                    dir_cosines[d][1] = tmp_cosines[1];
                    dir_cosines[d][2] = tmp_cosines[2];
                }

                ptr = miattgetstr( file->cdfid, dimvar, MIspacetype,
                                    MI_MAX_ATTSTR_LEN+1, space_type );

                if( ptr != NULL )
                {
                    if( string_length( prev_space_type ) > 0 &&
                        string_length( space_type ) > 0 &&
                        !equal_strings( prev_space_type, space_type ) )
                    {
                        space_type_consensus = FALSE;
                    }

                    if( string_length( space_type ) > 0 )
                        (void) strcpy( prev_space_type, space_type );
                }
            }
            else if (!strcmp(MItime, file->dim_names[d])) {
                /* For the moment this is implemented for time dimensions
                 * only.
                 */
                miattgetstr(file->cdfid, dimvar, MIspacing,
                            MI_MAX_ATTSTR_LEN+1, spacing_type);
                if (!strcmp(spacing_type, MI_IRREGULAR)) {
                    long start[1];
                    long count[1];

                    irr_starts[d] = malloc(sizeof(VIO_Real) * file->sizes_in_file[d]);
                    irr_widths[d] = malloc(sizeof(VIO_Real) * file->sizes_in_file[d]);

                    start[0] = 0;
                    count[0] = file->sizes_in_file[d];

                    mivarget(file->cdfid, dimvar, start, count,
                             NC_DOUBLE, MI_SIGNED, irr_starts[d]);

                    dimvar = ncvarid(file->cdfid, MItime_width);
                    if (dimvar < 0) {
                        for (i = 0; i < count[0]; i++) {
                            irr_widths[d][i] = file_separations[d];
                        }
                    }
                    else {
                        mivarget(file->cdfid, dimvar, start, count,
                                 NC_DOUBLE, MI_SIGNED, irr_widths[d]);
                    }
                }
                else {
                    if( miattget1(file->cdfid, dimvar, MIstart, NC_DOUBLE,
                                  (void *) (&start_position[d]) ) == MI_ERROR )
                        start_position[d] = 0.0;
                }
            }
        }

        if( file->to_volume_index[d] == INVALID_AXIS )
        {
            file->n_volumes_in_file *= (int) file->sizes_in_file[d];
        }
        else
        {
            sizes[file->to_volume_index[d]] = (int) file->sizes_in_file[d];
            volume_separations[file->to_volume_index[d]] =
                                          file_separations[d];

            if( file->to_volume_index[d] != INVALID_AXIS )
            {
                volume_starts[file->to_volume_index[d]] = start_position[d];
                set_volume_direction_unit_cosine( volume,
                                file->to_volume_index[d], dir_cosines[d] );
            }

        }
    }

    /* --- create the world transform stored in the volume */

    set_volume_separations( volume, volume_separations );
    set_volume_starts( volume, volume_starts );

    if( space_type_consensus )
        set_volume_space_type( volume, prev_space_type );

    /* --- create the file world transform */

    compute_world_transform( file->spatial_axes, file_separations,
                             dir_cosines, start_position,
                             &file->voxel_to_world_transform );

    /* --- decide on type conversion */

    if( file->converting_to_colour )
    {
        converted_type = NC_FLOAT;
        converted_sign = FALSE;
    }
    else
    {
        no_volume_data_type = (get_volume_data_type(volume) == VIO_NO_DATA_TYPE);
        if( no_volume_data_type )     /* --- use type of file */
        {
            if( miattgetstr( file->cdfid, file->img_var, MIsigntype,
                             MI_MAX_ATTSTR_LEN, signed_flag ) != NULL )
            {
                converted_sign = equal_strings( signed_flag, MI_SIGNED );
            }
            else
                converted_sign = file_datatype != NC_BYTE;
    
            converted_type = file_datatype;
            set_volume_type( volume, converted_type, converted_sign, 0.0, 0.0 );
        }
        else                                        /* --- use specified type */
        {
            converted_type = get_volume_nc_data_type( volume, &converted_sign );
        }
    }

    set_volume_sizes( volume, sizes );

    for_less( d, 0, file->n_file_dimensions ) 
    {
        if (file->to_volume_index[d] == INVALID_AXIS) {
            continue;
        }

        if (irr_starts[d] != NULL) {
            set_volume_irregular_starts(volume, file->to_volume_index[d],
                                        file->sizes_in_file[d],
                                        irr_starts[d]);
            FREE( irr_starts[d] );
        }
        
        if (irr_widths[d] != NULL) {
            set_volume_irregular_widths(volume, file->to_volume_index[d],
                                        file->sizes_in_file[d],
                                        irr_widths[d]);
            FREE( irr_widths[d] );
        }
    }
                                        
    /* --- create the image conversion variable */

    file->minc_icv = miicv_create();

    (void) miicv_setint( file->minc_icv, MI_ICV_TYPE, (int) converted_type );
    (void) miicv_setstr( file->minc_icv, MI_ICV_SIGN,
                         converted_sign ? MI_SIGNED : MI_UNSIGNED );
    (void) miicv_setint( file->minc_icv, MI_ICV_DO_NORM, TRUE );
    (void) miicv_setint( file->minc_icv, MI_ICV_DO_FILLVALUE, TRUE );

    get_volume_voxel_range( volume, &valid_range[0], &valid_range[1] );
    range_specified = (valid_range[0] < valid_range[1]);

    valid_range[0] = 0.0;
    valid_range[1] = 0.0;

    if( file->converting_to_colour )
    {
        valid_range[0] = 0.0;
        valid_range[1] = 2.0 * (double) (1ul << 31ul) - 1.0;
        set_volume_voxel_range( volume, valid_range[0], valid_range[1] );
    }
    else if( no_volume_data_type )
    {
        if (miget_valid_range( file->cdfid, file->img_var, 
                               valid_range) == MI_ERROR) {
           valid_range[0] = valid_range[1] = 0.0;
        }
    }

    if( !file->converting_to_colour &&
        (no_volume_data_type || !range_specified) )
    {
        set_volume_voxel_range( volume, valid_range[0], valid_range[1] );
    }

    if( !file->converting_to_colour )
    {
        get_volume_voxel_range( volume, &valid_range[0], &valid_range[1] );

        (void) miicv_setdbl( file->minc_icv, MI_ICV_VALID_MIN, valid_range[0]);
        (void) miicv_setdbl( file->minc_icv, MI_ICV_VALID_MAX, valid_range[1]);
    }
    else
    {
        (void) miicv_setdbl( file->minc_icv, MI_ICV_VALID_MIN, 0.0 );
        (void) miicv_setdbl( file->minc_icv, MI_ICV_VALID_MAX, 1.0 );
    }

    if (options->user_real_range[0] < options->user_real_range[1]) {
       (void) miicv_setint( file->minc_icv, MI_ICV_USER_NORM, TRUE );
       (void) miicv_setdbl( file->minc_icv, MI_ICV_IMAGE_MIN,
                            options->user_real_range[0] );
       (void) miicv_setdbl( file->minc_icv, MI_ICV_IMAGE_MAX,
                            options->user_real_range[1] );
    }

    if( options->convert_vector_to_scalar_flag && !file->converting_to_colour )
    {
        (void) miicv_setint( file->minc_icv, MI_ICV_DO_DIM_CONV, TRUE );
        (void) miicv_setint( file->minc_icv, MI_ICV_DO_SCALAR, TRUE );
        (void) miicv_setint( file->minc_icv, MI_ICV_XDIM_DIR, FALSE );
        (void) miicv_setint( file->minc_icv, MI_ICV_YDIM_DIR, FALSE );
        (void) miicv_setint( file->minc_icv, MI_ICV_ZDIM_DIR, FALSE );
        (void) miicv_setint( file->minc_icv, MI_ICV_KEEP_ASPECT, FALSE );
    }

    (void) miicv_attach( file->minc_icv, file->cdfid, file->img_var );

    /* --- compute the mapping to real values */

    (void) miicv_inqdbl( file->minc_icv, MI_ICV_NORM_MIN, &real_min );
    (void) miicv_inqdbl( file->minc_icv, MI_ICV_NORM_MAX, &real_max );

    if( !file->converting_to_colour )
        set_volume_real_range( volume, real_min, real_max );

    /* --- if promoting invalid values to zero, then we need to detach and
           reattach in order to change the fillvalue in the icv */

    if( options->promote_invalid_to_zero_flag )
    {
        (void) miicv_detach( file->minc_icv );

        if( !file->converting_to_colour )
        {
            if( real_min == real_max )
                voxel_zero = valid_range[0];
            else if( real_min > 0.0 )
                voxel_zero = valid_range[0];
            else if( real_max < 0.0 )
                voxel_zero = valid_range[1];
            else
            {
                voxel_zero = valid_range[0] +
                             (valid_range[1] - valid_range[0]) *
                             (0.0 - real_min) / (real_max - real_min);
            }

            (void) miicv_setdbl( file->minc_icv, MI_ICV_FILLVALUE, voxel_zero );
        }
        else
            (void) miicv_setdbl( file->minc_icv, MI_ICV_FILLVALUE, 0.0 );

        (void) miicv_attach( file->minc_icv, file->cdfid, file->img_var );
    }

    for_less( d, 0, file->n_file_dimensions )
        file->indices[d] = 0;

    file->end_volume_flag = FALSE;

    set_ncopts(NC_VERBOSE | NC_FATAL);

    /* --- decide how many full dimensions to read in at a time 
       to max out the read/write buffer and make it like the 
       chunking dimensions for compression */

    file->n_slab_dims = 0;
    slab_size = 1;
    int unit_size = get_type_size( get_volume_data_type(volume) );
    int full_dim = 1;

    for( d = file->n_file_dimensions-1; d >= 0; d-- ) {
      if( file->to_volume_index[d] != INVALID_AXIS ) {
        if( MI_MAX_VAR_BUFFER_SIZE > file->sizes_in_file[d] * slab_size * unit_size && full_dim ) {
          slab_size *= file->sizes_in_file[d];
          file->n_slab_dims++;  /* integral number of complete dimensions */
        } else {
          slab_size *= MIN( file->sizes_in_file[d],
                            (long)( MI_MAX_VAR_BUFFER_SIZE / ( slab_size * unit_size ) ) );
          full_dim = 0;
        }
      }
    }

    /* --- decide whether the volume data must be freed (if it changed size) */

    different = FALSE;
    for_less( d, 0, n_vol_dims )
    {
        if( sizes[d] != prev_sizes[d] )
            different = TRUE;
    }

    if( prev_nc_type != converted_type )
        different = TRUE;

    if( different && volume_is_alloced( volume ) && !volume_is_cached(volume) )
        free_volume_data( volume );

    return( file );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_minc_input
@INPUT      : filename
              volume
@OUTPUT     : 
@RETURNS    : Minc_file
@DESCRIPTION: Initializes the input of a MINC file, passing back a MINC
              file pointer.  It assumes that the volume has been created,
              with the desired type, or MI_ORIGINAL_TYPE type if it is desired
              to use whatever type is in the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : June, 1993           David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  Minc_file  initialize_minc_input(
    VIO_STR               filename,
    VIO_Volume            volume,
    minc_input_options   *options )
{
    Minc_file    file;
    int          minc_id;
    VIO_STR       expanded;

    set_ncopts(0);

    expanded = expand_filename( filename );

    minc_id = miopen( expanded, NC_NOWRITE );

    if( minc_id == MI_ERROR )
    {
        print_error( "Error: opening MINC file \"%s\".\n", expanded );
        return( (Minc_file) 0 );
    }

    file = initialize_minc_input_from_minc_id( minc_id, volume, options );

    if( file == (Minc_file) NULL )
        (void) miclose( minc_id );
    else
        file->filename = create_string( expanded );

    delete_string( expanded );

    return( file );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_n_input_volumes
@INPUT      : file
@OUTPUT     : 
@RETURNS    : number of input volumes
@DESCRIPTION: After initializing the file input with a specified volume,
              the user calls this function to decide how many volumes are
              stored in the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : June, 1993           David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_n_input_volumes(
    Minc_file  file )
{
    return( file->n_volumes_in_file );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : close_minc_input
@INPUT      : file
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Closes the minc input file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : June, 1993           David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  close_minc_input(
    Minc_file   file )
{
    int  d;

    if( file == (Minc_file) NULL )
    {
        print_error( "close_minc_input(): NULL file.\n" );
        return( VIO_ERROR );
    }

    (void) miclose( file->cdfid );
    (void) miicv_free( file->minc_icv );

    for_less( d, 0, file->n_file_dimensions )
        delete_string( file->dim_names[d] );

    delete_string( file->filename );

    delete_general_transform( &file->voxel_to_world_transform );
    FREE( file );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_minc_hyperslab
@INPUT      : file
              data_type
              n_array_dims
              array_sizes
              array_data_ptr
              to_array
              start
              count
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs a hyperslab from the file into the array pointer.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  input_minc_hyperslab(
    Minc_file        file,
    VIO_Data_types   data_type,
    int              n_array_dims,
    int              array_sizes[],
    void             *array_data_ptr,
    int              to_array[],
    int              start[],
    int              count[] )
{
    VIO_Status           status;
    int              ind, expected_ind, file_ind, d, i, dim;
    int              size0, size1, size2, size3, size4;
    int              n_tmp_dims, n_file_dims;
    void             *void_ptr;
    VIO_BOOL          direct_to_array, non_full_size_found;
    int              tmp_ind, tmp_sizes[MAX_VAR_DIMS];
    int              vol1_indices[VIO_MAX_DIMENSIONS];
    int              v[VIO_MAX_DIMENSIONS], voxel[VIO_MAX_DIMENSIONS];
    long             used_start[MAX_VAR_DIMS], used_count[MAX_VAR_DIMS];
    VIO_Real             rgb[4];
    VIO_Colour           colour;
    VIO_multidim_array   buffer_array, rgb_array;

    n_file_dims = file->n_file_dimensions;
    direct_to_array = TRUE;
    expected_ind = n_array_dims-1;
    tmp_ind = n_file_dims-1;
    non_full_size_found = FALSE;

    for_less( ind, 0, n_array_dims )
        vol1_indices[ind] = -1;

    /*--- check if the hyperslab is a continuous chunk of memory in the array */

    for( file_ind = n_file_dims-1;  file_ind >= 0;  --file_ind )
    {
        used_start[file_ind] = (long) start[file_ind];
        used_count[file_ind] = (long) count[file_ind];

        ind = to_array[file_ind];

        if( ind != INVALID_AXIS )
        {
            if( !non_full_size_found &&
                (long) count[file_ind] < file->sizes_in_file[file_ind] )
                non_full_size_found = TRUE;
            else if( non_full_size_found && count[file_ind] > 1 )
                direct_to_array = FALSE;

            if( count[file_ind] > 1 && ind != expected_ind )
                direct_to_array = FALSE;

            if( count[file_ind] != 1 || file->sizes_in_file[file_ind] == 1 )
            {
                tmp_sizes[tmp_ind] = count[file_ind];
                vol1_indices[tmp_ind] = ind;
                --tmp_ind;
            }

            --expected_ind;
        }
    }

    if( !direct_to_array || file->converting_to_colour )
    {
        /*--- make a temporary buffer array, so that there is a continuous
              chunk */

        n_tmp_dims = n_file_dims - tmp_ind - 1;
        for_less( dim, 0, n_tmp_dims )
        {
            tmp_sizes[dim] = tmp_sizes[dim+tmp_ind+1];
            vol1_indices[dim] = vol1_indices[dim+tmp_ind+1];
        }

        create_multidim_array( &buffer_array, n_tmp_dims, tmp_sizes, data_type);

        if( file->converting_to_colour )
        {
            used_start[n_file_dims] = 0;
            used_count[n_file_dims] = file->sizes_in_file[n_file_dims];
            tmp_sizes[n_tmp_dims] = (int) used_count[n_file_dims];

            create_multidim_array( &rgb_array, n_tmp_dims+1, tmp_sizes, VIO_FLOAT );

            GET_MULTIDIM_PTR( void_ptr, rgb_array, 0, 0, 0, 0, 0 );
        }
        else
        {
            GET_MULTIDIM_PTR( void_ptr, buffer_array, 0, 0, 0, 0, 0 );
        }
    }
    else
    {
        void_ptr = array_data_ptr;
    }

    if( miicv_get( file->minc_icv, used_start, used_count, void_ptr ) ==
                                                                 MI_ERROR )
    {
        status = VIO_ERROR;
        if( file->converting_to_colour )
            delete_multidim_array( &rgb_array );
        if( !direct_to_array || file->converting_to_colour )
            delete_multidim_array( &buffer_array );
    }
    else
        status = VIO_OK;

    if( status == VIO_OK && (!direct_to_array || file->converting_to_colour) )
    {
        if( file->converting_to_colour )
        {
            for_less( dim, n_tmp_dims, VIO_MAX_DIMENSIONS )
                tmp_sizes[dim] = 1;           

            size0 = tmp_sizes[0];
            size1 = tmp_sizes[1];
            size2 = tmp_sizes[2];
            size3 = tmp_sizes[3];
            size4 = tmp_sizes[4];

            for_less( v[4], 0, size4 )
            for_less( v[3], 0, size3 )
            for_less( v[2], 0, size2 )
            for_less( v[1], 0, size1 )
            for_less( v[0], 0, size0 )
            {
                for_less( d, 0, n_tmp_dims )
                    voxel[d] = v[d];

                for_less( i, 0, 4 )
                {
                    if( file->rgba_indices[i] < 0 )
                    {
                        if( i < 3 )
                            rgb[i] = 0.0;
                        else
                            rgb[i] = 1.0;
                    }
                    else
                    {
                         voxel[n_tmp_dims] = file->rgba_indices[i];
                         GET_MULTIDIM( rgb[i], (VIO_Real), rgb_array,
                                       voxel[0], voxel[1],
                                       voxel[2], voxel[3], voxel[4] );
                    }
                }

                colour = make_rgba_Colour_0_1( rgb[0], rgb[1], rgb[2], rgb[3] );
                SET_MULTIDIM( buffer_array,
                              voxel[0], voxel[1],
                              voxel[2], voxel[3], voxel[4], colour );
            }

            delete_multidim_array( &rgb_array );
        }

        GET_MULTIDIM_PTR( void_ptr, buffer_array, 0, 0, 0, 0, 0 );
        copy_multidim_data_reordered( get_type_size(data_type),
                                      array_data_ptr, n_array_dims, array_sizes,
                                      void_ptr, n_tmp_dims, tmp_sizes,
                                      tmp_sizes, vol1_indices, FALSE );

        delete_multidim_array( &buffer_array );
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_slab
@INPUT      : file
              volume
              start
              count
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Inputs a multidimensional slab from the file and copies it
              into the appropriate part of the volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : June, 1993           David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static int input_slab(
    Minc_file   file,
    VIO_Volume  volume,
    int         to_volume[],
    long        start[],
    long        count[] )
{
    int      file_ind, ind;
    int      volume_start[MAX_VAR_DIMS];
    int      file_start[VIO_MAX_DIMENSIONS];
    int      file_count[VIO_MAX_DIMENSIONS];
    int      array_sizes[VIO_MAX_DIMENSIONS];
    void     *array_data_ptr;

    for_less( ind, 0, VIO_MAX_DIMENSIONS )
    {
        volume_start[ind] = 0;
    }

    for_less( file_ind, 0, file->n_file_dimensions )
    {
        file_start[file_ind] = (int) start[file_ind];
        file_count[file_ind] = (int) count[file_ind];

        ind = to_volume[file_ind];
        if( ind != INVALID_AXIS )
            volume_start[ind] = file_start[file_ind];
    }

    get_multidim_sizes( &volume->array, array_sizes );
    GET_MULTIDIM_PTR( array_data_ptr, volume->array,
                      volume_start[0], volume_start[1], volume_start[2],
                      volume_start[3], volume_start[4] );

    return input_minc_hyperslab( file,
                                 get_multidim_data_type(&volume->array),
                                 get_multidim_n_dimensions(&volume->array),
                                 array_sizes, array_data_ptr, to_volume,
                                 file_start, file_count );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_more_minc_file
@INPUT      : file
@OUTPUT     : fraction_done        - amount of file read
@RETURNS    : TRUE if volume has more left to read
@DESCRIPTION: Reads another chunk from the input file, passes back the
              total fraction read so far, and returns FALSE when the whole
              volume has been read.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : June, 1993           David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  input_more_minc_file(
    Minc_file   file,
    VIO_Real        *fraction_done )
{
    int      d, ind, n_done, total, n_slab;
    long     count[MAX_VAR_DIMS];
    VIO_Volume volume;
    VIO_BOOL  increment;

    if( file->end_volume_flag )
    {
        print_error( "End of file in input_more_minc_file()\n" );
        return( FALSE );
    }

    volume = file->volume;

    if( !volume_is_alloced( volume ) )
    {
        alloc_volume_data( volume );
        if( volume->is_cached_volume )
        {
            open_cache_volume_input_file( &volume->cache, volume,
                                          file->filename,
                                          &file->original_input_options );
        }
        if( !volume_is_alloced( volume ) ) return( FALSE );
    }

    if( volume->is_cached_volume )
    {
        *fraction_done = 1.0;
        file->end_volume_flag = TRUE;
    }
    else
    {
        /* --- set the counts for reading, actually these will be the same
               every time */

        for_less( ind, 0, file->n_file_dimensions )
            count[ind] = 1;

        n_slab = 0;

        for( d = file->n_file_dimensions-1;
             d >= 0 && n_slab < file->n_slab_dims;
             --d )
        {
            if( file->to_volume_index[d] != INVALID_AXIS )
            {
                count[d] = file->sizes_in_file[d];
                ++n_slab;
            }
        }

        if (input_slab( file, volume, file->to_volume_index, file->indices,
                        count ) != VIO_OK)
        {
            return FALSE;
        }

        /* --- advance to next slab */

        increment = TRUE;
        n_slab = 0;
        total = 1;
        n_done = 0;

        for( d = file->n_file_dimensions-1;  d >= 0;  --d )
        {
            if( n_slab >= file->n_slab_dims &&
                file->to_volume_index[d] != INVALID_AXIS )
            {
                if( increment )
                {
                    ++file->indices[d];
                    if( file->indices[d] < file->sizes_in_file[d] )
                        increment = FALSE;
                    else
                        file->indices[d] = 0;
                }
                n_done += total * (int) file->indices[d];
                total *= (int) file->sizes_in_file[d];
            }

            if( file->to_volume_index[d] != INVALID_AXIS )
                ++n_slab;
        }

        if( increment )
        {
            *fraction_done = 1.0;
            file->end_volume_flag = TRUE;
        }
        else
        {
            *fraction_done = (VIO_Real) n_done / (VIO_Real) total;
        }
    }

    return( !file->end_volume_flag );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : advance_input_volume
@INPUT      : file
@OUTPUT     : 
@RETURNS    : TRUE if more volumes to read
@DESCRIPTION: Advances the file indices to prepare for reading the next
              volume from the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  advance_input_volume(
    Minc_file   file )
{
    int                 ind, c, axis;
    VIO_Real                voxel[VIO_MAX_DIMENSIONS], world_space[VIO_N_DIMENSIONS];
    VIO_Real                vol_world_space[VIO_N_DIMENSIONS];
    VIO_Transform           offset;
    VIO_General_transform   offset_transform, new_transform;

    ind = file->n_file_dimensions-1;

    while( ind >= 0 )
    {
        if( file->to_volume_index[ind] == INVALID_AXIS )
        {
            ++file->indices[ind];
            if( file->indices[ind] < file->sizes_in_file[ind] )
                break;

            file->indices[ind] = 0;
        }
        --ind;
    }

    if( ind >= 0 )
    {
        file->end_volume_flag = FALSE;

        for_less( ind, 0, get_volume_n_dimensions( file->volume ) )
            file->indices[file->valid_file_axes[ind]] = 0;

        /*--- update the volume's voxel-to-world transform */

        for_less( c, 0, VIO_N_DIMENSIONS )
        {
            axis = file->spatial_axes[c];
            if( axis != INVALID_AXIS )
                voxel[c] = (VIO_Real) file->indices[axis];
            else
                voxel[c] = 0.0;
        }

        general_transform_point( &file->voxel_to_world_transform,
                                 voxel[0], voxel[1], voxel[2],
                                 &world_space[VIO_X], &world_space[VIO_Y],
                                 &world_space[VIO_Z]);

        for_less( c, 0, get_volume_n_dimensions(file->volume) )
            voxel[c] = 0.0;
        
        convert_voxel_to_world( file->volume, voxel,
                                &vol_world_space[VIO_X], &vol_world_space[VIO_Y],
                                &vol_world_space[VIO_Z]);

        make_identity_transform( &offset );
        for_less( c, 0, VIO_N_DIMENSIONS )
            Transform_elem(offset,c,3) = world_space[c] - vol_world_space[c];
        create_linear_transform( &offset_transform, &offset );
        concat_general_transforms( get_voxel_to_world_transform(file->volume),
                                   &offset_transform, &new_transform );
        set_voxel_to_world_transform( file->volume, &new_transform );
        delete_general_transform( &offset_transform );

        /*--- update the volume if it is cached */

        if( file->volume->is_cached_volume )
            set_cache_volume_file_offset( &file->volume->cache, file->volume,
                                          file->indices );
    }
    else
        file->end_volume_flag = TRUE;

    return( file->end_volume_flag );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : reset_input_volume
@INPUT      : file
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Rewinds the file indices to start inputting volumes from the
              file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  reset_input_volume(
    Minc_file   file )
{
    int   d;

    for_less( d, 0, file->n_file_dimensions )
        file->indices[d] = 0;
    file->end_volume_flag = FALSE;

    set_cache_volume_file_offset( &file->volume->cache, file->volume,
                                  file->indices );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : match_dimension_names
@INPUT      : n_volume_dims
              volume_dimension_names
              n_file_dims
              file_dimension_names
@OUTPUT     : to_volume_index
@RETURNS    : TRUE if match found
@DESCRIPTION: Attempts to match all the volume dimensions with the file
              dimensions.  This is done in 3 passes.  In the first pass,
              exact matches are found.  In the second pass, volume dimensions
              of "any_spatial_dimension" are matched.  On the final pass,
              volume dimension names which are empty strings are matched
              to any remaining file dimensions.  If a dimension matches
              on "any_spatial_dimension" or empty string, then the name from
              the file is copied to the volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Oct. 22, 1995   D. MacDonald    - copies the name from the file
                                                to the volume
---------------------------------------------------------------------------- */

static  VIO_BOOL  match_dimension_names(
    int               n_volume_dims,
    VIO_STR            volume_dimension_names[],
    int               n_file_dims,
    VIO_STR            file_dimension_names[],
    int               to_volume_index[] )
{
    int      i, j, iteration, n_matches, dummy;
    int      to_file_index[VIO_MAX_DIMENSIONS];
    VIO_BOOL  match;
    VIO_BOOL  volume_dim_found[VIO_MAX_DIMENSIONS];

    n_matches = 0;

    for_less( i, 0, n_file_dims )
        to_volume_index[i] = INVALID_AXIS;

    for_less( i, 0, n_volume_dims )
    {
        volume_dim_found[i] = FALSE;
        to_file_index[i] = -1;
    }

    for_less( iteration, 0, 3 )
    {
        for( i = n_volume_dims-1;  i >= 0;  --i )
        {
            if( !volume_dim_found[i] )
            {
                for( j = n_file_dims-1;  j >= 0;  --j )
                {
                    if( to_volume_index[j] == INVALID_AXIS )
                    {
                        switch( iteration )
                        {
                        case 0:
                            match = equal_strings( volume_dimension_names[i],
                                                   file_dimension_names[j] );
                            break;
                        case 1:
                            match = equal_strings( volume_dimension_names[i],
                                                   ANY_SPATIAL_DIMENSION ) &&
                                    convert_dim_name_to_spatial_axis(
                                          file_dimension_names[j], &dummy );
                            break;
                        case 2:
                            match = string_length(volume_dimension_names[i])
                                        == 0;
                            break;
                        }

                        if( match )
                        {
                            to_volume_index[j] = i;
                            to_file_index[i] = j;
                            volume_dim_found[i] = TRUE;
                            ++n_matches;
                            break;
                        }
                    }
                }
            }
        }
    }

    if( n_matches == n_volume_dims )
    {
        for_less( i, 0, n_volume_dims )
        {
            if( equal_strings( volume_dimension_names[i],
                               ANY_SPATIAL_DIMENSION ) ||
                string_length(volume_dimension_names[i]) == 0 )
            {
                replace_string( &volume_dimension_names[i],
                    create_string( file_dimension_names[to_file_index[i]] ) );
            }
        }
    }

    return( n_matches == n_volume_dims );
}


#endif /*HAVE_MINC1*/


/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_minc_file_id
@INPUT      : file
@OUTPUT     : 
@RETURNS    : minc file id
@DESCRIPTION: Returns the minc file id to allow user to perform MINC calls on
              this file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_minc_file_id(
    Minc_file  file )
{
    return( file->cdfid );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_default_minc_input_options
@INPUT      : 
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the default minc input options.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_default_minc_input_options(
    minc_input_options  *options )
{
    static  int     default_rgba_indices[4] = { 0, 1, 2, 3 };
    
    /*mostly for debugging*/
    options->prefer_minc2_api=miget_cfg_bool(MICFG_MINC_PREFER_V2_API);
    
    set_minc_input_promote_invalid_to_zero_flag( options, TRUE );
    set_minc_input_vector_to_scalar_flag( options, TRUE );
    set_minc_input_vector_to_colour_flag( options, FALSE );
    set_minc_input_colour_dimension_size( options, 3 );
    set_minc_input_colour_max_dimension_size( options, 4 );
    set_minc_input_colour_indices( options, default_rgba_indices );
    set_minc_input_user_real_range(options, 0.0, 0.0);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_promote_invalid_to_zero_flag
@INPUT      : flag
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the invalid promotion flag of the input options.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Oct. 25, 1996   D. MacDonald    - changed to promote to 0,
                                                used to be min_valid
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_promote_invalid_to_zero_flag(
    minc_input_options  *options,
    VIO_BOOL             flag )
{
    options->promote_invalid_to_zero_flag = flag;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_promote_invalid_to_zero_flag
@INPUT      : flag
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the invalid promotion flag of the input options.  Maintained
              for functional interface backward compatibility.  Programmers
              should now be calling set_minc_input_promote_invalid_to_zero_flag.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Oct. 25, 1996   D. MacDonald    - replaced with above function
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_promote_invalid_to_min_flag(
    minc_input_options  *options,
    VIO_BOOL             flag )
{
    set_minc_input_promote_invalid_to_zero_flag( options, flag );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_vector_to_scalar_flag
@INPUT      : flag
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the vector conversion flag of the input options.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_vector_to_scalar_flag(
    minc_input_options  *options,
    VIO_BOOL             flag )
{
    options->convert_vector_to_scalar_flag = flag;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_vector_to_colour_flag
@INPUT      : flag
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the colour conversion flag of the input options.  Any
              volume with a vector dimension of length 3 will be converted
              to a 32 bit colour.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_vector_to_colour_flag(
    minc_input_options  *options,
    VIO_BOOL             flag )
{
    options->convert_vector_to_colour_flag = flag;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_colour_dimension_size
@INPUT      : size
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the required number of vector components in a file that
              contains colour data.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_colour_dimension_size(
    minc_input_options  *options,
    int                 size )
{
    if( size > 0 )
        options->dimension_size_for_colour_data = size;
    else
    {
        print_error( "Warning: set_minc_input_colour_dimension_size:\n" );
        print_error( "         illegal size: %d\n", size );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_colour_max_dimension_size
@INPUT      : size
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the maximum number of vector components in a file that
              contains colour data.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_colour_max_dimension_size(
    minc_input_options  *options,
    int                 size )
{
    if( size > 0 )
        options->max_dimension_size_for_colour_data = size;
    else
    {
        print_error( "Warning: set_minc_input_colour_max_dimension_size:\n" );
        print_error( "         illegal size: %d\n", size );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_colour_indices
@INPUT      : indices
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the indices of the red, green, blue, and alpha in
              files that contain colours as the vector dimension.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_colour_indices(
    minc_input_options  *options,
    int                 indices[4] )
{
    int   i;

    for_less( i, 0, 4 )
        options->rgba_indices[i] = indices[i];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_input_user_real_range
@INPUT      : minimum, maximum - real range for scaling of input
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the user-defined real range for scaling input when
              the Volume type is integer. The max must be greater than
              the min for this option to take effect. Setting this
              will force the Volume to have a particular real range,
              rather than using the full range of the input file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 2001            Peter Neelin
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_input_user_real_range(
    minc_input_options  *options,
    double              minimum,
    double              maximum )
{
    options->user_real_range[0] = minimum;
    options->user_real_range[1] = maximum;
}

