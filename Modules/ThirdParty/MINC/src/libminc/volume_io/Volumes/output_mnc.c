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

#define  UNITS           "mm"

static VIO_Status get_dimension_ordering(
    int          n_vol_dims,
    VIO_STR      vol_dim_names[],
    int          n_file_dims,
    VIO_STR      file_dim_names[],
    int          to_volume[],
    int          to_file[] );

/* ----------------------------- MNI Header -----------------------------------
@NAME       : is_default_direction_cosine
@INPUT      : axis
              dir_cosines
@OUTPUT     : 
@RETURNS    : TRUE if is default
@DESCRIPTION: Checks to see if the cosine is the default for the axis,
              i.e., for x axis, is it ( 1, 0, 0 ).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_BOOL  is_default_direction_cosine(
    int        axis,
    double     dir_cosines[] )
{
    VIO_BOOL   is_default;
    int       i;

    is_default = TRUE;
    for_less( i, 0, VIO_N_DIMENSIONS )
    {
        if( (i == axis && dir_cosines[i] != 1.0) ||
            (i != axis && dir_cosines[i] != 0.0) )
        {
            is_default = FALSE;
            break;
        }
    }

    return( is_default );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_world_transform
@INPUT      : file
              space_type
              voxel_to_world_transform
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Outputs the voxel to world transformation, in terms of MINC
              starts, steps, and direction cosines.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 24, 1995    David MacDonald
@MODIFIED   : Nov. 15, 1996    D. MacDonald  - added handling of space type
@MODIFIED   : May. 20, 1997    D. MacDonald  - removed float arithmetic
@MODIFIED   : May  22, 1997   D. MacDonald - added use_volume_starts_and_steps
---------------------------------------------------------------------------- */

static  VIO_Status  output_world_transform(
    Minc_file              file,
    VIO_STR                 space_type,
    VIO_General_transform   *voxel_to_world_transform,
    VIO_BOOL                use_volume_starts_and_steps_flag )
{
    double              step[MAX_VAR_DIMS];
    VIO_Real            start[MAX_VAR_DIMS];
    double              dir_cosines[VIO_MAX_DIMENSIONS][VIO_N_DIMENSIONS];
    int                 dim, axis, spatial_axes[VIO_N_DIMENSIONS];

    /*--- set all starts/steps/dir_cosines to default */

    for_less( dim, 0, file->n_file_dimensions )
    {
        start[dim] = 0.0;
        step[dim] = 1.0;
        dir_cosines[dim][VIO_X] = 0.0;
        dir_cosines[dim][VIO_Y] = 0.0;
        dir_cosines[dim][VIO_Z] = 0.0;
    }

    /*--- if must use the volume's starts and steps */

    if( use_volume_starts_and_steps_flag )
    {
        for_less( dim, 0, file->n_file_dimensions )
        {
            if( convert_dim_name_to_spatial_axis( file->dim_names[dim], &axis ))
            {
                if( file->to_volume_index[dim] == INVALID_AXIS )
                    dir_cosines[dim][axis] = 1.0;    /*--- default */
                else
                {
                    start[dim] =
                          file->volume->starts[file->to_volume_index[dim]];
                    step[dim] =
                          file->volume->separations[file->to_volume_index[dim]];
                    dir_cosines[dim][VIO_X] = file->volume->direction_cosines
                                       [file->to_volume_index[dim]][VIO_X];
                    dir_cosines[dim][VIO_Y] = file->volume->direction_cosines
                                       [file->to_volume_index[dim]][VIO_Y];
                    dir_cosines[dim][VIO_Z] = file->volume->direction_cosines
                                       [file->to_volume_index[dim]][VIO_Z];
                }
            }
        }
    }
    else  /*--- convert the linear transform to starts/steps/dir cosines */
    {
        if( voxel_to_world_transform == NULL ||
            get_transform_type( voxel_to_world_transform ) != LINEAR )
        {
            print_error(
             "Cannot output null or non-linear transforms.  Using identity.\n");

            for_less( dim, 0, file->n_file_dimensions )
            {
                if( convert_dim_name_to_spatial_axis( file->dim_names[dim],
                                                      &axis ))
                    dir_cosines[dim][axis] = 1.0;
            }
        }
        else
        {
            spatial_axes[0] = INVALID_AXIS;
            spatial_axes[1] = INVALID_AXIS;
            spatial_axes[2] = INVALID_AXIS;

            for_less( dim, 0, file->n_file_dimensions )
            {
                if( convert_dim_name_to_spatial_axis( file->dim_names[dim],
                                                      &axis ))
                    spatial_axes[axis] = dim;
            }

            convert_transform_to_starts_and_steps( voxel_to_world_transform,
                                                   file->n_file_dimensions,
                                                   NULL, spatial_axes,
                                                   start, step,
                                                   dir_cosines );
        }
    }

    for_less( dim, 0, file->n_file_dimensions )
    {
        if( convert_dim_name_to_spatial_axis( file->dim_names[dim], &axis ) )
        {
            file->dim_ids[dim] = micreate_std_variable( file->cdfid,
                                      file->dim_names[dim], NC_DOUBLE, 0, NULL);

            if( file->dim_ids[dim] < 0 )
                return( VIO_ERROR );

            (void) miattputdbl( file->cdfid, file->dim_ids[dim], MIstep,
                                step[dim]);
            (void) miattputdbl( file->cdfid, file->dim_ids[dim], MIstart,
                                start[dim]);
            if( !is_default_direction_cosine( axis, dir_cosines[dim] ) )
            {
                (void) ncattput( file->cdfid, file->dim_ids[dim],
                                 MIdirection_cosines,
                                 NC_DOUBLE, VIO_N_DIMENSIONS, dir_cosines[dim]);
            }
            (void) miattputstr( file->cdfid, file->dim_ids[dim], MIunits,
                                UNITS );
            if( !equal_strings( space_type, MI_UNKNOWN_SPACE ) )
            {
                (void) miattputstr( file->cdfid, file->dim_ids[dim],
                                    MIspacetype, space_type );
            }
        }
        else
            file->dim_ids[dim] = -1;
    }

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_image_variable
@INPUT      : file
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Defines the image variable in the output minc file. This 
              should be done as the last thing before ending the header 
              definition.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : September 13, 2001    Peter Neelin
@MODIFIED   : 
---------------------------------------------------------------------------- */

static void create_image_variable(Minc_file file)
{
    int old_ncopts;

    old_ncopts =get_ncopts();

    /* Create the variable */
    file->img_var_id = micreate_std_variable( file->cdfid, MIimage,
                                              file->nc_data_type,
                                              file->n_file_dimensions, 
                                              file->image_dims );

    /* Copy all attributes if required */
    if( file->src_img_var != MI_ERROR )
    {
        set_ncopts(0);
        (void) micopy_all_atts( file->src_cdfid, file->src_img_var,
                                file->cdfid, file->img_var_id );
        (void) ncattdel( file->cdfid, file->img_var_id, MIvalid_max );
        (void) ncattdel( file->cdfid, file->img_var_id, MIvalid_min );
        (void) ncattdel( file->cdfid, file->img_var_id, MIvalid_range );

        set_ncopts(old_ncopts);
    }

    (void) miattputstr( file->cdfid, file->img_var_id, MIcomplete, MI_FALSE );

    if( file->signed_flag )
        (void) miattputstr( file->cdfid, file->img_var_id, MIsigntype,
                            MI_SIGNED );
    else
        (void) miattputstr( file->cdfid, file->img_var_id, MIsigntype,
                            MI_UNSIGNED );

    /* --- put the valid voxel range */

    if( file->valid_range[0] < file->valid_range[1] )
    {

        (void) miset_valid_range( file->cdfid, file->img_var_id, 
                                  file->valid_range);

    }

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : end_file_def
@INPUT      : file
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Ends the definition of the file, calling ncendef, but
              first creating the image variable as the last variable in 
              the file. This is done to allow images > 2 GB (on 64-bit 
              machines) and to ensure that data is written right to the 
              end of the file for backwards compatibility.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : September 13, 2001    Peter Neelin
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_Status end_file_def(Minc_file file)
{
   int ret;

   create_image_variable(file);

   ret = ncendef( file->cdfid );

   return ( ret == MI_ERROR ? VIO_ERROR : VIO_OK );

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_minc_output
@INPUT      : filename
              n_dimensions
              dim_names
              sizes
              file_nc_data_type
              file_signed_flag
              file_voxel_min
              file_voxel_max
              voxel_to_world_transform
              volume_to_attach
              options
@OUTPUT     : 
@RETURNS    : minc file
@DESCRIPTION: Creates a minc file for outputting volumes.  The n_dimensions,
              dim_names, sizes, file_nc_data_type, file_signed_flag,
              file_voxel_min, file_voxel_max, and voxel_to_world_transform
              define the type and shape of the file.  The volume_to_attach
              is the volume that will be output once or many times to 
              fill up the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Nov. 15, 1996   D. MacDonald  - added handling of space type
@MODIFIED   : Nov.  2, 1998   D. MacDonald  - fixed the bug with non-global
                                              limits on multiple volumes,
                                              found by peter
---------------------------------------------------------------------------- */

VIOAPI  Minc_file  initialize_minc_output(
    VIO_STR                filename,
    int                    n_dimensions,
    VIO_STR                dim_names[],
    int                    sizes[],
    nc_type                file_nc_data_type,
    VIO_BOOL               file_signed_flag,
    VIO_Real               file_voxel_min,
    VIO_Real               file_voxel_max,
    VIO_General_transform  *voxel_to_world_transform,
    VIO_Volume             volume_to_attach,
    minc_output_options    *options )
{
    minc_file_struct    *file;
    int                 volume_sizes[VIO_MAX_DIMENSIONS];
    int                 n_volume_dims;
    int                 d, vol_index, n_range_dims;
    static  VIO_STR      default_dim_names[] = { MIzspace, MIyspace, MIxspace };
    VIO_STR              *vol_dimension_names;
    minc_output_options default_options;

    if( options == (minc_output_options *) NULL )
    {
        set_default_minc_output_options( &default_options );
        options = &default_options;
        options->is_labels=volume_to_attach->is_labels;
    }

    if( dim_names == NULL )
    {
        if( n_dimensions != 3 )
        {
            print_error( "initialize_minc_output: " );
            print_error(
                "can't use NULL dim_names except with 3 dimensions.\n" );
            return( (Minc_file) NULL );
        }

        dim_names = default_dim_names;
    }

    if( file_nc_data_type == MI_ORIGINAL_TYPE )
    {
        file_nc_data_type = get_volume_nc_data_type( volume_to_attach,
                                                     &file_signed_flag );
        get_volume_voxel_range( volume_to_attach,
                                &file_voxel_min, &file_voxel_max );
    }
    else if( (file_nc_data_type == NC_FLOAT ||
              file_nc_data_type == NC_DOUBLE) &&
              file_voxel_min >= file_voxel_max )
    {
        get_volume_real_range( volume_to_attach,
                               &file_voxel_min, &file_voxel_max );
    } 
    else if( options->is_labels ) 
    {
        get_volume_real_range( volume_to_attach,
                               &file_voxel_min, &file_voxel_max );
        
        options->global_image_range[0]=file_voxel_min;
        options->global_image_range[1]=file_voxel_max;
        
    }

    /* --- check if dimension name correspondence between volume and file */

    n_volume_dims = get_volume_n_dimensions( volume_to_attach );

    if( n_volume_dims > n_dimensions )
    {
        print_error( "initialize_minc_output:" );
        print_error( " volume (%d) has more dimensions than file (%d).\n",
                     n_volume_dims, n_dimensions );
        return( (Minc_file) NULL );
    }

    ALLOC( file, 1 );

    file->file_is_being_read = FALSE;
    file->n_file_dimensions = n_dimensions;
    file->volume = volume_to_attach;
    file->outputting_in_order = TRUE;
    file->entire_file_written = FALSE;
    file->ignoring_because_cached = FALSE;
    file->src_img_var = MI_ERROR;
    file->using_minc2_api = FALSE;

    file->filename = expand_filename( filename );

    if( volume_to_attach->is_cached_volume &&
        volume_to_attach->cache.output_file_is_open &&
        equal_strings( volume_to_attach->cache.output_filename, file->filename))
    {
        file->ignoring_because_cached = TRUE;
#ifdef HAVE_MINC1
        flush_volume_cache( volume_to_attach );
#endif /*HAVE_MINC1*/
        return( file );
    }

    /*--- find correspondence between volume dimensions and file dimensions */

    vol_dimension_names = get_volume_dimension_names( volume_to_attach );

    if( get_dimension_ordering( n_volume_dims, vol_dimension_names,
                                n_dimensions, dim_names,
                            file->to_volume_index, file->to_file_index ) != VIO_OK )
    {
        FREE( file );
        return( (Minc_file) NULL );
    }

    delete_dimension_names( volume_to_attach, vol_dimension_names );

    /*--- check if image range specified */

    if( options->global_image_range[0] >= options->global_image_range[1] )
    {
        n_range_dims = n_dimensions - 2;
        if( equal_strings( dim_names[n_dimensions-1], MIvector_dimension ) )
            --n_range_dims;

        for_less( d, n_range_dims, n_dimensions )
        {
            if( file->to_volume_index[d] == INVALID_AXIS )
            {
                print_error( "initialize_minc_output: " );
                print_error( "if outputting volumes which don't contain all image\n");
                print_error( "dimensions, then must specify global image range.\n" );
                FREE( file );
                return( (Minc_file) NULL );
            }
        }
    }

    /*--- check sizes match between volume and file */

    get_volume_sizes( volume_to_attach, volume_sizes );

    for_less( d, 0, n_dimensions )
    {
        vol_index = file->to_volume_index[d];

        if( vol_index >= 0 && volume_sizes[vol_index] != sizes[d] )
        {
            print_error( "initialize_minc_output: " );
            print_error( "volume size[%d]=%d does not match file[%d]=%d.\n",
                   vol_index, volume_sizes[vol_index], d, sizes[d] );
            return( NULL );
        }
    }

    /*--- create the file */

    set_ncopts(NC_VERBOSE);

    file->cdfid =  micreate( file->filename, NC_CLOBBER );

    if( file->cdfid == MI_ERROR )
    {
        print_error( "Error: opening MINC file \"%s\".\n", file->filename );
        return( NULL );
    }

    /* Create the root variable */
    (void) micreate_std_variable(file->cdfid, MIrootvariable, 
                                 NC_INT, 0, NULL);

    for_less( d, 0, n_dimensions )
    {
        file->sizes_in_file[d] = (long) sizes[d];
        file->indices[d] = 0;
        file->dim_names[d] = create_string( dim_names[d] );
        file->image_dims[d] = ncdimdef( file->cdfid, file->dim_names[d],
                                        (long) sizes[d] );
    }

    if( output_world_transform( file, volume_to_attach->coordinate_system_name,
                                voxel_to_world_transform,
                                options->use_volume_starts_and_steps ) != VIO_OK )
    {
        FREE( file );
        return( NULL );
    }

    /*
     * Save information for creating image variable later
     */

    file->nc_data_type = file_nc_data_type;
    file->signed_flag = file_signed_flag;
    file->valid_range[0] = file_voxel_min;
    file->valid_range[1] = file_voxel_max;
            
    file->image_range[0] = options->global_image_range[0];
    file->image_range[1] = options->global_image_range[1];

    if( file->image_range[0] < file->image_range[1] || options->is_labels )
    {
        file->min_id = micreate_std_variable( file->cdfid, MIimagemin,
                                              NC_DOUBLE, 0, (int *) NULL );
        file->max_id = micreate_std_variable( file->cdfid, MIimagemax,
                                              NC_DOUBLE, 0, (int *) NULL );
    }
    else
    {
        n_range_dims = n_dimensions - 2;
        if( equal_strings( dim_names[n_dimensions-1], MIvector_dimension ) )
            --n_range_dims;

        file->min_id = micreate_std_variable( file->cdfid, MIimagemin,
                                              NC_DOUBLE, n_range_dims, 
                                              file->image_dims);
        file->max_id = micreate_std_variable( file->cdfid, MIimagemax,
                                              NC_DOUBLE, n_range_dims, 
                                              file->image_dims );
    }

    set_ncopts(NC_VERBOSE | NC_FATAL);

    file->end_def_done = FALSE;
    file->variables_written = FALSE;

    return( file );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : copy_auxiliary_data_from_minc_file
@INPUT      : file
              filename
              history_string
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Copies the auxiliary data from the filename to the opened
              Minc file, 'file'.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  copy_auxiliary_data_from_minc_file(
    Minc_file   file,
    VIO_STR      filename,
    VIO_STR      history_string )
{
    VIO_Status  status;
    int     src_cdfid;
    VIO_STR  expanded;

    if( file->ignoring_because_cached )
        return( VIO_OK );

    set_ncopts(NC_VERBOSE);

    expanded = expand_filename( filename );

    src_cdfid =  miopen( expanded, NC_NOWRITE );

    if( src_cdfid == MI_ERROR )
    {
        print_error( "Error opening %s\n", expanded );
        return( VIO_ERROR );
    }

    delete_string( expanded );

    status = copy_auxiliary_data_from_open_minc_file( file, src_cdfid,
                                                      history_string );

    (void) miclose( src_cdfid );

    set_ncopts(NC_VERBOSE | NC_FATAL);

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : copy_auxiliary_data_from_open_minc_file
@INPUT      : file
              src_cdfid
              history_string
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Copies the auxiliary data from the opened minc file specified
              by src_cdfid to the opened minc file specified by 'file'.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  copy_auxiliary_data_from_open_minc_file(
    Minc_file   file,
    int         src_cdfid,
    VIO_STR      history_string )
{
    int     src_img_var, varid, n_excluded, excluded_vars[10];
    int     i, src_min_id, src_max_id, src_root_id;
    VIO_Status  status;
    VIO_STR  excluded_list[] = {
                                  MIxspace,
                                  MIyspace,
                                  MIzspace,
                                  MItime,
                                  MItfrequency,
                                  MIxfrequency,
                                  MIyfrequency,
                                  MIzfrequency,
                                  MIvector_dimension
                               };

    if( file->ignoring_because_cached )
        return( VIO_OK );

    if( file->end_def_done )
    {
        print_error( "Cannot call copy_auxiliary_data_from_open_minc_file when not in define mode\n" );
        return( VIO_ERROR );
    }

    set_ncopts(0);

    n_excluded = 0;

    for_less( i, 0, VIO_SIZEOF_STATIC_ARRAY( excluded_list ) )
    {
        if( (varid = ncvarid(src_cdfid, excluded_list[i] )) != MI_ERROR )
            excluded_vars[n_excluded++] = varid;
    }

    if( (src_img_var = ncvarid(src_cdfid, MIimage )) != MI_ERROR )
        excluded_vars[n_excluded++] = src_img_var;
    if( (src_max_id = ncvarid(src_cdfid, MIimagemax )) != MI_ERROR )
        excluded_vars[n_excluded++] = src_max_id;
    if( (src_min_id = ncvarid(src_cdfid, MIimagemin )) != MI_ERROR )
        excluded_vars[n_excluded++] = src_min_id;
    if( (src_root_id = ncvarid(src_cdfid, MIrootvariable )) != MI_ERROR )
        excluded_vars[n_excluded++] = src_root_id;

    set_ncopts(NC_VERBOSE);

    (void) micopy_all_var_defs( src_cdfid, file->cdfid, n_excluded,
                                excluded_vars );

    if( src_min_id != MI_ERROR )
    {
        (void) micopy_all_atts( src_cdfid, src_min_id,
                                file->cdfid, file->min_id );
    }

    if( src_max_id != MI_ERROR )
    {
        (void) micopy_all_atts( src_cdfid, src_max_id,
                                file->cdfid, file->max_id );
    }

    if( src_root_id != MI_ERROR )
    {
        (void) micopy_all_atts( src_cdfid, src_root_id,
                                file->cdfid,
                                ncvarid( file->cdfid, MIrootvariable) );
    }

    status = VIO_OK;

    if( history_string != NULL )
        status = add_minc_history( file, history_string );

    if( status == VIO_OK )
    {

        /* Set info for copying image attributes. Unset afterwards, just
           to be safe. */
        file->src_cdfid = src_cdfid;
        file->src_img_var = src_img_var;

        status = end_file_def( file );

        file->src_img_var = MI_ERROR;

        if( status != VIO_OK )
        {
            print_error( "Error outputting volume: possibly disk full?\n" );
        }
    }

    if( status == VIO_OK )
    {
        file->end_def_done = TRUE;

        (void) micopy_all_var_values( src_cdfid, file->cdfid,
                                      n_excluded, excluded_vars );
    }

    set_ncopts(NC_VERBOSE | NC_FATAL);

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : add_minc_history
@INPUT      : file
              history_string
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Adds the history_string to the history in the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Nov. 2, 1998  D. MacDonald - fixed error P. Neelin found with
                                           concating to non-existent history
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  add_minc_history(
    Minc_file   file,
    VIO_STR      history_string )
{
    int      old_att_length;
    nc_type  datatype;
    VIO_STR   new_history;

    if( file->ignoring_because_cached )
        return( VIO_OK );

    if( file->end_def_done )
    {
        print_error( "Cannot call add_minc_history when not in define mode\n" );
        return( VIO_ERROR );
    }

    set_ncopts(0);

    if( ncattinq(file->cdfid, NC_GLOBAL, MIhistory, &datatype, &old_att_length)
                                                          == MI_ERROR ||
        datatype != NC_CHAR )
    {
        old_att_length = 0;
    }

    /* Allocate a string and get the old history */

    new_history = alloc_string( old_att_length );
    new_history[0] = (char) 0;

    (void) miattgetstr( file->cdfid, NC_GLOBAL, MIhistory, old_att_length+1,
                        new_history );

/*
    if( new_history[old_att_length] != (char) 0 )
    {
        print( "old_att_length %d  %d\n", old_att_length,
                new_history[old_att_length] );
        new_history[old_att_length] = (char) 0;
        print( "string: %d\n", new_history );
    }
*/

    /* Add the new command and put the new history. */

    concat_to_string( &new_history, history_string );

    set_ncopts(NC_VERBOSE | NC_FATAL);
    (void) miattputstr( file->cdfid, NC_GLOBAL, MIhistory, new_history );

    delete_string( new_history );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_dimension_ordering
@INPUT      : n_vol_dims
              vol_dim_names
              n_file_dims
              file_dim_names
@OUTPUT     : to_volume
              to_file
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Matches dimension names between the volume and file, setting
              the axis conversion from file to_volume and from volume to_file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  get_dimension_ordering(
    int          n_vol_dims,
    VIO_STR       vol_dim_names[],
    int          n_file_dims,
    VIO_STR       file_dim_names[],
    int          to_volume[],
    int          to_file[] )
{
    VIO_Status   status;
    int      v, f, n_found;

    for_less( f, 0, n_file_dims )
        to_volume[f] = -1;

    for_less( v, 0, n_vol_dims )
        to_file[v] = -1;

    n_found = 0;

    for_less( v, 0, n_vol_dims )
    {
        for_less( f, 0, n_file_dims )
        {
            if( to_volume[f] < 0 &&
                equal_strings( vol_dim_names[v], file_dim_names[f] ) )
            {
                to_volume[f] = v;
                to_file[v] = f;
                ++n_found;
            }
        }
    }

    if( n_found != n_vol_dims )
    {
        print_error( "Unsuccessful matching of volume and output dimension names.\n");
        status = VIO_ERROR;
    }
    else
        status = VIO_OK;

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : check_minc_output_variables
@INPUT      : file
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Checks if the file variables has been put into data mode,
              and does so if necessary.  Then it checks if the variables have
              been written, and does so if necessary.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  check_minc_output_variables(
    Minc_file   file )
{
    int               d, axis;
    long              start_index, mindex[MAX_VAR_DIMS];
    VIO_Real          voxel_min, voxel_max, real_min, real_max;
    double            dim_value;
    VIO_Volume        volume;
    VIO_Status        status;

    if( !file->end_def_done )
    {
        /* --- Get into data mode */

        set_ncopts(NC_VERBOSE);
        status = end_file_def( file );
        set_ncopts(NC_VERBOSE | NC_FATAL);
        file->end_def_done = TRUE;

        if( status != VIO_OK )
        {
            print_error( "Error outputting volume: possibly disk full?\n" );
            return( status );
        }
    }

    if( !file->variables_written )
    {
        volume = file->volume;

        file->variables_written = TRUE;

        set_ncopts(NC_VERBOSE);
        for_less( d, 0, file->n_file_dimensions )
            mindex[d] = 0;

        dim_value = 0.0;
        for_less( d, 0, file->n_file_dimensions )
        {
            if( convert_dim_name_to_spatial_axis( file->dim_names[d], &axis ) )
            {
                (void) mivarput1( file->cdfid, file->dim_ids[d], mindex,
                                  NC_DOUBLE, MI_SIGNED, &dim_value );
            }
        }

        file->minc_icv = miicv_create();

        (void) miicv_setint( file->minc_icv, MI_ICV_TYPE,
                             (int) volume->nc_data_type);
        (void) miicv_setstr( file->minc_icv, MI_ICV_SIGN,
                             volume->signed_flag ? MI_SIGNED : MI_UNSIGNED );
        if ( volume->is_labels )
        {
          (void) miicv_setint( file->minc_icv, MI_ICV_DO_NORM, FALSE );
          (void) miicv_setint( file->minc_icv, MI_ICV_USER_NORM, FALSE );
        } else {
          (void) miicv_setint( file->minc_icv, MI_ICV_DO_NORM, TRUE );
          (void) miicv_setint( file->minc_icv, MI_ICV_USER_NORM, TRUE );
        }
        
        if( file->image_range[0] < file->image_range[1] || volume->is_labels)
        {
            (void) miicv_setdbl( file->minc_icv, MI_ICV_IMAGE_MIN,
                                 file->image_range[0] );
            (void) miicv_setdbl( file->minc_icv, MI_ICV_IMAGE_MAX,
                                 file->image_range[1] );
        }
        else
        {
            get_volume_real_range( volume, &real_min, &real_max );
            (void) miicv_setdbl( file->minc_icv, MI_ICV_IMAGE_MIN, real_min );
            (void) miicv_setdbl( file->minc_icv, MI_ICV_IMAGE_MAX, real_max );
        }

        get_volume_voxel_range( volume, &voxel_min, &voxel_max );
        if( voxel_min < voxel_max )
        {
            (void) miicv_setdbl( file->minc_icv, MI_ICV_VALID_MIN, voxel_min );
            (void) miicv_setdbl( file->minc_icv, MI_ICV_VALID_MAX, voxel_max );
        }
        else
            print_error( "Volume has invalid min and max voxel value\n" );

        (void) miicv_attach( file->minc_icv, file->cdfid, file->img_var_id );

        start_index = 0;

        if( file->image_range[0] < file->image_range[1] || volume->is_labels )
        {
            (void) mivarput1( file->cdfid, file->min_id, &start_index,
                              NC_DOUBLE, MI_SIGNED, &file->image_range[0] );
            (void) mivarput1( file->cdfid, file->max_id, &start_index,
                              NC_DOUBLE, MI_SIGNED, &file->image_range[1] );
        }
        set_ncopts(NC_VERBOSE | NC_FATAL);
    }

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_output_random_order
@INPUT      : file
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the file into random order access, used by volume
              caching.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 26, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  set_minc_output_random_order(
    Minc_file   file )
{
    VIO_Status  status;

    status = check_minc_output_variables( file );

    file->outputting_in_order = FALSE;

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_minc_hyperslab
@INPUT      : file
              data_type
              n_array_dims
              array_sizes
              array_data_ptr
              to_array
              file_start
              file_count
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Outputs a hyperslab from an array to the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_minc_hyperslab(
    Minc_file           file,
    VIO_Data_types      data_type,
    int                 n_array_dims,
    int                 array_sizes[],
    void                *array_data_ptr,
    int                 to_array[],
    int                 file_start[],
    int                 file_count[] )
{
    int              ind, expected_ind, file_ind, dim;
    int              n_file_dims, n_tmp_dims;
    long             long_file_start[VIO_MAX_DIMENSIONS];
    long             long_file_count[VIO_MAX_DIMENSIONS];
    void             *void_ptr;
    VIO_BOOL          direct_from_array, non_full_size_found;
    int              tmp_ind, tmp_sizes[VIO_MAX_DIMENSIONS];
    int              array_indices[VIO_MAX_DIMENSIONS];
    int              array_counts[MAX_VAR_DIMS];
    VIO_Status       status;
    VIO_multidim_array buffer_array;

    status = check_minc_output_variables( file );

    if( status != VIO_OK )
        return( status );

    n_file_dims = file->n_file_dimensions;
    expected_ind = n_array_dims-1;
    tmp_ind = n_file_dims-1;
    non_full_size_found = FALSE;

    for_less( ind, 0, n_array_dims )
    {
        array_indices[ind] = -1;
        array_counts[ind] = 1;
    }

    direct_from_array = TRUE;

    /*--- determine if the hyperslab represents a consecutive chunk of
          memory in the array */

    for( file_ind = n_file_dims-1;  file_ind >= 0;  --file_ind )
    {
        long_file_start[file_ind] = (long) file_start[file_ind];
        long_file_count[file_ind] = (long) file_count[file_ind];
        ind = to_array[file_ind];
        if( ind != INVALID_AXIS )
        {
            array_counts[ind] = file_count[file_ind];

            if( !non_full_size_found &&
                (long) file_count[file_ind] < file->sizes_in_file[file_ind] )
                non_full_size_found = TRUE;
            else if( non_full_size_found && file_count[file_ind] > 1 )
                direct_from_array = FALSE;

            if( file_count[file_ind] > 1 && ind != expected_ind )
                direct_from_array = FALSE;

            if( file_count[file_ind] != 1 ||
                file->sizes_in_file[file_ind] == 1 )
            {
                tmp_sizes[tmp_ind] = file_count[file_ind];
                array_indices[ind] = tmp_ind;
                --tmp_ind;
            }

            --expected_ind;
        }
    }

    if( direct_from_array )     /* hyperslab is consecutive chunk of memory */
    {
        void_ptr = array_data_ptr;
    }
    else
    {
        /*--- create a temporary array to copy hyperslab to, so that
              we have a consecutive chunk of memory to write from */

        n_tmp_dims = n_file_dims - tmp_ind - 1;

        for_less( dim, 0, n_tmp_dims )
            tmp_sizes[dim] = tmp_sizes[dim+tmp_ind+1];

        for_less( dim, 0, n_array_dims )
            array_indices[dim] -= tmp_ind + 1;

        create_multidim_array( &buffer_array, n_tmp_dims, tmp_sizes, data_type);

        GET_MULTIDIM_PTR( void_ptr, buffer_array, 0, 0, 0, 0, 0 );

        /*--- copy from the array argument to the temporary array */

        copy_multidim_data_reordered( get_type_size(data_type),
                                      void_ptr, n_tmp_dims, tmp_sizes,
                                      array_data_ptr, n_array_dims, array_sizes,
                                      array_counts, array_indices, TRUE );

        GET_MULTIDIM_PTR( void_ptr, buffer_array, 0, 0, 0, 0, 0 );
    }

    /*--- output the data to the file */

    if( miicv_put( file->minc_icv, long_file_start, long_file_count,
                   void_ptr ) == MI_ERROR )
        status = VIO_ERROR;
    else
        status = VIO_OK;

    if( !direct_from_array )
        delete_multidim_array( &buffer_array );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_slab
@INPUT      : file
              volume
              to_volume
              file_start
              file_count
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Outputs the slab specified by the file start and count arrays,
              from the volume.  The to_volume array translates axes in the file
              to axes in the volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Sep  1, 1995    D. MacDonald - added cached volumes.
---------------------------------------------------------------------------- */

static  void  output_slab(
    Minc_file   file,
    VIO_Volume  volume,
    int         to_volume[],
    long        file_start[],
    long        file_count[] )
{
    int               dim, file_ind, ind, n_slab_dims;
    int               to_array[VIO_MAX_DIMENSIONS];
    int               volume_start[VIO_MAX_DIMENSIONS];
    int               volume_sizes[VIO_MAX_DIMENSIONS];
    int               array_to_volume[VIO_MAX_DIMENSIONS];
    int               int_file_count[VIO_MAX_DIMENSIONS];
    int               int_file_start[VIO_MAX_DIMENSIONS];
    int               slab_sizes[VIO_MAX_DIMENSIONS];
    int               v[VIO_MAX_DIMENSIONS];
    int               size0, size1, size2, size3, size4;
    VIO_Real              value;
    void              *array_data_ptr;
    VIO_multidim_array    array;

    for_less( file_ind, 0, file->n_file_dimensions )
    {
        int_file_start[file_ind] = (int) file_start[file_ind];
        int_file_count[file_ind] = (int) file_count[file_ind];

        ind = to_volume[file_ind];
        if( ind != INVALID_AXIS )
            volume_start[ind] = int_file_start[file_ind];
        else
            volume_start[ind] = 0;
    }

#ifdef HAVE_MINC1    
    if( volume->is_cached_volume )
    {
        /*--- must make a temporary hyperslab array to contain the volume */

        for_less( dim, 0, get_volume_n_dimensions(volume) )
            volume_sizes[dim] = 1;

        for_less( dim, 0, VIO_MAX_DIMENSIONS )
            array_to_volume[dim] = 0;

        for_less( dim, get_volume_n_dimensions(volume), VIO_MAX_DIMENSIONS )
        {
            volume_start[dim] = 0;
            volume_sizes[dim] = 1;
        }

        n_slab_dims = 0;
        for_less( file_ind, 0, file->n_file_dimensions )
        {
            ind = to_volume[file_ind];
            if( ind != INVALID_AXIS )
            {
                to_array[file_ind] = n_slab_dims;
                array_to_volume[n_slab_dims] = ind;
                slab_sizes[n_slab_dims] = int_file_count[file_ind];
                volume_sizes[ind] = int_file_count[file_ind];
                ++n_slab_dims;
            }
            else
            {
                to_array[file_ind] = INVALID_AXIS;
            }
        }

        create_multidim_array( &array, n_slab_dims, slab_sizes,
                               get_volume_data_type(volume) );

        /*--- copy from the cached volume to the temporary array */

        size0 = volume_sizes[0];
        size1 = volume_sizes[1];
        size2 = volume_sizes[2];
        size3 = volume_sizes[3];
        size4 = volume_sizes[4];

        for_less( v[0], 0, size0 )
        for_less( v[1], 0, size1 )
        for_less( v[2], 0, size2 )
        for_less( v[3], 0, size3 )
        for_less( v[4], 0, size4 )
        {
            value = get_volume_voxel_value( volume,
                                            volume_start[0] + v[0],
                                            volume_start[1] + v[1],
                                            volume_start[2] + v[2],
                                            volume_start[3] + v[3],
                                            volume_start[4] + v[4] );

            SET_MULTIDIM( array, v[array_to_volume[0]],
                                 v[array_to_volume[1]],
                                 v[array_to_volume[2]],
                                 v[array_to_volume[3]],
                                 v[array_to_volume[4]], value );
        }

        /*--- output the temporary array */

        GET_MULTIDIM_PTR( array_data_ptr, array, 0, 0, 0, 0, 0 );
        (void) output_minc_hyperslab( file, get_volume_data_type(volume),
                                      n_slab_dims, slab_sizes, array_data_ptr,
                                      to_array, int_file_start, int_file_count);
        delete_multidim_array( &array );
    }
    else
    {
#endif /*HAVE_MINC1*/
        GET_MULTIDIM_PTR( array_data_ptr, volume->array,
                          volume_start[0], volume_start[1], volume_start[2],
                          volume_start[3], volume_start[4] );
        get_volume_sizes( volume, volume_sizes );

#ifdef HAVE_MINC1        
        (void) output_minc_hyperslab( file, get_volume_data_type(volume),
                                      get_volume_n_dimensions(volume),
                                      volume_sizes, array_data_ptr,
                                      to_volume,
                                      int_file_start, int_file_count );
#else /*HAVE_MINC1*/        
        /*TODO: implement MINC2 api based reader*/
#endif /*HAVE_MINC1*/        
#ifdef HAVE_MINC1    
    }
#endif /*HAVE_MINC1*/
  
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_the_volume
@INPUT      : file
              volume
              volume_count
              file_start
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Outputs the volume to the file in the given position.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  output_the_volume(
    Minc_file   file,
    VIO_Volume  volume,
    int         volume_count[],
    long        file_start[] )
{
    VIO_Status            status;
    int               d, n_volume_dims, sizes[VIO_MAX_DIMENSIONS];
    int               slab_size, n_slab, this_count;
    int               vol_index, step, n_steps, n_range_dims;
    int               to_volume_index[MAX_VAR_DIMS];
    int               to_file_index[VIO_MAX_DIMENSIONS];
    long              file_indices[MAX_VAR_DIMS];
    long              count[MAX_VAR_DIMS];
    VIO_Real          real_min, real_max;
    VIO_STR            *vol_dimension_names;
    VIO_BOOL           increment;
    VIO_progress_struct   progress;

    status = check_minc_output_variables( file );

    if( status != VIO_OK )
        return( status );

    /* --- check if dimension name correspondence between volume and file */

    n_volume_dims = get_volume_n_dimensions( volume );

    if( n_volume_dims > file->n_file_dimensions )
    {
        print_error( "output_volume_to_minc_file_position:" );
        print_error( " volume (%d) has more dimensions than file (%d).\n",
                     n_volume_dims, file->n_file_dimensions );
        return( VIO_ERROR );
    }

    /*--- find correspondence between volume dimensions and file dimensions */

    vol_dimension_names = get_volume_dimension_names( volume );

    status = get_dimension_ordering( n_volume_dims, vol_dimension_names,
                                     file->n_file_dimensions, file->dim_names,
                                     to_volume_index, to_file_index );

    delete_dimension_names( volume, vol_dimension_names );

    if( status != VIO_OK )
        return( VIO_ERROR );

    /*--- check sizes match between volume and file */

    get_volume_sizes( volume, sizes );

    for_less( d, 0, file->n_file_dimensions )
    {
        vol_index = to_volume_index[d];

        if( vol_index >= 0 )
        {
            if( volume_count[vol_index] < 0 ||
                volume_count[vol_index] > sizes[vol_index] )
            {
                print_error( "output_the_volume: invalid volume count.\n" );
                print_error( "    count[%d] = %d\n",
                       vol_index, volume_count[vol_index] );
                return( VIO_ERROR );
            }

            this_count = volume_count[vol_index];
        }
        else
        {
            this_count = 1;
        }

        if( file_start[d] < 0 || file_start[d] + (long) this_count >
            file->sizes_in_file[d] )
        {
            print_error( "output_the_volume:  invalid minc file position.\n" );
            print_error( "    start[%d] = %ld     count[%d] = %d\n", d, file_start[d],
                      d, this_count );
            return( VIO_ERROR );
        }
    }

    /*--- if per slice image ranges, output the ranges corresponding to this
          volume */

    if( file->image_range[0] >= file->image_range[1] && !volume->is_labels )
    {
        long     n_ranges, range_start[MAX_VAR_DIMS], range_count[MAX_VAR_DIMS];
        long     r;
        double   *image_range;

        n_range_dims = file->n_file_dimensions - 2;
        if( equal_strings( file->dim_names[file->n_file_dimensions-1],
                           MIvector_dimension ) )
            --n_range_dims;

        n_ranges = 1;
        for_less( d, 0, n_range_dims )
        {
            vol_index = to_volume_index[d];
            if( vol_index == INVALID_AXIS )
            {
                range_count[d] = 1;
                range_start[d] = file_start[d];
            }
            else
            {
                n_ranges *= (long) volume_count[vol_index];
                range_count[d] = (long) volume_count[vol_index];
                range_start[d] = 0;
            }
        }

        get_volume_real_range( volume, &real_min, &real_max );

        ALLOC( image_range, n_ranges );

        for_less( r, 0, n_ranges )
            image_range[r] = real_min;

        (void) mivarput( file->cdfid, file->min_id,
                         range_start, range_count,
                         NC_DOUBLE, MI_UNSIGNED, (void *) image_range );

        for_less( r, 0, n_ranges )
            image_range[r] = real_max;

        (void) mivarput( file->cdfid, file->max_id,
                         range_start, range_count,
                         NC_DOUBLE, MI_UNSIGNED, (void *) image_range );

        FREE( image_range );
    }

    /* --- determine which contiguous blocks of volume to output
       to max out the read/write buffer and make it like the
       chunking dimensions for compression (for efficiency) */

    file->n_slab_dims = 0;
    slab_size = 1;
    n_steps = 1;
    int unit_size = get_type_size( get_volume_data_type(volume) );

    for( d = file->n_file_dimensions-1; d >= 0; d-- ) {
      count[d] = 1;
      file_indices[d] = file_start[d];
      if( to_volume_index[d] != INVALID_AXIS ) {
        if( MI_MAX_VAR_BUFFER_SIZE > volume_count[to_volume_index[d]] * slab_size * unit_size 
            && n_steps == 1 ) {
          count[d] = volume_count[to_volume_index[d]];
          file->n_slab_dims++;  /* integral number of complete dimensions */
        } else {
          count[d] = MIN( volume_count[to_volume_index[d]], 
                          (long)( MI_MAX_VAR_BUFFER_SIZE / ( slab_size * unit_size ) ) );
          n_steps *= (int)( ( volume_count[to_volume_index[d]] + count[d] - 1 ) / count[d] );
        }
        slab_size *= count[d];
      }
    }

    if( file->n_slab_dims == 0 ) {
      fprintf( stderr, "Error: You seem to be processing a minc file\n" );
      fprintf( stderr, "with no valid axes. Please check your volume.\n" );
      exit(1);
    }

    /*--- now write entire volume in contiguous chunks (possibly only 1 req'd)*/

    step = 0;

    initialize_progress_report( &progress, FALSE, n_steps,"Outputting Volume" );

    increment = FALSE;
    while( !increment ) {

        /*--- set the indices of the file slab to write */

        long local_count[MAX_VAR_DIMS];
        for( d = 0; d < file->n_file_dimensions; d++ ) {
          vol_index = to_volume_index[d];
          local_count[d] = MIN( volume_count[vol_index] - file_indices[d], count[d] );
        }

        output_slab( file, volume, to_volume_index, file_indices, local_count );

        /*--- increment the file index dimensions which correspond
              for the next slab to write to output */

        increment = TRUE;

        d = file->n_file_dimensions-1;
        n_slab = 0;
        while( increment && d >= 0 ) {
            vol_index = to_volume_index[d];

            if( vol_index != INVALID_AXIS && n_slab >= file->n_slab_dims ) {
                file_indices[d] += local_count[d];
                if( file_indices[d] < file_start[d] + (long) volume_count[vol_index] ) {
                    increment = FALSE;
                } else {
                    file_indices[d] = file_start[d];
                }
            }

            if( vol_index != INVALID_AXIS )
                ++n_slab;

            --d;
        }

        ++step;

        if( n_steps > 1 )
            update_progress_report( &progress, step );
    }

    terminate_progress_report( &progress );

    if( step != n_steps ) {
      fprintf( stderr, "Error: Your output minc file may be incomplete\n" );
      fprintf( stderr, "(wrote only %d out of %d buffers)\n", step, n_steps );
      exit(1);
    }

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_volume_to_minc_file_position
@INPUT      : file
              volume
              volume_count
              file_start
@OUTPUT     : Outputs the volume to the specified file position.
@RETURNS    : 
@DESCRIPTION: 
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_volume_to_minc_file_position(
    Minc_file   file,
    VIO_Volume  volume,
    int         volume_count[],
    long        file_start[] )
{
    if( file->ignoring_because_cached )
        return( VIO_OK );

    file->outputting_in_order = FALSE;

    return( output_the_volume( file, volume, volume_count, file_start ) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_minc_volume
@INPUT      : file
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Outputs the attached volume to the MINC file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_minc_volume(
    Minc_file   file )
{
    int        d, volume_count[VIO_MAX_DIMENSIONS];
    VIO_BOOL    increment;

    if( file->ignoring_because_cached )
        return( VIO_OK );

    /*--- check number of volumes written */

    d = 0;
    while( d < file->n_file_dimensions &&
           file->to_volume_index[d] != INVALID_AXIS )
        ++d;

    if( d < file->n_file_dimensions &&
        file->indices[d] >= file->sizes_in_file[d] )
    {
        print_error(
             "output_minc_volume: attempted to write too many subvolumes.\n");
        return( VIO_ERROR );
    }

    get_volume_sizes( file->volume, volume_count );

    if( output_the_volume( file, file->volume, volume_count,
                           file->indices ) != VIO_OK )
        return( VIO_ERROR );

    /*--- increment the file index dimensions which do not
          correspond to volume dimensions */

    increment = TRUE;

    d = file->n_file_dimensions-1;
    while( increment && d >= 0 )
    {
        if( file->to_volume_index[d] == INVALID_AXIS )
        {
            ++file->indices[d];

            if( file->indices[d] < file->sizes_in_file[d] )
                increment = FALSE;
            else
                file->indices[d] = 0;
        }

        --d;
    }

    if( increment )
        file->entire_file_written = TRUE;

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : close_minc_output
@INPUT      : file
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Closes the MINC file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  close_minc_output(
    Minc_file   file )
{
    int    d;

    if( file == (Minc_file) NULL )
    {
        print_error( "close_minc_output(): NULL file.\n" );
        return( VIO_ERROR );
    }

    if( !file->ignoring_because_cached )
    {
        if( file->outputting_in_order && !file->entire_file_written )
        {
            print_error( "Warning:  the MINC file has been " );
            print_error( "closed without writing part of it.\n");
        }

        (void) miattputstr( file->cdfid, file->img_var_id, MIcomplete, MI_TRUE);

        (void) miclose( file->cdfid );
        (void) miicv_free( file->minc_icv );

        for_less( d, 0, file->n_file_dimensions )
            delete_string( file->dim_names[d] );
    }

    delete_string( file->filename );

    FREE( file );

    return( VIO_OK );
}
#endif /*HAVE_MINC1*/



/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_default_minc_output_options
@INPUT      : 
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the minc output options to the default.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : May  22, 1997   D. MacDonald - added use_volume_starts_and_steps
---------------------------------------------------------------------------- */

VIOAPI  void  set_default_minc_output_options(
    minc_output_options  *options           )
{
    int   dim;

    for_less( dim, 0, VIO_MAX_DIMENSIONS )
        options->dimension_names[dim] = NULL;

    options->global_image_range[0] = 0.0;
    options->global_image_range[1] = -1.0;

    options->use_volume_starts_and_steps = FALSE;
    options->use_starts_set = FALSE;
    
    options->is_labels  = FALSE;
    
    /*mostly for debugging*/
    options->prefer_minc2_api=miget_cfg_bool(MICFG_MINC_PREFER_V2_API);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : copy_minc_output_options
@INPUT      : src
@OUTPUT     : dest
@RETURNS    : 
@DESCRIPTION: Copies the minc output options to a new structure.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Nov. 12, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  copy_minc_output_options(
    minc_output_options  *src,
    minc_output_options  *dest )
{
    int   dim;

    if( src == NULL )
        set_default_minc_output_options( dest );
    else
    {
        *dest = *src;

        for_less( dim, 0, VIO_MAX_DIMENSIONS )
        {
            if( src->dimension_names[dim] != NULL )
                dest->dimension_names[dim] = create_string(
                                                  src->dimension_names[dim] );
        }
        dest->is_labels=src->is_labels;
        dest->prefer_minc2_api=src->prefer_minc2_api;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : delete_minc_output_options
@INPUT      : options
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Deletes the minc output options.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  delete_minc_output_options(
    minc_output_options  *options           )
{
    int   i;

    for_less( i, 0, VIO_MAX_DIMENSIONS )
        delete_string( options->dimension_names[i] );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_output_dimensions_order
@INPUT      : n_dimensions
              dimension_names
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the dimension ordering of the minc output options.
              This option is used by output_volume, but not by
              initialize_minc_output.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_output_dimensions_order(
    minc_output_options  *options,
    int                  n_dimensions,
    VIO_STR               dimension_names[] )
{
    int   i;

    for_less( i, 0, n_dimensions )
    {
        replace_string( &options->dimension_names[i],
                        create_string(dimension_names[i]) );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_output_real_range
@INPUT      : real_min
              real_max
@OUTPUT     : options
@RETURNS    : 
@DESCRIPTION: Sets the global real range of the entire file, unless real_min
              >= real_max.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_output_real_range(
    minc_output_options  *options,
    VIO_Real                 real_min,
    VIO_Real                 real_max )
{
    options->global_image_range[0] = real_min;
    options->global_image_range[1] = real_max;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc_output_use_volume_starts_and_steps_flag
@INPUT      : options
              flag
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Tells MINC output to use the exact starts and steps stored in
              the volume, not the voxel-to-world-transform.  This avoids
              round-off errors in converting to transform on input, then
              from transform on output.
@METHOD     : 
@GLOBALS    : 
@CALLS      :  
@CREATED    : May. 22, 1997    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_minc_output_use_volume_starts_and_steps_flag(
    minc_output_options  *options,
    VIO_BOOL              flag )
{
    options->use_volume_starts_and_steps = flag;
    options->use_starts_set = TRUE;
}
