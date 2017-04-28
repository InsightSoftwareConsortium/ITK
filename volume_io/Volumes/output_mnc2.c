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

#ifdef HAVE_MINC2

#include  <minc2.h>

#define  INVALID_AXIS   -1

static  VIO_Status  get_dimension_ordering(
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
    Minc_file               file,
    VIO_STR                 space_type,
    VIO_General_transform   *voxel_to_world_transform,
    VIO_BOOL                use_volume_starts_and_steps_flag,
    midimhandle_t           *minc_dimensions
                                          )
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
            miset_dimension_separation(minc_dimensions[dim],step[dim]);
            miset_dimension_start(minc_dimensions[dim],start[dim]);
            miset_dimension_apparent_voxel_order(minc_dimensions[dim],MI_POSITIVE);
            
            if( !is_default_direction_cosine( axis, dir_cosines[dim] ) )
            {
              miset_dimension_cosines(minc_dimensions[dim],dir_cosines[dim]);
            }
        }
        else
            file->dim_ids[dim] = -1;
    }

    return( VIO_OK );
}


/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_minc2_output
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

VIOAPI  Minc_file  initialize_minc2_output(
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
    static  VIO_STR     default_dim_names[] = { MIzspace, MIyspace, MIxspace };
    VIO_STR             *vol_dimension_names;
    minc_output_options default_options;
    mitype_t            file_minc_datatype;
    midimhandle_t       minc_dimensions[VIO_MAX_DIMENSIONS];
    mivolumeprops_t     hprops;

    if( minew_volume_props(&hprops) < 0)
    {
      return NULL;
    }

    if( options == (minc_output_options *) NULL )
    {
        set_default_minc_output_options( &default_options );
        options = &default_options;
        options->is_labels=volume_to_attach->is_labels;
    }
    
    /*TODO: provide some other compression ? */
    miset_props_compression_type(hprops, MI_COMPRESS_ZLIB);
    miset_props_zlib_compression(hprops,4);

    if( dim_names == NULL )
    {
        if( n_dimensions != 3 )
        {
            print_error( "initialize_minc_output: " );
            print_error(
                "can't use NULL dim_names except with 3 dimensions.\n" );
            mifree_volume_props( hprops );
            return( (Minc_file) NULL );
        }

        dim_names = default_dim_names;
    }

    if( file_nc_data_type == MI_ORIGINAL_TYPE )
    {
        file_nc_data_type = get_volume_nc_data_type( volume_to_attach,
                                                     &file_signed_flag );
        get_volume_voxel_range( volume_to_attach,
                                &file_voxel_min, 
                                &file_voxel_max );
    }
    else if( (file_nc_data_type == NC_FLOAT ||
              file_nc_data_type == NC_DOUBLE) &&
              file_voxel_min >= file_voxel_max )
    {
        get_volume_real_range( volume_to_attach,
                               &file_voxel_min, 
                               &file_voxel_max );
    } 
    else if( options->is_labels ) 
    {
        get_volume_real_range( volume_to_attach,
                               &file_voxel_min, 
                               &file_voxel_max );
        
        options->global_image_range[0]=file_voxel_min;
        options->global_image_range[1]=file_voxel_max;
    }

    file_minc_datatype = nc_type_to_minc2_type(file_nc_data_type ,
                                               file_signed_flag);
    
    /* --- check if dimension name correspondence between volume and file */

    n_volume_dims = get_volume_n_dimensions( volume_to_attach );

    if( n_volume_dims > n_dimensions )
    {
        print_error( "initialize_minc_output:" );
        print_error( " volume (%d) has more dimensions than file (%d).\n",
                     n_volume_dims, n_dimensions );
        mifree_volume_props( hprops );
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
    file->using_minc2_api = TRUE;

    file->filename = expand_filename( filename );

    /*--- find correspondence between volume dimensions and file dimensions */

    vol_dimension_names = get_volume_dimension_names( volume_to_attach );

    if( get_dimension_ordering( n_volume_dims,
                                vol_dimension_names,
                                n_dimensions,
                                dim_names,
                                file->to_volume_index,
                                file->to_file_index ) != VIO_OK )
    {
        FREE( file );
        delete_dimension_names( volume_to_attach, vol_dimension_names );
        mifree_volume_props( hprops );
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
                mifree_volume_props( hprops );
                
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

            mifree_volume_props( hprops );
            return( NULL );
        }
    }

    for_less( d, 0, n_dimensions )
    {
        file->sizes_in_file[d] = (long) sizes[d];
        file->indices[d] = 0;
        file->dim_names[d] = create_string( dim_names[d] );

        if(equal_strings( dim_names[d], MIvector_dimension ) )
        {
          micreate_dimension(file->dim_names[d],
                           MI_DIMCLASS_RECORD,
                           MI_DIMATTR_REGULARLY_SAMPLED,
                           (misize_t)file->sizes_in_file[d],
                           &minc_dimensions[d] );
        } else if (equal_strings( dim_names[d], MItime ) ) {
          micreate_dimension(file->dim_names[d],
                           MI_DIMCLASS_TIME,
                           MI_DIMATTR_REGULARLY_SAMPLED,/*TODO: make sure time is regularly sampled here*/
                           (misize_t)file->sizes_in_file[d],
                           &minc_dimensions[d] );
        } else {
          micreate_dimension(file->dim_names[d],
                           MI_DIMCLASS_SPATIAL,
                           MI_DIMATTR_REGULARLY_SAMPLED,
                           (misize_t)file->sizes_in_file[d],
                           &minc_dimensions[d] );
        }
    }

    if( output_world_transform( file, volume_to_attach->coordinate_system_name,
                                voxel_to_world_transform,
                                options->use_volume_starts_and_steps , minc_dimensions) != VIO_OK )
    {
      /*TODO: print error message*/
      mifree_volume_props( hprops );
      FREE( file );
      return( NULL );
    }
    /*--- create the file */

    if( micreate_volume ( file->filename,
                          n_dimensions ,
                          minc_dimensions,
                          file_minc_datatype,
                          MI_CLASS_REAL,
                          hprops,
                          &file->minc2id) <0 )
    {
        print_error( "Error: creating MINC2 file \"%s\".\n", file->filename );
        mifree_volume_props( hprops );
        return( NULL );
    }
    
    if (  micreate_volume_image ( file->minc2id ) <0 )
    {
        print_error( "Error: creating MINC2 file \"%s\".\n", file->filename );
        mifree_volume_props( hprops );
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

    miset_volume_valid_range(file->minc2id,file->valid_range[1],file->valid_range[0]);
    
    if( file->image_range[0] < file->image_range[1] || options->is_labels )
    {
      miset_slice_scaling_flag(file->minc2id, 0 );
      miset_volume_range(file->minc2id, file->image_range[1], file->image_range[0]);
    } else {
      get_volume_real_range( volume_to_attach, &file->image_range[0], &file->image_range[1] );
      /*miset_slice_scaling_flag(file->minc2id, 1 );*/
      miset_slice_scaling_flag(file->minc2id, 0 );
      miset_volume_range(file->minc2id,file->image_range[1],file->image_range[0]);
    }

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

VIOAPI  VIO_Status  copy_auxiliary_data_from_minc2_file(
    Minc_file    file,
    VIO_STR      filename,
    VIO_STR      history_string )
{
    VIO_Status  status;
    VIO_STR  expanded;
    mihandle_t    minc_id;


    if( file->ignoring_because_cached )
        return( VIO_OK );

    expanded = expand_filename( filename );
    
    if ( miopen_volume(expanded, MI2_OPEN_READ, &minc_id) < 0 )
    {
        print_error( "Error opening %s\n", expanded );

        delete_string( expanded );

        return VIO_ERROR;
    }

    
    status = copy_auxiliary_data_from_open_minc2_file( file, minc_id,
                                                      history_string );
    
    delete_string( expanded );
    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : copy_auxiliary_data_from_open_minc2_file
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

VIOAPI  VIO_Status  copy_auxiliary_data_from_open_minc2_file(
    Minc_file    file,
    mihandle_t   minc_id,
    VIO_STR      history_string )
{
    VIO_Status  status;
    
    /*TODO: convert this to MINC2*/

#if 0
    int     src_img_var, varid, n_excluded, excluded_vars[10];
    int     i, src_min_id, src_max_id, src_root_id;

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
    if( file->end_def_done )
    {
        print_error( "Cannot call copy_auxiliary_data_from_open_minc_file when not in define mode\n" );
        return( VIO_ERROR );
    }

    n_excluded = 0;

    /*TODO: convert this to MINC2*/
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
#endif /*0*/

    status = VIO_OK;

    if( history_string != NULL )
        status = add_minc2_history( file, history_string );


    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : add_minc2_history
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

VIOAPI  VIO_Status  add_minc2_history(
    Minc_file    file,
    VIO_STR      history_string )
{
    VIO_STR   new_history;
    size_t    minc_history_length=0;
    size_t    new_history_length=0;

    if( file->end_def_done )
    {
        print_error( "Cannot call add_minc_history when not in define mode\n" );
        return( VIO_ERROR );
    }
    
    if(miget_attr_length(file->minc2id,"","history",&minc_history_length) == MI_NOERROR)
    {
      new_history_length=minc_history_length+strlen(history_string)+1;
      new_history=alloc_string(new_history_length);
      
      if( miget_attr_values(file->minc2id,MI_TYPE_STRING,"","history",minc_history_length,new_history) == MI_NOERROR)
      {
        concat_to_string( &new_history, history_string );
        miset_attr_values(file->minc2id,MI_TYPE_STRING,"","history",new_history_length,new_history);
      }
      delete_string( new_history );
    } else {
      miset_attr_values(file->minc2id,MI_TYPE_STRING,"","history",strlen(history_string)+1,history_string);
    }
    
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
    VIO_STR      vol_dim_names[],
    int          n_file_dims,
    VIO_STR      file_dim_names[],
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
@NAME       : check_minc2_output_variables
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

static  VIO_Status  check_minc2_output_variables(
    Minc_file   file )
{
    VIO_Real          voxel_min, voxel_max;
    VIO_Volume        volume;

    if( !file->variables_written )
    {
        volume = file->volume;

        file->variables_written = TRUE;

        get_volume_voxel_range( volume, &voxel_min, &voxel_max );
        
        if( voxel_min < voxel_max )
        {
          miset_volume_valid_range(file->minc2id,voxel_max,voxel_min);
        }
        else
          print_error( "Volume has invalid min and max voxel value\n" );
        
        if ( volume->is_labels )
          miset_volume_range(file->minc2id,voxel_max,voxel_min);
    }

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_minc2_output_random_order
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

VIOAPI  VIO_Status  set_minc2_output_random_order(
    Minc_file   file )
{
    VIO_Status  status;

    status = check_minc2_output_variables( file );

    file->outputting_in_order = FALSE;

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_minc2_hyperslab
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

static  VIO_Status  output_minc2_hyperslab(
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
    misize_t         long_file_start[VIO_MAX_DIMENSIONS];
    misize_t         long_file_count[VIO_MAX_DIMENSIONS];
    void             *void_ptr;
    VIO_BOOL         direct_from_array, non_full_size_found;
    int              tmp_ind, tmp_sizes[VIO_MAX_DIMENSIONS];
    int              array_indices[VIO_MAX_DIMENSIONS];
    int              array_counts[MAX_VAR_DIMS];
    VIO_Status       status;
    VIO_multidim_array buffer_array;

    status = check_minc2_output_variables( file );

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
    status = VIO_OK;
    
    if( file->volume->is_labels)
    {
      if( miset_voxel_value_hyperslab(
          file->minc2id,
          vio_type_to_minc2_type(data_type),
          long_file_start,long_file_count,void_ptr) < 0 )
        status = VIO_ERROR;
    } 
    else 
    {
      if( miset_hyperslab_with_icv(
          file->minc2id,
          vio_type_to_minc2_type(data_type),
          long_file_start,long_file_count,void_ptr) < 0 )
        status = VIO_ERROR;
    }
    if( !direct_from_array )
        delete_multidim_array( &buffer_array );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_slab2
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

static  void  output_slab2(
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
        
        output_minc2_hyperslab( file, get_volume_data_type(volume),
                                      n_slab_dims, slab_sizes, array_data_ptr,
                                      to_array, int_file_start, int_file_count);
        delete_multidim_array( &array );
    }
    else
    {
        GET_MULTIDIM_PTR( array_data_ptr, volume->array,
                          volume_start[0], volume_start[1], volume_start[2],
                          volume_start[3], volume_start[4] );
        get_volume_sizes( volume, volume_sizes );

        output_minc2_hyperslab( file, get_volume_data_type(volume),
                                      get_volume_n_dimensions(volume),
                                      volume_sizes, array_data_ptr,
                                      to_volume,
                                      int_file_start, int_file_count );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_the_volume2
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
static  VIO_Status  output_the_volume2(
    Minc_file   file,
    VIO_Volume  volume,
    int         volume_count[],
    long        file_start[] )
{
    VIO_Status        status;
    int               d, n_volume_dims, sizes[VIO_MAX_DIMENSIONS];
    int               slab_size, n_slab, this_count;
    int               vol_index, step, n_steps;
    int               to_volume_index[MAX_VAR_DIMS];
    int               to_file_index[VIO_MAX_DIMENSIONS];
    long              file_indices[MAX_VAR_DIMS];
    long              count[MAX_VAR_DIMS];
    VIO_Real          real_min, real_max;
    VIO_STR           *vol_dimension_names;
    VIO_BOOL          increment;
    VIO_progress_struct   progress;

    status = check_minc2_output_variables( file );

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
                print_error( "output_the_volume2: invalid volume count.\n" );
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

        if(   file_start[d] < 0 || 
            ( file_start[d] + (long) this_count ) > file->sizes_in_file[d] )
        {
            print_error( "output_the_volume2:  invalid minc file position.\n" );
            print_error( "    start[%d] = %ld     count[%d] = %d\n", d, file_start[d],
                      d, this_count );
            return( VIO_ERROR );
        }
    }

    /* --- determine which contiguous blocks of volume to output
       to max out the read/write buffer and make it like the
       chunking dimensions for compression (for efficiency) */

    file->n_slab_dims = 0;
    slab_size = 1;
    n_steps = 1;

    for( d = file->n_file_dimensions-1; d >= 0; d-- ) {
      count[d] = 1;
      file_indices[d] = file_start[d];
      if( to_volume_index[d] != INVALID_AXIS ) {
        count[d] = volume_count[to_volume_index[d]];
        file->n_slab_dims++;  /* integral number of complete dimensions */
        slab_size *= count[d];
      }
    }

    if( file->n_slab_dims == 0 ) {
      print_error( "Error: You seem to be processing a minc file\n" );
      print_error( "with no valid axes. Please check your volume.\n" );
      exit(1);
    }

    /*DUMB code for fake interslice scaling*/
    if( file->image_range[0] >= file->image_range[1] || volume->is_labels)
      get_volume_real_range( volume, &real_min, &real_max );

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
        
        if( file->image_range[0] >= file->image_range[1] && !volume->is_labels)
          miset_slice_range(file->minc2id,(const misize_t*)file_indices,file->n_file_dimensions,real_max,real_min);
        
        output_slab2( file, volume, to_volume_index, file_indices, local_count );

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
      print_error( "Error: Your output minc file may be incomplete\n" );
      print_error( "(wrote only %d out of %d buffers)\n", step, n_steps );
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
#ifdef OUTPUT_MNC2_UNUSED
VIOAPI  VIO_Status  output_volume_to_minc2_file_position(
    Minc_file   file,
    VIO_Volume  volume,
    int         volume_count[],
    long        file_start[] )
{
    file->outputting_in_order = FALSE;

    return( output_the_volume2( file, volume, volume_count, file_start ) );
}
#endif /* OUTPUT_MNC2_UNUSED */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_minc2_volume
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

VIOAPI  VIO_Status  output_minc2_volume(
    Minc_file   file )
{
    int        d, volume_count[VIO_MAX_DIMENSIONS];
    VIO_BOOL    increment;

    /*--- check number of volumes written */

    d = 0;
    while( d < file->n_file_dimensions &&
           file->to_volume_index[d] != INVALID_AXIS )
        ++d;

    if( d < file->n_file_dimensions &&
        file->indices[d] >= file->sizes_in_file[d] )
    {
        print_error(
             "output_minc2_volume: attempted to write too many subvolumes.\n");
        return( VIO_ERROR );
    }

    get_volume_sizes( file->volume, volume_count );

    if( output_the_volume2( file, file->volume, volume_count,
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

VIOAPI  VIO_Status  close_minc2_output(
    Minc_file   file )
{
    int    d;

    if( file == (Minc_file) NULL )
    {
        print_error( "close_minc_output(): NULL file.\n" );
        return( VIO_ERROR );
    }

    if( file->outputting_in_order && !file->entire_file_written )
    {
        print_error( "Warning:  the MINC2 file has been " );
        print_error( "closed without writing part of it.\n");
    }

    for_less( d, 0, file->n_file_dimensions )
        delete_string( file->dim_names[d] );
      
    miclose_volume( file->minc2id );
    
    delete_string( file->filename );

    FREE( file );

    return( VIO_OK );
}

#endif /*HAVE_MINC2*/
