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


static void calculate_volume_real_range(VIO_Volume volume,double *real_min,double *real_max);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_file_dimension_names
@INPUT      : filename
              n_dims
@OUTPUT     : dim_names
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Gets the names of the dimensions from the specified file.
              dim_names is an array of VIO_STRS, where the array has been
              allocated, but not each string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status   get_file_dimension_names(
    VIO_STR   filename,
    int      *n_dims,
    VIO_STR   *dim_names[] )
{
    int                   i;
    VIO_Status            status;
    volume_input_struct   volume_input;
    VIO_Volume            tmp_volume;

    status = start_volume_input( filename, -1, File_order_dimension_names,
                                 MI_ORIGINAL_TYPE, FALSE, 0.0, 0.0,
                                 TRUE, &tmp_volume, (minc_input_options *) NULL,
                                 &volume_input );

    if( status == VIO_OK )
    {
        *n_dims = get_volume_n_dimensions( tmp_volume );

        ALLOC( *dim_names, *n_dims );

        for_less( i, 0, *n_dims )
        {
            (*dim_names)[i] = create_string(
                           volume_input.minc_file->dim_names[i]);
        }

        delete_volume_input( &volume_input );
        delete_volume( tmp_volume );
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_output_dim_names
@INPUT      : volume
              original_filename
              options
@OUTPUT     : file_sizes
@RETURNS    : array of names
@DESCRIPTION: Creates an array of dimension names for file output.  If the
              options contains a set of names, they are used.  Otherwise,
              if the original_filename is specified (non-NULL), then the
              dimension names from it are used, if they contain all the
              dimension names in the volume.  Otherwise, those from the
              volume are used.  The file_sizes[] array is set to match
              the sizes in the volume cross-referenced with the dimension
              names.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Nov. 4, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_STR  *create_output_dim_names(
    VIO_Volume            volume,
    VIO_STR               original_filename,
    minc_output_options   *options,
    int                   file_sizes[] )
{
    int                  n_dims, n_file_dims, dim_index;
    int                  vol_sizes[VIO_MAX_DIMENSIONS];
    int                  i, j, n_found;
    VIO_STR               *file_dim_names;
    VIO_STR               *vol_dimension_names;
    VIO_STR               *dim_names;

    get_volume_sizes( volume, vol_sizes );

    n_dims = get_volume_n_dimensions(volume);
    vol_dimension_names = get_volume_dimension_names( volume );

    ALLOC( dim_names, n_dims );

    /*--- either get the output dim name ordering from the original
          filename, the volume, or the options */

    if( options != NULL && string_length( options->dimension_names[0] ) > 0 )
    {
        for_less( i, 0, n_dims )
            dim_names[i] = create_string( options->dimension_names[i] );
    }
    else
    {
        if( original_filename != NULL && file_exists(original_filename) &&
            get_file_dimension_names( original_filename, &n_file_dims,
                                      &file_dim_names ) == VIO_OK )
        {
            /*--- extract the dimension names from the file which match
                  those of the volume, so a 3D volume of z, y, x with a 4D file
                  of x y z t, will generate a list of dim_names: x y z */

            dim_index = 0;

            for_less( i, 0, n_file_dims )
            {
                for_less( j, 0, n_dims )
                {
                    if( equal_strings( vol_dimension_names[j],
                                       file_dim_names[i] ) )
                    {
                        dim_names[dim_index] = create_string(
                                                  vol_dimension_names[j] );
                        ++dim_index;
                        break;
                    }
                }

                if( dim_index == n_dims )  /*--- save time */
                    break;
            }

            /*--- check that all volume dimensions exist in the file */

            if( dim_index != n_dims )
            {
                for_less( i, 0, dim_index )
                    delete_string( dim_names[i] );

                for_less( i, 0, n_dims )
                    dim_names[i] = create_string( vol_dimension_names[i] );
            }

            /*--- free up the file dimension names */

            for_less( i, 0, n_file_dims )
                delete_string( file_dim_names[i] );

            FREE( file_dim_names );
        }
        else   /*--- no original file specified, use the volumes own list */
        {
            for_less( i, 0, n_dims )
                dim_names[i] = create_string( vol_dimension_names[i] );
        }
    }

    /*--- check that the set of dimension names are simply a permutation
          of the ones in the volume */

    n_found = 0;

    for_less( i, 0, n_dims )
    {
        for_less( j, 0, n_dims )
        {
            if( equal_strings( dim_names[j], vol_dimension_names[i] ) )
            {
                file_sizes[j] = vol_sizes[i];
                ++n_found;
            }
        }
    }

    /*--- act on whether the set of names was a permutation or not */

    if( n_found != n_dims )
    {
        print_error(
               "create_output_dim_names: dimension name mismatch.\n" );

        delete_dimension_names( volume, dim_names );

        dim_names = NULL;
    }

    /*--- no longer need the volume dimension names */

    delete_dimension_names( volume, vol_dimension_names );

    return( dim_names );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : copy_volume_auxiliary_and_history
@INPUT      : minc_file
              filename
              original_filename
              history
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: If the original_filename is specified, Copies the auxiliary
              data from it.  If the history is specified, adds the history
              line.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 24, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status   copy_volume_auxiliary_and_history(
    Minc_file    minc_file,
    VIO_STR      filename,
    VIO_STR      original_filename,
    VIO_STR      history )
{
    VIO_Status    status;
    VIO_BOOL   copy_original_file_data;
    VIO_STR    full_filename, full_original_filename;

    copy_original_file_data = FALSE;

    if( original_filename != NULL )
    {
        full_filename = expand_filename( filename );
        full_original_filename = expand_filename( original_filename );

        if( !equal_strings( full_filename, full_original_filename ) &&
            file_exists( full_original_filename ) )
        {
            copy_original_file_data = TRUE;
        }

        delete_string( full_filename );
        delete_string( full_original_filename );
    }

    status = VIO_OK;

    if( copy_original_file_data )
    {
#ifdef HAVE_MINC1
      if(!minc_file->using_minc2_api) status = copy_auxiliary_data_from_minc_file( minc_file,
                                                  original_filename,
                                                  history );
#endif
                                                  
#ifdef HAVE_MINC2
      if(minc_file->using_minc2_api) status = copy_auxiliary_data_from_minc2_file( minc_file,
                                                  original_filename,
                                                  history );
#endif
    }
    else if( history != NULL )
    {

#ifdef HAVE_MINC1
      if(!minc_file->using_minc2_api) 
              status = add_minc_history( minc_file, history );
#endif      
#ifdef HAVE_MINC2
      if(minc_file->using_minc2_api) 
              status = add_minc2_history( minc_file, history );
#endif       
    }
    
    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_modified_volume
@INPUT      : filename
              file_nc_data_type
              file_signed_flag
              file_voxel_min
              file_voxel_max
              volume
              original_filename
              history
              options
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Creates a Minc file and outputs the volume to it.  The data
              type of the file is either specified by the second through fifth
              parameters, or by the volume, if file_nc_data_type is
              MI_ORIGINAL_TYPE.  The volume is assumed to be derived, in some
              fashion, from an existing MINC file, and the auxiliary data
              from the existing MINC file, 'original_filename', is
              copied to the output file, along with the 'history' string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : May  22, 1997   D. MacDonald - now sets
                                            use_volume_starts_and_steps flag
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_modified_volume(
    VIO_STR                filename,
    nc_type                file_nc_data_type,
    VIO_BOOL               file_signed_flag,
    VIO_Real               file_voxel_min,
    VIO_Real               file_voxel_max,
    VIO_Volume             volume,
    VIO_STR                original_filename,
    VIO_STR                history,
    minc_output_options   *options )
{
    VIO_Status           status;
    Minc_file            minc_file=NULL;
    int                  n_dims, sizes[VIO_MAX_DIMENSIONS];
    VIO_Real             real_min, real_max;
    VIO_STR               *dim_names;
    minc_output_options  used_options;

    dim_names = create_output_dim_names( volume, original_filename,
                                         options, sizes );

    if( dim_names == NULL )
        return( VIO_ERROR );

    n_dims = get_volume_n_dimensions(volume);

    if( options == NULL )
        set_default_minc_output_options( &used_options );
    else 
        used_options = *options;

    if( used_options.global_image_range[0] >=
        used_options.global_image_range[1] || volume->is_labels )
    {
        get_volume_real_range( volume, &real_min, &real_max );
        
        /*HACK: fixing condition when outputting floating-point volume with default range*/
        if(real_min==-DBL_MAX || real_max==DBL_MAX)
        {
            calculate_volume_real_range(volume, &real_min, &real_max );
            set_volume_real_range(volume,real_min,real_max);
        }
        
        set_minc_output_real_range( &used_options, real_min, real_max );
    }

    /*--- if the user has not explicitly set the use_volume_starts_and_steps
          flag, let's set it if the transform is linear, to output the
          same starts as was input, and avoid round-off error */

    if( !used_options.use_starts_set &&
        !used_options.use_volume_starts_and_steps &&
        get_transform_type(get_voxel_to_world_transform(volume)) == LINEAR )
    {
        set_minc_output_use_volume_starts_and_steps_flag( &used_options, TRUE );
    }

#if defined(HAVE_MINC1) && defined(HAVE_MINC2)
    if(used_options.prefer_minc2_api) {
#endif
            
#if defined(HAVE_MINC2)
            minc_file = initialize_minc2_output( filename,
                                        n_dims, dim_names, sizes,
                                        file_nc_data_type, file_signed_flag,
                                        file_voxel_min, file_voxel_max,
                                        get_voxel_to_world_transform(volume),
                                        volume, &used_options );
#endif
                                        
#if defined(HAVE_MINC1) && defined(HAVE_MINC2)
    } else {
#endif
    
#ifdef HAVE_MINC1
    minc_file = initialize_minc_output( filename,
                                        n_dims, dim_names, sizes,
                                        file_nc_data_type, file_signed_flag,
                                        file_voxel_min, file_voxel_max,
                                        get_voxel_to_world_transform(volume),
                                        volume, &used_options );
#endif

#if defined(HAVE_MINC1) && defined(HAVE_MINC2)
    }
#endif
    
    if( minc_file == NULL )
        return( VIO_ERROR );

    status = copy_volume_auxiliary_and_history( minc_file, filename,
                                                original_filename, history );

    if( status == VIO_OK )
    {
            
#if defined(HAVE_MINC2)
          if(minc_file->using_minc2_api) status = output_minc2_volume( minc_file );
#endif
           
#if defined(HAVE_MINC1)
          if(!minc_file->using_minc2_api) status = output_minc_volume( minc_file );
#endif
        
    }
    
    if( status == VIO_OK )
    {
#if defined(HAVE_MINC1)
            if(minc_file->using_minc2_api) status = close_minc2_output( minc_file );
#endif
            
#if defined(HAVE_MINC1)
            if(!minc_file->using_minc2_api)  status = close_minc_output( minc_file );
#endif
    }
    
    delete_dimension_names( volume, dim_names );
    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_volume
@INPUT      : filename
              file_nc_data_type
              file_signed_flag
              file_voxel_min
              file_voxel_max
              volume
              history
              options
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sames as output_modified_volume, above, but the volume is not
              a modification of an existing MINC file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_volume(
    VIO_STR               filename,
    nc_type               file_nc_data_type,
    VIO_BOOL              file_signed_flag,
    VIO_Real              file_voxel_min,
    VIO_Real              file_voxel_max,
    VIO_Volume            volume,
    VIO_STR               history,
    minc_output_options   *options )
{
    return( output_modified_volume( filename, file_nc_data_type,
                                    file_signed_flag,
                                    file_voxel_min, file_voxel_max,
                                    volume, NULL, history, options ) );
}



/* HACK
 */
static void calculate_volume_real_range(VIO_Volume volume,double *real_min,double *real_max)
{
    int               volume_sizes[VIO_MAX_DIMENSIONS];
    int               v[VIO_MAX_DIMENSIONS];
    int               ndim=get_volume_n_dimensions(volume);
    int done=0;
    
    *real_min=DBL_MAX;
    *real_max=-DBL_MAX;
    
    get_volume_sizes( volume, volume_sizes );
    v[0]=v[1]=v[2]=v[3]=v[4]=0;
    
    do
    {
        int i;
        double value = get_volume_voxel_value( volume,
                                        v[0],
                                        v[1],
                                        v[2],
                                        v[3],
                                        v[4] );
        if(value>*real_max) *real_max=value;
        if(value<*real_min) *real_min=value;
        
        /*another hack*/
        for(i=0;i<ndim;i++)
        {
            int j;
            if(volume_sizes[i]==0) continue;
            
            v[i]++;
            if(v[i]<volume_sizes[i]) 
                break;
            
            if(i==(ndim-1)) {done=1;break;}
            
            for(j=0;j<=i;j++)
                v[j]=0;
        }
    } while(!done);

}
