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

#ifdef HAVE_MINC1
#include  <minc.h>
#endif /*HAVE_MINC1*/

#define  DEFAULT_SUFFIX  "fre"

#define  NUM_BYTE_VALUES      256

static  VIO_Status  input_slice(
    volume_input_struct   *volume_input );

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_free_format_input
@INPUT      : 
@OUTPUT     : volume
              volume_input
@RETURNS    : VIO_OK if successful
@DESCRIPTION: Initializes loading a free format file by reading the header.
              If the file contains short, but the convert_to_byte flag is set
              in volume_input, then the file is converted to bytes on input.
              This function assumes that volume->filename has been assigned.
@CREATED    :                      David MacDonald
@MODIFIED   : May  22, 1997   D. MacDonald  - now uses volume starts
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  initialize_free_format_input(
    VIO_STR              filename,
    VIO_Volume           volume,
    volume_input_struct  *volume_input )
{
    VIO_Status     status, file_status;
    VIO_STR        volume_filename, abs_volume_filename, slice_filename;
    int            sizes[VIO_N_DIMENSIONS];
    int            c, volume_byte_offset, int_size;
    int            n_bytes_per_voxel, min_value, max_value, i;
    int            n_slices, n_voxels_in_slice;
    long           slice;
    nc_type        desired_data_type;
    int            value;
    char           ch;
    VIO_Real       file_separations[VIO_MAX_DIMENSIONS];
    VIO_Real       volume_separations[VIO_MAX_DIMENSIONS];
    VIO_Real       trans[VIO_N_DIMENSIONS];
    FILE           *file;
    VIO_BOOL       axis_valid;
    int            axis;

    file_status = open_file_with_default_suffix(
                         filename, DEFAULT_SUFFIX,
                         READ_FILE, ASCII_FORMAT, &file );

    status = file_status;

    abs_volume_filename = NULL;

    /* read the line containing: n_bytes_per_voxel
    */

    if( status == VIO_OK )
        status = input_int( file, &n_bytes_per_voxel );

    /* input the 3 translation values used for the voxel_to_world_transform */

    if( status == VIO_OK &&
        (input_real( file, &trans[VIO_X] ) != VIO_OK ||
         input_real( file, &trans[VIO_Y] ) != VIO_OK ||
         input_real( file, &trans[VIO_Z] ) != VIO_OK) )
    {
        print_error( "Error reading x,y,z translations from %s.\n", filename );
        status = VIO_ERROR;
    }

    if( status == VIO_OK )
    {
        /* decide what type of data is in image file */

        if( n_bytes_per_voxel == 1 )
            volume_input->file_data_type = VIO_UNSIGNED_BYTE;
        else if( n_bytes_per_voxel == 2 )
            volume_input->file_data_type = VIO_UNSIGNED_SHORT;
        else
        {
            print_error( "Must be either 1 or 2 bytes per voxel.\n" );
            status = VIO_ERROR;
        }

        /* decide how to store data in memory */

        if( get_volume_data_type(volume) == VIO_NO_DATA_TYPE )
            desired_data_type = NC_BYTE;
        else
            desired_data_type = volume->nc_data_type;
    }

    /* read 3 lines, 1 for each axis:

            number_voxels   +/-voxel_separation  x|y|z

       where the x, y, or z is used to indicate the ordering of the axes
             within the file, with the 3rd one being the fastest varying index.
             negative voxel separation means flip on display 
    */

    if( volume->spatial_axes[VIO_X] < 0 ||
        volume->spatial_axes[VIO_Y] < 0 ||
        volume->spatial_axes[VIO_Z] < 0 )
    {
        print_error(
         "warning initialize_free_format_input: setting spatial axes to XYZ.\n");
        volume->spatial_axes[VIO_X] = 0;
        volume->spatial_axes[VIO_Y] = 1;
        volume->spatial_axes[VIO_Z] = 2;
    }

    if( status == VIO_OK )
    for_less( axis, 0, VIO_N_DIMENSIONS )
    {
        status = VIO_ERROR;

        if( input_int( file, &int_size ) != VIO_OK )
            break;

        volume_input->sizes_in_file[axis] = (long) int_size;

        if( input_real( file, &file_separations[axis] ) != VIO_OK )
            break;

        if( input_nonwhite_character( file, &ch ) != VIO_OK )
            break;

        axis_valid = TRUE;

        switch( ch )
        {
        case 'x':
        case 'X':  volume_input->axis_index_from_file[axis] =
                                           volume->spatial_axes[VIO_X];
                   break;

        case 'y':
        case 'Y':  volume_input->axis_index_from_file[axis] =
                                           volume->spatial_axes[VIO_Y];
                   break;

        case 'z':
        case 'Z':  volume_input->axis_index_from_file[axis] =
                                           volume->spatial_axes[VIO_Z];
                   break;

        default:   axis_valid = FALSE;    break;
        }

        if( !axis_valid )
        {
            print_error( "Invalid axis.\n" );
            break;
        }

        status = VIO_OK;
    }

    for_less( c, 0, VIO_N_DIMENSIONS )
    {
        volume->spatial_axes[c] = c;
        volume->direction_cosines[c][VIO_X] = 0.0;
        volume->direction_cosines[c][VIO_Y] = 0.0;
        volume->direction_cosines[c][VIO_Z] = 0.0;
        volume->direction_cosines[c][c] = 1.0;
    }

    if( status == VIO_OK &&
        (volume_input->axis_index_from_file[VIO_X] ==
         volume_input->axis_index_from_file[VIO_Y] ||
         volume_input->axis_index_from_file[VIO_X] ==
         volume_input->axis_index_from_file[VIO_Z] ||
         volume_input->axis_index_from_file[VIO_Y] ==
         volume_input->axis_index_from_file[VIO_Z]) )
    {
        print_error( "Two axis indices are equal.\n" );
        status = VIO_ERROR;
    }

    volume_input->directory = extract_directory( filename );

    if( status == VIO_OK )
    {
      if( volume_input->sizes_in_file[0] <= 0 )
      {
          n_slices = 0;

          while( input_string( file, &slice_filename, (char) ' ' )==VIO_OK)
          {
              SET_ARRAY_SIZE( volume_input->slice_filenames, n_slices, n_slices+1,
                              DEFAULT_CHUNK_SIZE );
              volume_input->slice_filenames[n_slices] = slice_filename;
              SET_ARRAY_SIZE( volume_input->slice_byte_offsets,
                              n_slices, n_slices+1, DEFAULT_CHUNK_SIZE );

              if( input_int( file, &volume_input->slice_byte_offsets[n_slices] )
                  != VIO_OK )
              {
                  volume_input->slice_byte_offsets[n_slices] = 0;
              }

              ++n_slices;
          }

          volume_input->sizes_in_file[0] = (long) n_slices;
          volume_input->one_file_per_slice = TRUE;
      }
      else
      {
          volume_input->one_file_per_slice = FALSE;
          status = input_string( file, &volume_filename, (char) ' ' );
          abs_volume_filename = get_absolute_filename( volume_filename,
                                                       volume_input->directory );
          delete_string( volume_filename );

          if( input_int( file, &volume_byte_offset ) != VIO_OK )
              volume_byte_offset = 0;

      }
    }

    if( status == VIO_OK )
        status = close_file( file );

    /* record the information in the volume struct */

    if( status == VIO_OK )
    {
        for_less( axis, 0, VIO_N_DIMENSIONS )
        {
            sizes[volume_input->axis_index_from_file[axis]] =
                                 (int) volume_input->sizes_in_file[axis];
            volume_separations[volume_input->axis_index_from_file[axis]] =
                                                     file_separations[axis];

            if( volume_separations[axis] < 0.0 )
            {
                trans[axis] += -volume_separations[axis] *
                               (VIO_Real) (sizes[axis]-1);
            }
        }

        set_volume_separations( volume, volume_separations );
        set_volume_starts( volume, trans );
    }

    set_volume_type( volume, desired_data_type, FALSE, 0.0, 0.0 );
    set_volume_sizes( volume, sizes );

    /* allocate the slice buffer */

    n_voxels_in_slice = (int) volume_input->sizes_in_file[1] *
                        (int) volume_input->sizes_in_file[2];

    if( status == VIO_OK )
    switch( volume_input->file_data_type )
    {
    case  VIO_UNSIGNED_BYTE:
        ALLOC( volume_input->byte_slice_buffer, n_voxels_in_slice );
        break;

    case  VIO_UNSIGNED_SHORT:
        ALLOC( volume_input->short_slice_buffer, n_voxels_in_slice );
        break;

    default:
        handle_internal_error( "initialize_free_format_input" );
        break;
    }

    /* if the data must be converted to byte, read the entire image file simply
       to find the max and min values, and set the value_scale and
       value_translation */

    if( status == VIO_OK &&
        get_volume_data_type(volume) != volume_input->file_data_type )
    {
        if( status == VIO_OK && !volume_input->one_file_per_slice )
        {
            status = open_file( abs_volume_filename, READ_FILE, BINARY_FORMAT,
                                &volume_input->volume_file );

            if( status == VIO_OK )
                status = set_file_position( file, (long) volume_byte_offset );
        }

        min_value = 0;
        max_value = 0;

        volume_input->slice_index = 0;

        for_less( slice, 0, volume_input->sizes_in_file[0] )
        {
            (void) input_slice( volume_input );

            for_less( i, 0, n_voxels_in_slice )
            {
                value = (int) volume_input->short_slice_buffer[i];
                if( ( slice == 0 && i == 0 ) || value < min_value )
                    min_value = value;
                if( ( slice == 0 && i == 0 ) || value > max_value )
                    max_value = value;
            }
        }

        set_volume_voxel_range( volume, (VIO_Real) min_value, (VIO_Real) max_value );

        if( status == VIO_OK && !volume_input->one_file_per_slice )
            status = close_file( volume_input->volume_file );
    }

    if( status == VIO_OK && !volume_input->one_file_per_slice )
    {
        status = open_file( abs_volume_filename, READ_FILE, BINARY_FORMAT,
                            &volume_input->volume_file );

        delete_string( abs_volume_filename );

        if( status == VIO_OK )
            status = set_file_position( file, (long) volume_byte_offset );
    }

    volume_input->slice_index = 0;

    delete_string( abs_volume_filename );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : delete_free_format_input
@INPUT      : volume_input
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the slice buffer and closes the image file.
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  delete_free_format_input(
    volume_input_struct   *volume_input )
{
    long   slice;

    if( volume_input->file_data_type == VIO_UNSIGNED_BYTE )
    {
        FREE( volume_input->byte_slice_buffer );
    }
    else
    {
        FREE( volume_input->short_slice_buffer );
    }

    delete_string( volume_input->directory );

    if( volume_input->one_file_per_slice )
    {
        for_less( slice, 0, volume_input->sizes_in_file[0] )
            delete_string( volume_input->slice_filenames[slice] );

        FREE( volume_input->slice_filenames );
        FREE( volume_input->slice_byte_offsets );
    }
    else
    {
        (void) close_file( volume_input->volume_file );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_slice
@INPUT      : volume
              volume_input
@OUTPUT     : 
@RETURNS    : VIO_OK if success
@DESCRIPTION: Reads the next slice from the volume.
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  input_slice(
    volume_input_struct   *volume_input )
{
    VIO_Status           status;
    FILE             *file;
    VIO_STR           slice_filename;

    status = VIO_OK;

    if( (long) volume_input->slice_index < volume_input->sizes_in_file[0] )
    {
        if( volume_input->one_file_per_slice )
        {
            slice_filename = get_absolute_filename(
                   volume_input->slice_filenames[volume_input->slice_index],
                   volume_input->directory );
            status = open_file( slice_filename, READ_FILE, BINARY_FORMAT,
                                &file );

            delete_string( slice_filename );

            if( status == VIO_OK )
                status = set_file_position( file,
                           (long) (volume_input->slice_byte_offsets
                                           [volume_input->slice_index]) );
        }
        else
            file = volume_input->volume_file;

        if( status == VIO_OK )
        switch( volume_input->file_data_type )
        {
        case  VIO_UNSIGNED_BYTE:
            status = io_binary_data( file, READ_FILE,
                                 (void *) volume_input->byte_slice_buffer,
                                 sizeof(volume_input->byte_slice_buffer[0]),
                                 (int) volume_input->sizes_in_file[1] *
                                 (int) volume_input->sizes_in_file[2] );
            break;

        case  VIO_UNSIGNED_SHORT:
            status = io_binary_data( file, READ_FILE,
                                 (void *) volume_input->short_slice_buffer,
                                 sizeof(volume_input->short_slice_buffer[0]),
                                 (int) volume_input->sizes_in_file[1] *
                                 (int) volume_input->sizes_in_file[2] );
            break;

        default:
            handle_internal_error( "input_slice" );
            break;
        }

        if( status == VIO_OK && volume_input->one_file_per_slice )
        {
            status = close_file( file );
        }

        ++volume_input->slice_index;
    }
    else
        status = VIO_ERROR;

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_more_free_format_file
@INPUT      : volume
              volume_input
@OUTPUT     : fraction_done
@RETURNS    : TRUE if more to input after this call
@DESCRIPTION: Reads in one more slice from the image file.
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  input_more_free_format_file(
    VIO_Volume                volume,
    volume_input_struct   *volume_input,
    VIO_Real                  *fraction_done )
{
    VIO_Real            min_value, max_value, value;
    int             x, y, z, sizes[VIO_MAX_DIMENSIONS];
    long            i;
    VIO_Status          status;
    VIO_BOOL         more_to_do, scaling_flag;
    VIO_Real            value_translation, value_scale;
    VIO_Real            original_min_voxel, original_max_voxel;
    int             *inner_index, indices[VIO_MAX_DIMENSIONS];
    unsigned char   *byte_buffer_ptr;
    unsigned short  *short_buffer_ptr;

    if( (long) volume_input->slice_index < volume_input->sizes_in_file[0] )
    {
        if( !volume_is_alloced( volume ) ) {
            alloc_volume_data( volume );
            if( !volume_is_alloced( volume ) ) return( FALSE );
        }

        status = input_slice( volume_input );

        scaling_flag = FALSE;
        if( get_volume_data_type(volume) != volume_input->file_data_type )
        {
            scaling_flag = TRUE;
            get_volume_voxel_range( volume, &original_min_voxel,
                                    &original_max_voxel );
            value_translation = original_min_voxel;
            value_scale = (original_max_voxel - original_min_voxel) /
                          (VIO_Real) (NUM_BYTE_VALUES - 1);
        }

        inner_index = &indices[volume_input->axis_index_from_file[2]];

        indices[volume_input->axis_index_from_file[0]] =
                                         volume_input->slice_index-1;

        if( status == VIO_OK )
        switch( volume_input->file_data_type )
        {
        case  VIO_UNSIGNED_BYTE:
            byte_buffer_ptr = volume_input->byte_slice_buffer;
            for_less( i, 0, volume_input->sizes_in_file[1] )
            {
                indices[volume_input->axis_index_from_file[1]] = (int) i;
                for_less( *inner_index, 0, (int) volume_input->sizes_in_file[2] )
                {
                    if( scaling_flag )
                    {
                        value = ((VIO_Real) (*byte_buffer_ptr) - value_translation)/
                                   value_scale;

                        if( value < 0.0 )
                            value = 0.0;
                        else if( value > 255.0 )
                            value = 255.0;
                    }
                    else
                        value = (VIO_Real) (*byte_buffer_ptr);

                    set_volume_voxel_value( volume,
                               indices[VIO_X], indices[VIO_Y], indices[VIO_Z], 0, 0,
                               value );

                    ++byte_buffer_ptr;
                }
            }
            break;

        case  VIO_UNSIGNED_SHORT:
            short_buffer_ptr = volume_input->short_slice_buffer;
            for_less( i, 0, volume_input->sizes_in_file[1] )
            {
                indices[volume_input->axis_index_from_file[1]] = (int) i;
                for_less( *inner_index, 0, (int) volume_input->sizes_in_file[2])
                {
                    if( scaling_flag )
                    {
                        value = 
                         ((VIO_Real) (*short_buffer_ptr) - value_translation) /
                                               value_scale;
                    }
                    else
                        value = (VIO_Real) (*short_buffer_ptr);

                    set_volume_voxel_value( volume,
                               indices[VIO_X], indices[VIO_Y], indices[VIO_Z], 0, 0,
                               value );

                    ++short_buffer_ptr;
                }
            }
            break;

        default:
            handle_internal_error( "input_more_free_format_file" );
            break;
        }
    }

    get_volume_sizes( volume, sizes );

    *fraction_done = (VIO_Real) volume_input->slice_index /
                   (VIO_Real) sizes[volume_input->axis_index_from_file[0]];

    more_to_do = TRUE;

    if( volume_input->slice_index ==
        sizes[volume_input->axis_index_from_file[0]] )
    {
        more_to_do = FALSE;

        min_value = get_volume_voxel_value( volume, 0, 0, 0, 0, 0 );
        max_value = min_value;
        for_less( x, 0, sizes[VIO_X] )
        {
            for_less( y, 0, sizes[VIO_Y] )
            {
                for_less( z, 0, sizes[VIO_Z] )
                {
                    value = get_volume_voxel_value( volume, x, y, z, 0,0);
                    if( value < min_value )
                        min_value = value;
                    else if( value > max_value )
                        max_value = value;
                }
            }
        }

        set_volume_voxel_range( volume, min_value, max_value );

        if( get_volume_data_type(volume) != volume_input->file_data_type )
        {
            set_volume_real_range( volume, original_min_voxel,
                                   original_max_voxel );
        }
    }

    return( more_to_do );
}
