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

/*--------------------- file format keywords ------------------------------ */

static const VIO_STR      TRANSFORM_FILE_HEADER = "MNI Transform File";
static const VIO_STR      TYPE_STRING = "Transform_Type";
static const VIO_STR      LINEAR_TRANSFORM_STRING = "Linear_Transform";
static const VIO_STR      LINEAR_TYPE = "Linear";
static const VIO_STR      THIN_PLATE_SPLINE_STRING ="Thin_Plate_Spline_Transform";
static const VIO_STR      INVERT_FLAG_STRING = "Invert_Flag";
static const VIO_STR      TRUE_STRING = "True";
static const VIO_STR      FALSE_STRING = "False";
static const VIO_STR      N_DIMENSIONS_STRING = "Number_Dimensions";
static const VIO_STR      POINTS_STRING = "Points";
static const VIO_STR      DISPLACEMENTS_STRING = "Displacements";
static const VIO_STR      GRID_TRANSFORM_STRING = "Grid_Transform";
static const VIO_STR      DISPLACEMENT_VOLUME = "Displacement_Volume";

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_default_transform_file_suffix
@INPUT      : 
@OUTPUT     : 
@RETURNS    : "xfm"
@DESCRIPTION: Returns the default transform file suffix.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_STR  get_default_transform_file_suffix( void )
{
    return( "xfm" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_one_transform
@INPUT      : file
              filename        \ these two used to manufacture unique filenames
              volume_count    / for grid transform volume files.
              invert  - whether to invert the transform
              transform
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Outputs a transform to the MNI transform file.
            : Increment *volume_count.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Feb 21, 1995    David MacDonald : added grid transforms 
---------------------------------------------------------------------------- */

static  void  output_one_transform(
    FILE                *file,
    const char          *filename,
    int                 *volume_count,
    VIO_BOOL             invert,
    VIO_General_transform   *transform )
{
    int        i, c, trans;
    VIO_Transform  *lin_transform;
    VIO_STR     volume_filename, base_filename, prefix_filename;

    switch( transform->type )
    {
    case LINEAR:
        (void) fprintf( file, "%s = %s;\n", TYPE_STRING, LINEAR_TYPE );

        (void) fprintf( file, "%s =\n", LINEAR_TRANSFORM_STRING );

        if( invert )
            lin_transform = get_inverse_linear_transform_ptr( transform );
        else
            lin_transform = get_linear_transform_ptr( transform );

        for_less( i, 0, 3 )
        {
            (void) fprintf( file, " %.15g %.15g %.15g %.15g",
                                  Transform_elem(*lin_transform,i,0),
                                  Transform_elem(*lin_transform,i,1),
                                  Transform_elem(*lin_transform,i,2),
                                  Transform_elem(*lin_transform,i,3) );
            if( i == 2 )
                (void) fprintf( file, ";" );
            (void) fprintf( file, "\n" );
        }
        break;

    case THIN_PLATE_SPLINE:
        (void) fprintf( file, "%s = %s;\n", TYPE_STRING,
                        THIN_PLATE_SPLINE_STRING);

        if( transform->inverse_flag )
            invert = !invert;

        if( invert )
            (void) fprintf( file, "%s = %s;\n", INVERT_FLAG_STRING,TRUE_STRING);

        (void) fprintf( file, "%s = %d;\n", N_DIMENSIONS_STRING,
                        transform->n_dimensions );

        (void) fprintf( file, "%s =\n", POINTS_STRING );

        for_less( i, 0, transform->n_points )
        {
            for_less( c, 0, transform->n_dimensions )
                (void) fprintf( file, " %.15g", transform->points[i][c] );

            if( i == transform->n_points-1 )
                (void) fprintf( file, ";" );

            (void) fprintf( file, "\n" );
        }

        (void) fprintf( file, "%s =\n", DISPLACEMENTS_STRING );

        for_less( i, 0, transform->n_points + transform->n_dimensions + 1 )
        {
            for_less( c, 0, transform->n_dimensions )
                (void) fprintf( file, " %.15g", transform->displacements[i][c]);

            if( i == transform->n_points + transform->n_dimensions + 1 - 1 )
                (void) fprintf( file, ";" );

            (void) fprintf( file, "\n" );
        }
        break;
        
    case GRID_TRANSFORM:
        (void) fprintf( file, "%s = %s;\n", TYPE_STRING,
                        GRID_TRANSFORM_STRING );

        if( transform->inverse_flag )
            invert = !invert;

        if( invert )
            (void) fprintf( file, "%s = %s;\n", INVERT_FLAG_STRING,TRUE_STRING);

        /*--- the volume will be stored in a file of the same prefix as this
              transform, but ending in _grid.mnc */

        if( filename == NULL || string_length(filename) == 0 )
            prefix_filename = create_string( "grid" );
        else
        {
            prefix_filename = create_string( filename );
            i = string_length( prefix_filename ) - 1;
            while( i > 0 && prefix_filename[i] != '.' &&
                   prefix_filename[i] != '/' )
                --i;
            if( i >= 0 && prefix_filename[i] == '.' )
                prefix_filename[i] = VIO_END_OF_STRING;
        }
        
        if( transform->displacement_volume_file )
        {
          delete_string( transform->displacement_volume_file );
          transform->displacement_volume_file = NULL;
        }
        
        /*--- write out the volume filename to the transform file */
/*        if( ! transform->displacement_volume_file )
        {*/
          volume_filename = alloc_string( string_length(prefix_filename) +
                                        100 );
          sprintf( volume_filename, "%s_grid_%d.mnc", prefix_filename,
                        *volume_count );
          
          transform->displacement_volume_file = volume_filename;

/*        }*/
        /* Increment the volume counter as a side-effect to ensure that grid
        * files have different names.
        */
        (*volume_count)++;

        /*--- decide where to write the volume file */

        base_filename = remove_directories_from_filename( transform->displacement_volume_file );

        fprintf( file, "%s = %s;\n", DISPLACEMENT_VOLUME, base_filename);

        /*--- write the volume file */

        if( transform->displacement_volume )
          output_volume( transform->displacement_volume_file, 
                              MI_ORIGINAL_TYPE, FALSE, 0.0, 0.0,
                              (VIO_Volume) transform->displacement_volume,
                              NULL, NULL );

        delete_string( prefix_filename );
        /*delete_string( volume_filename );*/
        delete_string( base_filename );

        break;

    case USER_TRANSFORM:
        print_error( "Cannot output user transformation.\n" );
        output_comments( file, "User transform goes here." );
        break;

    case CONCATENATED_TRANSFORM:
        
        if( transform->inverse_flag )
            invert = !invert;

        if( invert )
        {
            for( trans = get_n_concated_transforms(transform)-1;  trans >= 0;
                 --trans )
            {
                 output_one_transform( file, filename, volume_count, invert,
                               get_nth_general_transform(transform,trans) );
            }
        }
        else
        {
            for_less( trans, 0, get_n_concated_transforms(transform) )
            {
                 output_one_transform( file, filename, volume_count, invert,
                                   get_nth_general_transform(transform,trans) );
            }
        }
        break;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_transform
              file
              filename          \  these two args used to manufacture unique
              volume_count_ptr  /  filenames for grid transform volumes
              comments   - can be null
              transform
@INPUT      : 
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Outputs the transform to the file in MNI transform format.
              The filename is used to define the prefix for writing out
              volumes that are part of grid transforms.  The volume_count_ptr
              is used to give each volume written in a given file a unique
              index, and therefore a unique filename.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Feb. 21, 1995   D. MacDonald
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_transform(
    FILE                *file,
    const char          *filename,
    int                 *volume_count_ptr,
    const char          *comments,
    VIO_General_transform   *transform )
{
    int    volume_count;

    /* --- parameter checking */

    if( file == NULL )
    {
        print_error( "output_transform(): passed NULL FILE ptr.\n" );
        return( VIO_ERROR );
    }

    /* --- okay write the file */

    (void) fprintf( file, "%s\n", TRANSFORM_FILE_HEADER );

    output_comments( file, comments );
    (void) fprintf( file, "\n" );

    /*--- if the user has not initialized the volume count, then do so */

    if( volume_count_ptr == NULL )
    {
        volume_count = 0;
        volume_count_ptr = &volume_count;
    }

    output_one_transform( file, filename, volume_count_ptr, FALSE, transform );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_one_transform
@INPUT      : file
              filename    - used to get relative paths
@OUTPUT     : transform
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs a transform from the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Feb. 21, 1995   David MacDonald - added grid transforms
---------------------------------------------------------------------------- */

static VIO_Status input_one_transform(
    FILE                *file,
    const char          *filename,
    VIO_General_transform   *transform )
{
    VIO_Status        status;
    int               i, j, n_points, n_dimensions;
    VIO_Real          **points, **displacements;
    VIO_Real          value, *points_1d;
    VIO_STR           type_name, str, volume_filename, directory, tmp_filename;
    VIO_Volume        volume;
    VIO_Transform     linear_transform;
    VIO_Transform_types   transform_type;
    VIO_BOOL              inverse_flag;
    VIO_General_transform inverse;
    minc_input_options    options;

    inverse_flag = FALSE;

    /* --- read the type of transform */

    status = mni_input_keyword_and_equal_sign( file, TYPE_STRING, FALSE );

    if( status != VIO_OK )
        return( status );

    if( mni_input_string( file, &type_name, (char) ';', (char) 0 ) != VIO_OK )
    {
        print_error( "input_transform(): missing transform type.\n");
        return( VIO_ERROR );
    }
    if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
        return( VIO_ERROR );

    if( equal_strings( type_name, LINEAR_TYPE ) )
        transform_type = LINEAR;
    else if( equal_strings( type_name, THIN_PLATE_SPLINE_STRING ) )
        transform_type = THIN_PLATE_SPLINE;
    else if( equal_strings( type_name, GRID_TRANSFORM_STRING ) )
        transform_type = GRID_TRANSFORM;
    else
    {
        delete_string( type_name );
        print_error( "input_transform(): invalid transform type.\n");
        return( VIO_ERROR );
    }

    delete_string( type_name );

    /* --- read the next string */

    if( mni_input_string( file, &str, (char) '=', (char) 0 ) != VIO_OK )
        return( VIO_ERROR );

    if( equal_strings( str, INVERT_FLAG_STRING ) )
    {
        delete_string( str );

        if( mni_skip_expected_character( file, (char) '=' ) != VIO_OK )
            return( VIO_ERROR );
        if( mni_input_string( file, &str, (char) ';', (char) 0 ) != VIO_OK )
            return( VIO_ERROR );
        if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
        {
            delete_string( str );
            return( VIO_ERROR );
        }

        if( equal_strings( str, TRUE_STRING ) )
            inverse_flag = TRUE;
        else if( equal_strings( str, FALSE_STRING ) )
            inverse_flag = FALSE;
        else
        {
            delete_string( str );
            print_error( "Expected %s or %s after %s =\n",
                         TRUE_STRING, FALSE_STRING, INVERT_FLAG_STRING );
            return( VIO_ERROR );
        }

        delete_string( str );

        if( mni_input_string( file, &str, (char) '=', (char) 0 ) != VIO_OK )
            return( VIO_ERROR );
    }

    switch( transform_type )
    {
    default:
        print_error( "Unsupported transform type %d \n", transform_type );
        delete_string( str );
        return( VIO_ERROR );
        
    case LINEAR:
        if( !equal_strings( str, LINEAR_TRANSFORM_STRING ) )
        {
            print_error( "Expected %s =\n", LINEAR_TRANSFORM_STRING );
            delete_string( str );
            return( VIO_ERROR );
        }

        delete_string( str );

        if( mni_skip_expected_character( file, (char) '=' ) != VIO_OK )
            return( VIO_ERROR );

        make_identity_transform( &linear_transform );

        /* now read the 3 lines of transforms */

        for_less( i, 0, 3 )
        {
            for_less( j, 0, 4 )
            {
                if( mni_input_real( file, &value ) != VIO_OK )
                {
                    print_error(
                    "input_transform(): error reading transform elem [%d,%d]\n",
                    i+1, j+1 );
                    return( VIO_ERROR );
                }

                Transform_elem(linear_transform,i,j) = value;
            }
        }

        if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
            return( VIO_ERROR );

        create_linear_transform( transform, &linear_transform );

        break;

    case THIN_PLATE_SPLINE:

        /* --- read Number_Dimensions = 3; */

        if( !equal_strings( str, N_DIMENSIONS_STRING ) )
        {
            print_error( "Expected %s =\n", N_DIMENSIONS_STRING );
            delete_string( str );
            return( VIO_ERROR );
        }

        delete_string( str );

        if( mni_skip_expected_character( file, (char) '=' ) != VIO_OK )
            return( VIO_ERROR );
        if( mni_input_int( file, &n_dimensions ) != VIO_OK )
            return( VIO_ERROR );
        if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
            return( VIO_ERROR );

        /* --- read Points = x y z x y z .... ; */

        if( mni_input_keyword_and_equal_sign( file, POINTS_STRING, TRUE ) != VIO_OK)
            return( VIO_ERROR );
        if( mni_input_reals( file, &n_points, &points_1d ) != VIO_OK )
            return( VIO_ERROR );

        if( n_points % n_dimensions != 0 )
        {
            print_error(
                        "Number of points (%d) must be multiple of number of dimensions (%d)\n",
                        n_points, n_dimensions );
            return( VIO_ERROR );
        }

        n_points = n_points / n_dimensions;

        VIO_ALLOC2D( points, n_points, n_dimensions );
        for_less( i, 0, n_points )
        {
            for_less( j, 0, n_dimensions )
            {
                points[i][j] = points_1d[VIO_IJ(i,j,n_dimensions)];
            }
        }

        FREE( points_1d );

        /* --- allocate and input the displacements */

        VIO_ALLOC2D( displacements, n_points + n_dimensions + 1, n_dimensions );

        if( mni_input_keyword_and_equal_sign( file, DISPLACEMENTS_STRING, TRUE )
                                                                       != VIO_OK )
            return( VIO_ERROR );

        for_less( i, 0, n_points + n_dimensions + 1 )
        {
            for_less( j, 0, n_dimensions )
            {
                if( mni_input_real( file, &value ) != VIO_OK )
                {
                    print_error( "Expected more displacements.\n" );
                    return( VIO_ERROR );
                }
                displacements[i][j] = value;
            }
        }

        if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
            return( VIO_ERROR );

        create_thin_plate_transform_real( transform, n_dimensions,
                                          n_points, points, displacements );


        VIO_FREE2D( points );
        VIO_FREE2D( displacements );

        break;
        
    case GRID_TRANSFORM:

        /*--- read the displacement volume filename */

        if( !equal_strings( str, DISPLACEMENT_VOLUME ) )
        {
            print_error( "Expected %s =\n", DISPLACEMENT_VOLUME );
            delete_string( str );
            return( VIO_ERROR );
        }

        delete_string( str );

        if( mni_skip_expected_character( file, (char) '=' ) != VIO_OK )
            return( VIO_ERROR );

        if( mni_input_string( file, &volume_filename,
                              (char) ';', (char) 0 ) != VIO_OK )
            return( VIO_ERROR );

        if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
        {
            delete_string( volume_filename );
            return( VIO_ERROR );
        }

        /*--- if the volume filename is relative, add the required directory */

        if( volume_filename[0] != '/' && filename != NULL )
        {
            directory = extract_directory( filename );

            if( string_length(directory) > 0 )
            {
                tmp_filename = concat_strings( directory, "/" );
                concat_to_string( &tmp_filename, volume_filename );
                replace_string( &volume_filename, tmp_filename );
            }

            delete_string( directory );
        }

        /*--- input the displacement volume */

        /*TODO: initialize strings*/
        set_default_minc_input_options( &options );
        set_minc_input_vector_to_scalar_flag( &options, FALSE );

        if( input_volume( volume_filename, 4, NULL, 
                          MI_ORIGINAL_TYPE, FALSE, 0.0, 0.0, 
                          TRUE, &volume, &options ) != VIO_OK )
        {
            delete_string( volume_filename );
            return( VIO_ERROR );
        }
        create_grid_transform_no_copy( transform, volume, volume_filename );
        delete_string( volume_filename );

        /*--- create the transform */


        break;
    }

    if( inverse_flag )
    {
        create_inverse_general_transform( transform, &inverse );
        delete_general_transform( transform );
        *transform = inverse;
    }

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_transform
@INPUT      : file
              filename    - used to define directory for relative filename
@OUTPUT     : transform
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs the transform from the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Feb. 21, 1995   D. MacDonald
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  input_transform(
    FILE                *file,
    const char          *filename,
    VIO_General_transform   *transform )
{
    VIO_Status              status;
    int                 n_transforms;
    VIO_STR              line;
    VIO_General_transform   next, concated;

    /* parameter checking */

    if( file == (FILE *) 0 )
    {
        print_error( "input_transform(): passed NULL FILE ptr.\n");
        return( VIO_ERROR );
    }

    /* okay read the header */

    if( mni_input_string( file, &line, (char) 0, (char) 0 ) != VIO_OK )
    {
        delete_string( line );
        print_error( "input_transform(): could not read header in file.\n");
        return( VIO_ERROR );
    }

    if( !equal_strings( line, TRANSFORM_FILE_HEADER ) )
    {
        delete_string( line );
        print_error( "input_transform(): invalid header in file.\n");
        return( VIO_ERROR );
    }

    delete_string( line );

    n_transforms = 0;
    while( (status = input_one_transform( file, filename, &next )) == VIO_OK )
    {
        if( n_transforms == 0 )
            *transform = next;
        else
        {
            concat_general_transforms( transform, &next, &concated );
            delete_general_transform( transform );
            delete_general_transform( &next );
            *transform = concated;
        }
        ++n_transforms;
    }

    if( status == VIO_ERROR )
    {
        print_error( "input_transform: error reading transform.\n" );
        return( VIO_ERROR );
    }
    else if( n_transforms == 0 )
    {
        print_error( "input_transform: no transform present.\n" );
        return( VIO_ERROR );
    }

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_transform_file
@INPUT      : filename
              comments
              transform
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Opens the file, outputs the transform, and closes the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_transform_file(
    const char           *filename,
    const char           *comments,
    VIO_General_transform   *transform )
{
    VIO_Status  status;
    FILE    *file;
    int     volume_count;

    status = open_file_with_default_suffix( filename,
                      get_default_transform_file_suffix(),
                      WRITE_FILE, ASCII_FORMAT, &file );

    volume_count = 0;

    if( status == VIO_OK )
        status = output_transform( file, filename, &volume_count,
                                   comments, transform );

    if( status == VIO_OK )
        status = close_file( file );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_transform_file
@INPUT      : filename
@OUTPUT     : transform
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Opens the file, inputs the transform, and closes the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  input_transform_file(
    const char              *filename,
    VIO_General_transform   *transform )
{
    VIO_Status  status;
    FILE    *file;

    status = open_file_with_default_suffix( filename,
                      get_default_transform_file_suffix(),
                      READ_FILE, ASCII_FORMAT, &file );

    if( status == VIO_OK )
        status = input_transform( file, filename, transform );

    if( status == VIO_OK )
        status = close_file( file );

    return( status );
}
