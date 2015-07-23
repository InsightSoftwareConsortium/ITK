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

static   const char      * const TAG_FILE_HEADER = "MNI Tag Point File";
static   const char      * const VOLUMES_STRING = "Volumes";
static   const char      * const TAG_POINTS_STRING = "Points";

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_default_tag_file_suffix
@INPUT      : 
@OUTPUT     : 
@RETURNS    : "tag"
@DESCRIPTION: Returns the default tag file suffix.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_STR  get_default_tag_file_suffix( void )
{
    return( "tag" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_tag_file_output
@INPUT      : file
              comments
              n_volumes
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Writes the header and first part of a tag file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  initialize_tag_file_output(
    FILE      *file,
    VIO_STR    comments,
    int       n_volumes )
{
    VIO_Status   status;

    /* parameter checking */

    status = VIO_OK;

    if( file == NULL )
    {
        print_error( "start_writing_tags(): passed NULL FILE ptr.\n");
        status = VIO_ERROR;
    }

    if( n_volumes != 1 && n_volumes != 2 )
    {
        print_error( "output_tag_points():" );
        print_error( " can only support 1 or 2 volumes;\n" );
        print_error( "     you've supplied %d.\n", n_volumes );
        status = VIO_ERROR;
    }

    if( status == VIO_OK )
    {
        /* okay write the file header and tag points header */

        (void) fprintf( file, "%s\n", TAG_FILE_HEADER );
        (void) fprintf( file, "%s = %d;\n", VOLUMES_STRING, n_volumes );
        output_comments( file, comments );
        (void) fprintf( file, "\n" );

        (void) fprintf( file, "%s =", TAG_POINTS_STRING );
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_one_tag
@INPUT      : file
              n_volumes
              tag_volume1
              tag_volume2
              weight        - NULL if not desired to specify
              structure_id  - NULL if not desired to specify
              patient_id    - NULL if not desired to specify
              label         - NULL if not desired to specify
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Writes one tag to the output.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_one_tag(
    FILE      *file,
    int       n_volumes,
    VIO_Real      tag_volume1[],
    VIO_Real      tag_volume2[],
    VIO_Real      *weight,
    int       *structure_id,
    int       *patient_id,
    VIO_STR    label )
{
    VIO_Status   status;
    VIO_BOOL  aux_present;

    /* parameter checking */

    status = VIO_OK;

    (void) fprintf( file, "\n %.15g %.15g %.15g",
                    tag_volume1[0],
                    tag_volume1[1],
                    tag_volume1[2] );

    if( n_volumes >= 2 )
    {
        (void) fprintf( file, " %.15g %.15g %.15g",
                        tag_volume2[0],
                        tag_volume2[1],
                        tag_volume2[2] );
    }

    /*--- decide whether to output the 3 numerical information */

    aux_present = (weight != NULL ||
                   structure_id != NULL ||
                   patient_id != NULL);

    if( aux_present )
    {
        if( weight != (VIO_Real *) NULL )
            (void) fprintf( file, " %.15g", *weight );
        else
            (void) fprintf( file, " %.15g", 0.0 );

        if( structure_id != NULL )
            (void) fprintf( file, " %d", *structure_id );
        else
            (void) fprintf( file, " %d", -1 );

        if( patient_id != NULL )
            (void) fprintf( file, " %d", *patient_id );
        else
            (void) fprintf( file, " %d", -1 );
    }

    if( label != NULL )
        (void) fprintf( file, " \"%s\"", label );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : terminate_tag_file_output
@INPUT      : file
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Finishes writing the tag file, by placing the closing semicolon.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  terminate_tag_file_output(
    FILE    *file )
{
    (void) fprintf( file, ";\n" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_tag_points
@INPUT      : file
              comments       - may be null
              n_volumes
              n_tag_points
              tags_volume1
              tags_volume2
              weights
              structure_ids
              patient_ids
              labels
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Outputs the tag points in MNI tag point format.  If weights,
              structure_ids, and patient_ids are all NULL, they are not
              written to the file.  If labels is NULL, it is not written.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Oct. 19, 1995   D. MacDonald, now calls the 1 at a time routine
@MODIFIED   : Apr.  1, 1996   D. MacDonald, fixed bug of passing non-null
                              tags_volume2 with n_volumes==1
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_tag_points(
    FILE      *file,
    VIO_STR    comments,
    int       n_volumes,
    int       n_tag_points,
    VIO_Real      **tags_volume1,
    VIO_Real      **tags_volume2,
    VIO_Real      weights[],
    int       structure_ids[],
    int       patient_ids[],
    VIO_STR    *labels )
{
    VIO_Status   status;
    int      i;

    status = initialize_tag_file_output( file, comments, n_volumes );

    if( status == VIO_OK )
    {
        for( i = 0;  i < n_tag_points;  ++i )
        {
            status = output_one_tag( file, n_volumes,
                            tags_volume1[i],
                            (n_volumes == 1) ? NULL : tags_volume2[i],
                            weights == NULL ? NULL : &weights[i],
                            structure_ids == NULL ? NULL : &structure_ids[i],
                            patient_ids == NULL ? NULL : &patient_ids[i],
                            labels == NULL ? NULL : labels[i] );

            if( status != VIO_OK )
                break;
        }
    }

    if( status == VIO_OK )
        terminate_tag_file_output( file );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_tags
@INPUT      : tags
              n_tag_points
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the tag x,y,z positions.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static void free_tags(
    VIO_Real    **tags,
    int     n_tag_points )
{
    int   i;

    for( i = 0;  i < n_tag_points;  ++i )
        FREE( tags[i] );

    if( n_tag_points > 0 )
        FREE( tags );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_tag_points
@INPUT      : n_volumes
              n_tag_points
              tags_volume1
              tags_volume2
              weights
              structure_ids
              patient_ids
              labels
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the tag point data.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  free_tag_points(
    int       n_volumes,
    int       n_tag_points,
    VIO_Real      **tags_volume1,
    VIO_Real      **tags_volume2,
    VIO_Real      weights[],
    int       structure_ids[],
    int       patient_ids[],
    char      **labels )
{
    int   i;

    if( n_tag_points > 0 )
    {
        free_tags( tags_volume1, n_tag_points );

        if( n_volumes == 2 )
            free_tags( tags_volume2, n_tag_points );

        if( weights != (VIO_Real *) NULL )
            FREE( weights );

        if( structure_ids != (int *) NULL )
            FREE( structure_ids );

        if( patient_ids != (int *) NULL )
            FREE( patient_ids );

        if( labels != (char **) NULL )
        {
            for( i = 0;  i < n_tag_points;  ++i )
                delete_string( labels[i] );

            if( n_tag_points > 0 )
                FREE( labels );
        }
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : extract_label
@INPUT      : str
@OUTPUT     : label
@RETURNS    : 
@DESCRIPTION: Extracts the label from the string, by either taking the
              first space delimited word, or first quoted string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_STR extract_label(
    VIO_STR     str )
{
    VIO_BOOL  quoted;
    int      i;
    VIO_STR   label;

    i = 0;

    /* --- skip leading space */

    while( str[i] == ' ' || str[i] == '\t' )
        ++i;

    if( str[i] == '"' )
    {
        quoted = TRUE;
        ++i;
    }
    else
        quoted = FALSE;

    /* --- copy characters until either closing quote is found (if quoted),
           or white space or end of string is found */

    label = create_string( NULL );

    while( str[i] != VIO_END_OF_STRING &&
           ( (quoted && str[i] != '"') ||
             (!quoted && str[i] != ' ' && str[i] != '\t') ) )
    {
        concat_char_to_string( &label, str[i] );
        ++i;
    }

    return( label );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_tag_file_input
@INPUT      : file
@OUTPUT     : n_volumes
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Reads the tag file header and first part of file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  initialize_tag_file_input(
    FILE      *file,
    int       *n_volumes_ptr )
{
    VIO_STR  line;
    int     n_volumes;

    /* parameter checking */

    if( file == NULL )
    {
        print_error( "initialize_tag_file_input(): passed NULL FILE ptr.\n");
        return( VIO_ERROR );
    }

    /* okay read the header */

    if( mni_input_string( file, &line, (char) 0, (char) 0 ) != VIO_OK ||
        !equal_strings( line, TAG_FILE_HEADER ) )
    {
        print_error( "input_tag_points(): invalid header in file.\n");
        delete_string( line );
        return( VIO_ERROR );
    }

    delete_string( line );

    /* now read the number of volumes */

    if( mni_input_keyword_and_equal_sign( file, VOLUMES_STRING, TRUE ) != VIO_OK )
        return( VIO_ERROR );

    if( mni_input_int( file, &n_volumes ) != VIO_OK )
    {
        print_error( "input_tag_points(): expected # volumes after %s.\n",
                     VOLUMES_STRING );
        return( VIO_ERROR );
    }

    if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
        return( VIO_ERROR );

    if( n_volumes != 1 && n_volumes != 2 )
    {
        print_error( "input_tag_points(): invalid # volumes: %d \n",
                     n_volumes );
        return( VIO_ERROR );
    }

    /* now read the tag points header */

    if( mni_input_keyword_and_equal_sign( file, TAG_POINTS_STRING, TRUE ) != VIO_OK)
        return( VIO_ERROR );

    if( n_volumes_ptr != NULL )
        *n_volumes_ptr = n_volumes;

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : read_one_tag
@INPUT      : file
              n_volumes
@OUTPUT     : tags_volume1_ptr
              tags_volume2_ptr
              weight_ptr
              structure_id_ptr
              patient_id_ptr
              label_ptr
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs the file and passes back the data.  The last four arguments
              can each be set to NULL if the corresponding information is not
              desired.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_Status read_one_tag(
    FILE      *file,
    int       n_volumes,
    VIO_Real      tags_volume1_ptr[],
    VIO_Real      tags_volume2_ptr[],
    VIO_Real      *weight_ptr,
    int       *structure_id_ptr,
    int       *patient_id_ptr,
    VIO_STR    *label_ptr )
{
    VIO_Status  status;
    VIO_STR  line;
    VIO_BOOL last_was_blank, in_quotes;
    int     n_strings, pos, i;
    VIO_Real    x1 = 0.0, y1 = 0.0, z1 = 0.0, x2 = 0.0, y2 = 0.0, z2 = 0.0;
    int     structure_id, patient_id;
    VIO_Real    weight;
    VIO_STR  label;

    /* parameter checking */

    if( file == NULL )
    {
        print_error( "read_one_tag(): passed NULL FILE ptr.\n");
        return( VIO_ERROR );
    }

    status = mni_input_real( file, &x1 );

    if( status == VIO_OK )
    {
        if( mni_input_real( file, &y1 ) != VIO_OK ||
            mni_input_real( file, &z1 ) != VIO_OK ||
            (n_volumes == 2 &&
             (mni_input_real( file, &x2 ) != VIO_OK ||
              mni_input_real( file, &y2 ) != VIO_OK ||
              mni_input_real( file, &z2 ) != VIO_OK)) )
        {
            print_error( "read_one_tag(): error reading tag point\n" );
            return( VIO_ERROR );
        }

        if( tags_volume1_ptr != NULL )
        {
            tags_volume1_ptr[VIO_X] = x1;
            tags_volume1_ptr[VIO_Y] = y1;
            tags_volume1_ptr[VIO_Z] = z1;
        }

        if( n_volumes == 2 && tags_volume2_ptr != NULL )
        {
            tags_volume2_ptr[VIO_X] = x2;
            tags_volume2_ptr[VIO_Y] = y2;
            tags_volume2_ptr[VIO_Z] = z2;
        }

        label = NULL;
        weight = 0.0;
        structure_id = -1;
        patient_id = -1;

        n_strings = 0;
        if( mni_input_line( file, &line ) == VIO_OK )
        {
            i = 0;
            last_was_blank = TRUE;
            in_quotes = FALSE;
            while( line[i] != VIO_END_OF_STRING )
            {
                if( line[i] == ' ' || line[i] == '\t' )
                {
                    last_was_blank = TRUE;
                }
                else
                {
                    if( last_was_blank && !in_quotes )
                        ++n_strings;

                    last_was_blank = FALSE;

                    if( line[i] == '\"' )
                        in_quotes = !in_quotes;
                }
                ++i;
            }

            while( i > 0 &&
                   (line[i] == ' ' || line[i] == '\t' ||
                    line[i] == VIO_END_OF_STRING) )
                --i;

            if( line[i] == ';' )
            {
                (void) unget_character( file, (char) ';' );
                line[i] = VIO_END_OF_STRING;
            }
        }

        if( n_strings != 0 )
        {
            if( n_strings == 1 )
            {
                label = extract_label( line );
            }
            else if( n_strings < 3 || n_strings > 4 ||
                     sscanf( line, "%lf %d %d %n", &weight, &structure_id,
                             &patient_id, &pos ) != 3 )
            {
                print_error( "input_tag_points(): error reading tag point\n" );
                return( VIO_ERROR );
            }
            else if( n_strings == 4 )
            {
                label = extract_label( &line[pos] );
            }
        }

        delete_string( line );

        if( weight_ptr != NULL )
            *weight_ptr = weight;

        if( structure_id_ptr != NULL )
            *structure_id_ptr = structure_id;

        if( patient_id_ptr != NULL )
            *patient_id_ptr = patient_id;

        if( label_ptr != NULL )
            *label_ptr = label;
        else
            delete_string( label );
    }

    if( status == VIO_ERROR )  /* --- found no more tag points, should now find ; */
    {
        if( mni_skip_expected_character( file, (char) ';' ) != VIO_OK )
            status = VIO_ERROR;
        else
            status = VIO_END_OF_FILE;
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_tag_file
@INPUT      : filename
              comments
              n_volumes
              n_tag_points
              tags_volume1
              tags_volume2
              weights
              structure_ids
              patient_ids
              labels
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Opens the file, outputs the tag points, and closes the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : Oct. 19, 1995   D. MacDonald    - now calls the 1 at a time funcs
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  output_tag_file(
    VIO_STR    filename,
    VIO_STR    comments,
    int       n_volumes,
    int       n_tag_points,
    VIO_Real      **tags_volume1,
    VIO_Real      **tags_volume2,
    VIO_Real      weights[],
    int       structure_ids[],
    int       patient_ids[],
    VIO_STR    labels[] )
{
    VIO_Status  status;
    FILE    *file;

    status = open_file_with_default_suffix( filename,
                                            get_default_tag_file_suffix(),
                                            WRITE_FILE, ASCII_FORMAT, &file );

    if( status == VIO_OK )
        status = output_tag_points( file, comments, n_volumes, n_tag_points,
                                    tags_volume1, tags_volume2, weights,
                                    structure_ids, patient_ids, labels );

    if( status == VIO_OK )
        status = close_file( file );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_tag_file
@INPUT      : filename
@OUTPUT     : n_volumes
              n_tag_points
              tags_volume1
              tags_volume2
              weights
              structure_ids
              patient_ids
              labels
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Opens the file, inputs the tag points, and closes the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  input_tag_file(
    VIO_STR    filename,
    int       *n_volumes,
    int       *n_tag_points,
    VIO_Real      ***tags_volume1,
    VIO_Real      ***tags_volume2,
    VIO_Real      **weights,
    int       **structure_ids,
    int       **patient_ids,
    VIO_STR    *labels[] )
{
    VIO_Status  status;
    FILE    *file;

    status = open_file_with_default_suffix( filename,
                                            get_default_tag_file_suffix(),
                                            READ_FILE, ASCII_FORMAT, &file );

    if( status == VIO_OK )
        status = input_tag_points( file, n_volumes, n_tag_points,
                                   tags_volume1, tags_volume2, weights,
                                   structure_ids, patient_ids, labels );

    if( status == VIO_OK )
        status = close_file( file );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_one_tag
@INPUT      : file
              n_volumes
@OUTPUT     : tag_volume1
              tag_volume2
              weight
              structure_id
              patient_id
              label
              status
@RETURNS    : TRUE if successful.
@DESCRIPTION: Reads one tag point line from the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  input_one_tag(
    FILE      *file,
    int       n_volumes,
    VIO_Real      tag_volume1[],
    VIO_Real      tag_volume2[],
    VIO_Real      *weight,
    int       *structure_id,
    int       *patient_id,
    VIO_STR    *label,
    VIO_Status    *status )
{
    VIO_BOOL  read_one;
    VIO_Status   read_status;

    read_status = read_one_tag( file, n_volumes,
                                tag_volume1, tag_volume2, weight,
                                structure_id, patient_id, label );

    read_one = (read_status == VIO_OK);

    if( read_status == VIO_END_OF_FILE )
        read_status = VIO_OK;

    if( status != NULL )
        *status = read_status;

    return( read_one );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_tag_points
@INPUT      : file
@OUTPUT     : n_volumes
              n_tag_points
              tags_volume1
              tags_volume2
              weights
              structure_ids
              patient_ids
              labels
@RETURNS    : OR or VIO_ERROR
@DESCRIPTION: Inputs an entire tag point file into a set of arrays.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Status  input_tag_points(
    FILE      *file,
    int       *n_volumes_ptr,
    int       *n_tag_points,
    VIO_Real      ***tags_volume1,
    VIO_Real      ***tags_volume2,
    VIO_Real      **weights,
    int       **structure_ids,
    int       **patient_ids,
    VIO_STR    *labels[] )
{
    VIO_Status   status;
    VIO_Real     tags1[VIO_N_DIMENSIONS];
    VIO_Real     tags2[VIO_N_DIMENSIONS];
    VIO_Real     weight;
    int      structure_id, patient_id, n_volumes;
    VIO_STR   label;

    status = initialize_tag_file_input( file, &n_volumes );

    if( n_volumes_ptr != NULL )
        *n_volumes_ptr = n_volumes;

    *n_tag_points = 0;

    while( status == VIO_OK &&
           input_one_tag( file, n_volumes,
                          tags1, tags2, &weight, &structure_id, &patient_id,
                          &label, &status ) )
    {
        if( tags_volume1 != NULL )
        {
            SET_ARRAY_SIZE( *tags_volume1, *n_tag_points, *n_tag_points+1,
                            DEFAULT_CHUNK_SIZE );
            ALLOC( (*tags_volume1)[*n_tag_points], 3 );
            (*tags_volume1)[*n_tag_points][VIO_X] = tags1[VIO_X];
            (*tags_volume1)[*n_tag_points][VIO_Y] = tags1[VIO_Y];
            (*tags_volume1)[*n_tag_points][VIO_Z] = tags1[VIO_Z];
        }

        if( n_volumes == 2 && tags_volume2 != NULL )
        {
            SET_ARRAY_SIZE( *tags_volume2, *n_tag_points, *n_tag_points+1,
                            DEFAULT_CHUNK_SIZE );
            ALLOC( (*tags_volume2)[*n_tag_points], 3 );
            (*tags_volume2)[*n_tag_points][VIO_X] = tags2[VIO_X];
            (*tags_volume2)[*n_tag_points][VIO_Y] = tags2[VIO_Y];
            (*tags_volume2)[*n_tag_points][VIO_Z] = tags2[VIO_Z];
        }

        if( weights != NULL )
        {
            SET_ARRAY_SIZE( *weights, *n_tag_points, *n_tag_points+1,
                            DEFAULT_CHUNK_SIZE);
            (*weights)[*n_tag_points] = weight;
        }

        if( structure_ids != NULL )
        {
            SET_ARRAY_SIZE( *structure_ids, *n_tag_points, *n_tag_points+1,
                            DEFAULT_CHUNK_SIZE);
            (*structure_ids)[*n_tag_points] = structure_id;
        }

        if( patient_ids != NULL )
        {
            SET_ARRAY_SIZE( *patient_ids, *n_tag_points, *n_tag_points+1,
                            DEFAULT_CHUNK_SIZE);
            (*patient_ids)[*n_tag_points] = patient_id;
        }

        if( labels != NULL )
        {
            SET_ARRAY_SIZE( *labels, *n_tag_points, *n_tag_points+1,
                            DEFAULT_CHUNK_SIZE);
            (*labels)[*n_tag_points] = label;
        }
        else
            delete_string( label );

        ++(*n_tag_points);
    }

    return( status );
}
