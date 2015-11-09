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

static   const char      COMMENT_CHAR1 = '%';
static   const char      COMMENT_CHAR2 = '#';

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_get_nonwhite_character
@INPUT      : file
@OUTPUT     : ch
@RETURNS    : VIO_OK or VIO_END_OF_FILE
@DESCRIPTION: Gets the next non white space character from the MNI file
              (i.e., tags or transforms).  This routine handles comment
              characters, and is thus the base routine for all MNI tag or
              transform file input.  Any part of a line starting with a
              comment character is ignored.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_get_nonwhite_character(
    FILE   *file,
    char   *ch )
{
    VIO_BOOL  in_comment;
    VIO_Status   status;

    in_comment = FALSE;

    do
    {
        status = input_character( file, ch );
        if( status == VIO_OK )
        {
            if( *ch == COMMENT_CHAR1 || *ch == COMMENT_CHAR2 )
                in_comment = TRUE;
            else if( *ch == '\n' )
                in_comment = FALSE;
        }
    }
    while( status == VIO_OK &&
           (in_comment || *ch == ' ' || *ch == '\t' || *ch == '\n' || 
            *ch == '\r') );     /* ignore carriage returns */

    if( status == VIO_ERROR )
        status = VIO_END_OF_FILE;

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_skip_expected_character
@INPUT      : file
              expected_ch
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Gets the next nonwhite character.  If it is the expected
              character, fine, otherwise print an error message.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_skip_expected_character(
    FILE   *file,
    char   expected_ch )
{
    char     ch;
    VIO_Status   status;

    status = mni_get_nonwhite_character( file, &ch );

    if( status == VIO_OK )
    {
        if( ch != expected_ch )
        {
            print_error( "Expected '%c', found '%c'.\n", expected_ch, ch );
            status = VIO_ERROR;
        }
    }
    else
    {
        print_error( "Expected '%c', found end of file.\n", expected_ch );
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_input_line
@INPUT      : file
              max_length
@OUTPUT     : string
@RETURNS    : VIO_OK or VIO_END_OF_FILE
@DESCRIPTION: Inputs a line of text from a file.  The carriage return is
              read, but not placed in the string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_input_line(
    FILE     *file,
    VIO_STR   *string )
{
    VIO_Status   status;
    char     ch;

    *string = create_string( NULL );

    status = input_character( file, &ch );

    while( status == VIO_OK && ch != '\n' )
    {
        if (ch != '\r') {       /* Always ignore carriage returns */
            concat_char_to_string( string, ch );
        }

        status = input_character( file, &ch );
    }

    if( status != VIO_OK )
    {
        delete_string( *string );
        *string = NULL;
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_input_string
@INPUT      : file
              max_length
              termination_char1
              termination_char2
@OUTPUT     : string
@RETURNS    : VIO_OK or VIO_END_OF_FILE
@DESCRIPTION: Inputs a string from the file, up to the next occurrence of
              one of the termination characters or a carriage return.  If
              the first nonwhite character is a '"', then the termination
              characters become '"'.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_input_string(
    FILE     *file,
    VIO_STR   *string,
    char     termination_char1,
    char     termination_char2 )
{
    VIO_Status   status;
    char     ch;
    VIO_BOOL quoted;

    *string = create_string( NULL );

    status = mni_get_nonwhite_character( file, &ch );

    if( status == VIO_OK && ch == '"' )
    {
        quoted = TRUE;
        status = mni_get_nonwhite_character( file, &ch );
        termination_char1 = '"';
        termination_char2 = '"';
    }
    else
        quoted = FALSE;

    while( status == VIO_OK &&
           ch != termination_char1 && ch != termination_char2 && ch != '\n' )
    {
        if (ch != '\r') {       /* Always ignore carriage returns */
            concat_char_to_string( string, ch );
        }
        status = input_character( file, &ch );
    }

    if( !quoted )
        (void) unget_character( file, ch );

    while( string_length(*string) > 0 &&
           (*string)[string_length(*string)-1] == ' ' )
        (*string)[string_length(*string)-1] = VIO_END_OF_STRING;

    if( status != VIO_OK )
    {
        delete_string( *string );
        *string = NULL;
    }

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_input_keyword_and_equal_sign
@INPUT      : file
              keyword
              print_error_message - whether to print error messages
@OUTPUT     : 
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs the desired keyword from the file and an equal sign.
              If there is no match, then an error message may be printed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_input_keyword_and_equal_sign(
    FILE         *file,
    const char   keyword[],
    VIO_BOOL     print_error_message )
{
    VIO_Status     status;
    VIO_STR     str;

    status = mni_input_string( file, &str, (char) '=', (char) 0 );

    if( status == VIO_END_OF_FILE )
        return( status );

    if( status != VIO_OK || !equal_strings( str, keyword ) ||
        mni_skip_expected_character( file, (char) '=' ) != VIO_OK )
    {
        if( print_error_message )
            print_error( "Expected \"%s =\"\n", keyword );
        status = VIO_ERROR;
    }

    delete_string( str );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : unget_string
@INPUT      : file
              str
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Places the first nonblank character of the string back onto
              the input stream, as an approximation to pushing the entire
              string back on the input stream, which only happens in error
              situations.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static void  unget_string(
    FILE    *file,
    VIO_STR  str )
{
    int  len;

    len = 0;

    while( str[len] == ' ' || str[len] == '\t' )
        ++len;

    if( str[len] != VIO_END_OF_STRING )
        (void) unget_character( file, str[len] );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_input_real
@INPUT      : file
@OUTPUT     : d
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs an ascii representation of a real value.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_input_real(
    FILE    *file,
    VIO_Real    *d )
{
    VIO_Status   status;
    VIO_STR   str;

    status = mni_input_string( file, &str, (char) ' ', (char) ';' );

    if( status == VIO_OK && sscanf( str, "%lf", d ) != 1 )
    {
        unget_string( file, str );
        status = VIO_ERROR;
    }

    delete_string( str );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_input_reals
@INPUT      : file
@OUTPUT     : n
              reals
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs an arbitrary number of real values, up to the next
              semicolon.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_input_reals(
    FILE    *file,
    int     *n,
    VIO_Real    *reals[] )
{
    VIO_Real  d;

    *n = 0;

    while( mni_input_real( file, &d ) != VIO_ERROR )
    {
        ADD_ELEMENT_TO_ARRAY( *reals, *n, d, DEFAULT_CHUNK_SIZE );
    }

    return( mni_skip_expected_character( file, (char) ';' ) );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mni_input_int
@INPUT      : file
@OUTPUT     : i
@RETURNS    : VIO_OK or VIO_ERROR
@DESCRIPTION: Inputs an integer from an ascii file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI VIO_Status  mni_input_int(
    FILE    *file,
    int     *i )
{
    VIO_Status status;
    VIO_STR   str;

    status = mni_input_string( file, &str, (char) ' ', (char) ';' );

    if( status == VIO_OK && sscanf( str, "%d", i ) != 1 )
    {
        unget_string( file, str );
        status = VIO_ERROR;
    }

    delete_string( str );

    return( status );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_comments
@INPUT      : file
              comments
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Outputs a string to the file, in comment format, by placing
              a comment at the beginning of the string, and after each
              carriage return.  An extra carriage return is placed after
              the comments, if the comments do not end in a carriage return.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  output_comments(
    FILE       *file,
    const char *comments )
{
    int   i, len;

    if( comments != NULL )
    {
        len = string_length( comments );

        (void) output_character( file, COMMENT_CHAR1 );
        for( i = 0;  i < len;  ++i )
        {
            (void) output_character( file, comments[i] );
            if( comments[i] == '\n' && i < len - 1 )
                (void) output_character( file, COMMENT_CHAR1 );
        }

        if( len > 0 && comments[len-1] != '\n' )
            (void) output_character( file, (char) '\n' );
    }
}
