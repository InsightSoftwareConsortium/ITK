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

#include "minc_error.h"

#include  <internal_volume_io.h>
#include  <stdarg.h>

#define  MAX_PRINT_STACK  100

typedef  void (*print_function_type) ( VIO_STR );

static  print_function_type  print_function[MAX_PRINT_STACK] = { NULL };
static  int                  top_of_stack = 0;

static  print_function_type  print_error_function[MAX_PRINT_STACK] = { NULL };
static  int                  top_of_error_stack = 0;

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_print_function
@INPUT      : function
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the output function.  If you use the function print()
              everywhere, in place of printf, then by default it uses
              printf to send output to stdout.  However, you can call
              the set_print_function() to tell it to use a different output
              function, e.g. output to a GL or X window.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_print_function( void  (*function) ( VIO_STR ) )
{
    print_function[top_of_stack] = function;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : push_print_function
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Save the current print function, so, for instance, you can
              print to stdout temporarily.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  push_print_function( void )
{
    if( top_of_stack < MAX_PRINT_STACK - 1 )
    {
        ++top_of_stack;
        print_function[top_of_stack] = NULL;
    }
    else
        handle_internal_error( "Stack overflow in push_print_function" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : pop_print_function
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Restore the print function.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  pop_print_function( void )
{
    if( top_of_stack > 0 )
        --top_of_stack;
    else
        handle_internal_error( "Stack underflow in pop_print_function" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : print
@INPUT      : exactly same arguments as printf
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: prints the arguments to a temporary string buffer, then either
              printf's the or calls the user installed function to output
              the string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

/* VARARGS */
VIOAPI  void  print( VIO_STR format, ... )
{
    va_list  ap;
    char     print_buffer[VIO_EXTREMELY_LARGE_STRING_SIZE];

    va_start( ap, format );
    (void) vsprintf( print_buffer, format, ap );
    va_end( ap );

    if( print_function[top_of_stack] == NULL )
        (void) printf( "%s", print_buffer );
    else
        (*(print_function[top_of_stack])) ( print_buffer );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_print_error_function
@INPUT      : function
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the output function.  If you use the function print_error()
              everywhere, in place of printf, then by default it uses
              printf to send output to stderr.  However, you can call
              the set_print_error_function() to tell it to use a different
              output function, e.g. output to a GL or X window.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_print_error_function( void  (*function) ( char [] ) )
{
    print_error_function[top_of_error_stack] = function;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : push_print_error_function
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Save the current print error function, so, for instance, you can
              print to stdout temporarily.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  push_print_error_function( void )
{
    if( top_of_error_stack < MAX_PRINT_STACK - 1 )
    {
        ++top_of_error_stack;
        print_error_function[top_of_error_stack] = NULL;
    }
    else
        handle_internal_error( "Stack overflow in push_print_error_function" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : pop_print_error_function
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Restore the print_error function.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  pop_print_error_function( void )
{
    if( top_of_error_stack > 0 )
        --top_of_error_stack;
    else
        handle_internal_error( "Stack underflow in pop_print_error_function" );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : print_error
@INPUT      : exactly same arguments as printf
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: prints the arguments to a temporary string buffer, then either
              fprintf's to stderr or calls the user installed function to
              output the string.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

/* VARARGS */
VIOAPI  void  print_error( char format[], ... )
{
    va_list  ap;
    char     print_buffer[VIO_EXTREMELY_LARGE_STRING_SIZE];

    va_start( ap, format );
    vsprintf( print_buffer, format, ap );
    va_end( ap );

    if( print_error_function[top_of_error_stack] == NULL )
        MI_LOG_ERROR(MI_MSG_VOLUME_IO, print_buffer);
    else
        (*(print_error_function[top_of_error_stack])) ( print_buffer );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : handle_internal_error
@INPUT      : str
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Prints the error string and tries to get users permission to
              abort.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void   handle_internal_error( char  str[] )
{
    /*print_error( "Internal error:  %s\n", str );
    abort_if_allowed();*/
    MI_LOG_ERROR(MI_MSG_VOLUME_IO,"Internal error:  %s", str);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : abort_if_allowed
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Checks if the user wants to abort.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  abort_if_allowed( void )
{
    char  ch;

    if( VIO_ENV_EXISTS( "ABORT_FLAG" ) )
    {
        print_error( "Do you wish to abort (y/n): " );
        do
        {
            ch = (char) getchar();
        }
        while( ch != 'y' && ch != 'n' );

        while( getchar() != '\n' )
        {
        }

        if( ch == 'y' )
        {
            abort();
        }
    }
}
