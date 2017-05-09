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

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_check.c
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Maintains a skiplist structure to list all memory allocated,
            : and check for errors such as freeing a pointer twice or
            : overlapping allocations.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */


#if 0
#define  MAX_SKIP_LEVELS   50
#define  SKIP_P            0.5

#define  MEMORY_DIFFERENCE  1000000

typedef  struct skip_entry
{
    void                    *ptr;
    size_t                  n_bytes;
    VIO_STR                  source_file;
    int                     line_number;
    int                     sequence_number;
    struct  skip_entry      *forward[1];
} skip_entry;

typedef  struct
{
    size_t         next_memory_threshold;
    size_t         total_memory_allocated;
    skip_entry     *header;
    int            level;
} alloc_struct;

typedef  struct
{
    skip_entry   *update[MAX_SKIP_LEVELS];
} update_struct;

static  void     update_total_memory( alloc_struct *, size_t );
static  int      get_random_level( void );
static  void     output_entry( FILE *, skip_entry * );
static  VIO_BOOL  size_display_enabled( void );
static  size_t   skip_alloc_size = 0;

typedef  void      *alloc_ptr;

#define  ALLOC_SKIP_STRUCT( ptr, n_level )                                    \
     (ptr) = (skip_entry *) malloc(                                          \
          (sizeof(skip_entry)+((size_t)(n_level)-1) * sizeof(skip_entry *)) );

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_alloc_list
@INPUT      : alloc_list
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Initializes the allocation list to empty.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static   void  initialize_alloc_list(
    alloc_struct  *alloc_list )
{
    int   i;

    alloc_list->next_memory_threshold = MEMORY_DIFFERENCE;
    alloc_list->total_memory_allocated = 0;

    ALLOC_SKIP_STRUCT( alloc_list->header, MAX_SKIP_LEVELS );
    skip_alloc_size += sizeof(skip_entry)+(MAX_SKIP_LEVELS-1) *
                       sizeof(skip_entry *);
    alloc_list->level = 1;

    for_less( i, 0, MAX_SKIP_LEVELS )
        alloc_list->header->forward[i] = (skip_entry *) 0;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : check_initialized_alloc_list
@INPUT      : alloc_list
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Checks to make sure the allocation list is initialized.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  check_initialized_alloc_list(
    alloc_struct  *alloc_list )
{
    static   VIO_BOOL  first = TRUE;

    if( first )
    {
        first = FALSE;
        initialize_alloc_list( alloc_list );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : find_pointer_position
@INPUT      : alloc_list
            : ptr
@OUTPUT     : update
@RETURNS    : TRUE if found
@DESCRIPTION: Searches the alloc_list for the given ptr, and sets the update
            : struct so that it can provide an insert.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_BOOL  find_pointer_position(
    alloc_struct    *alloc_list,
    void            *ptr,
    update_struct   *update )
{
    int           i;
    skip_entry    *x;
    VIO_BOOL       found;

    x = alloc_list->header;

	i = alloc_list->level-1;
	if( i < 0 )
		return FALSE;
	
    for( ;  i >= 0;  --i )
    {
        while( x->forward[i] != NULL && (void *) x->forward[i]->ptr < ptr )
        {
            x = x->forward[i];
        }
        update->update[i] = x;
    }

    x = update->update[0]->forward[0];

    found = (x != NULL) && (x->ptr == ptr);

    return( found );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : insert_ptr_in_alloc_list
@INPUT      : alloc_list
            : update           - the set of pointers indicating where to insert
            : ptr              }
            : n_bytes          }}
            : source_file      }}} these are recorded in the list
            : line_number      }}
            : sequence_number    }
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Records the allocated pointer in the allocation list.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static   void  insert_ptr_in_alloc_list(
    alloc_struct   *alloc_list,
    update_struct  *update,
    void           *ptr,
    size_t         n_bytes,
    VIO_STR         source_file,
    int            line_number,
    int            sequence_number )
{
    int           i, new_level;
    skip_entry    *x;

    new_level = get_random_level();

    if( new_level > alloc_list->level )
    {
        for( i = alloc_list->level;  i < new_level;  ++i )
            update->update[i] = alloc_list->header;

        alloc_list->level = new_level;
    }

    ALLOC_SKIP_STRUCT( x, new_level );
    skip_alloc_size += sizeof(skip_entry)+((size_t)new_level-1) *
                       sizeof(skip_entry *);

    x->ptr = ptr;
    x->n_bytes = n_bytes;
    x->source_file = source_file;
    x->line_number = line_number;
    x->sequence_number = sequence_number;
    update_total_memory( alloc_list, n_bytes );

    for( i = 0;  i < new_level;  ++i )
    {
        x->forward[i] = update->update[i]->forward[i];
        update->update[i]->forward[i] = x;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : check_overlap
@INPUT      : update
            : ptr
            : n_bytes
@OUTPUT     : entry
@RETURNS    : TRUE if an overlap
@DESCRIPTION: Checks the new ptr to see if it overlaps with the previous and
            : following memory allocations in the list, and returns the result.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_BOOL  check_overlap(
    alloc_struct       *alloc_list,
    update_struct      *update,
    void               *ptr,
    size_t             n_bytes,
    skip_entry         **entry )
{
    VIO_BOOL      overlap;

    overlap = FALSE;

    *entry = update->update[0];

    if( *entry != alloc_list->header && *entry != (skip_entry *) 0 )
    {
        if( (void *) ((char *) (*entry)->ptr + (*entry)->n_bytes) > ptr )
             overlap = TRUE;
        else
        {
            (*entry) = (*entry)->forward[0];
            if( *entry != (skip_entry *) 0 &&
                (void *) ((char*)ptr + n_bytes) > (*entry)->ptr )
                overlap = TRUE;
        }
    }

    return( overlap );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : remove_ptr_from_alloc_list
@INPUT      : alloc_list
            : ptr
@OUTPUT     : source_file
            : line_number
            : sequence_number
@RETURNS    : TRUE if it existed
@DESCRIPTION: Finds and deletes the entry in the skip list associated with
            : ptr, and returns the information associated with the entry
            : (source_file, line_number, sequence_number).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static   VIO_BOOL  remove_ptr_from_alloc_list(
    alloc_struct   *alloc_list,
    void           *ptr,
    VIO_STR         *source_file,
    int            *line_number,
    int            *sequence_number )
{
    int           i;
    VIO_BOOL       found;
    skip_entry    *x;
    update_struct update;

    found = find_pointer_position( alloc_list, ptr, &update );

    if( found )
    {
        x = update.update[0]->forward[0];

        *source_file = x->source_file;
        *line_number = x->line_number;
        *sequence_number = x->sequence_number;

        update_total_memory( alloc_list, -x->n_bytes );

        for( i = 0;  i < alloc_list->level;  ++i )
        {
            if( update.update[i]->forward[i] != x )
                break;
            update.update[i]->forward[i] = x->forward[i];
        }

        skip_alloc_size -= sizeof(skip_entry) +
                           (size_t) (i-1) * sizeof(skip_entry *);

        free( (alloc_ptr) x );

        while( alloc_list->level > 1 &&
               alloc_list->header->forward[alloc_list->level-1] ==
                    (skip_entry *) 0 )
        {
            --alloc_list->level;
        }
    }

    return( found );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_random_0_to_1
@INPUT      : 
@OUTPUT     : 
@RETURNS    : random number
@DESCRIPTION: Returns a random number >= 0 and < 1.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Real  get_random_0_to_1( void )
{
    return( (VIO_Real) rand() );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_random_level
@INPUT      : 
@OUTPUT     : 
@RETURNS    : a random level between 1 and MAX_LEVELS
@DESCRIPTION: Determines a random level with exponential probability of higher
            : levels.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  int  get_random_level( void )
{
    int    level;

    level = 1;

    while( get_random_0_to_1() < SKIP_P && level < MAX_SKIP_LEVELS )
        ++level;

    return( level );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : memory_still_alloced
@INPUT      : alloc_list
@OUTPUT     : 
@RETURNS    : TRUE or FALSE
@DESCRIPTION: Decides if any memory is still alloced, thus checking for 
              memory leaks.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_BOOL  memory_still_alloced(
    alloc_struct  *alloc_list )
{
    return( alloc_list->header->forward[0] != (skip_entry *) NULL );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_alloc_list
@INPUT      : file
            : alloc_list
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Outputs the list of allocated memory to the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  output_alloc_list(
    FILE          *file,
    alloc_struct  *alloc_list )
{
    skip_entry  *ptr;

    ptr = alloc_list->header->forward[0];

    while( ptr != (skip_entry *) 0 )
    {
        output_entry( file, ptr );
        ptr = ptr->forward[0];
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : update_total_memory
@INPUT      : alloc_list
            : n_bytes
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Adds n_bytes to the size of memory recorded.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  update_total_memory(
    alloc_struct  *alloc_list,
    size_t        n_bytes )
{
    alloc_list->total_memory_allocated += n_bytes;

    if( size_display_enabled() &&
        alloc_list->total_memory_allocated >
        alloc_list->next_memory_threshold )
    {
        alloc_list->next_memory_threshold = MEMORY_DIFFERENCE *
                (alloc_list->total_memory_allocated / MEMORY_DIFFERENCE + 1);
        print( "Memory allocated =%5.1f Megabytes  (Overhead = %5.1f Mb)\n",
                (VIO_Real) alloc_list->total_memory_allocated / 1000000.0,
                (VIO_Real) skip_alloc_size / 1000000.0 );
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : print_source_location
@INPUT      : source_file
            : line_number
            : sequence_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Prints the information about a particular allocation.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  print_source_location(
    VIO_STR   source_file,
    int      line_number,
    int      sequence_number )
{
    print_error( "%s:%d\t%d'th alloc",
                 source_file, line_number, sequence_number );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_entry
@INPUT      : file
            : entry
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Outputs the information about an allocation entry to the file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  output_entry(
    FILE          *file,
    skip_entry    *entry )
{
    (void) fprintf( file, "%s:%d\t%d'th alloc\n",
                    entry->source_file,
                    entry->line_number,
                    entry->sequence_number );
}

/*  
--------------------------------------------------------------------------
    Routines that are to be called from outside this file
--------------------------------------------------------------------------
*/

static   alloc_struct   alloc_list;

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_total_memory_alloced
@INPUT      : 
@OUTPUT     : 
@RETURNS    : size_t  - the number of bytes allocated
@DESCRIPTION: Returns the total amount of memory allocated by the program,
            : not counting that used by the skip list.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  size_t  get_total_memory_alloced( void )
{
    return( alloc_list.total_memory_allocated );
}

static  VIO_BOOL  checking_enabled;
static  VIO_BOOL  enabled_initialized = FALSE;

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_checking_enabled
@INPUT      : 
@OUTPUT     : 
@RETURNS    : TRUE if alloc checking is turned on
@DESCRIPTION: Checks an environment variable to see if alloc checking is
            : not disabled.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  alloc_checking_enabled( void )
{
#ifdef NO_DEBUG_ALLOC
    return( FALSE );
#else
    if( !enabled_initialized )
    {
        set_alloc_checking( VIO_ENV_EXISTS( "DEBUG_ALLOC" ) );
    }

    return( checking_enabled );
#endif
}

VIOAPI  void  set_alloc_checking( VIO_BOOL state )
{
    enabled_initialized = TRUE;
    checking_enabled = state;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : size_display_enabled
@INPUT      : 
@OUTPUT     : 
@RETURNS    : TRUE if size displaying is turned on
@DESCRIPTION: Checks an environment variable to see if memory size display
            : is disabled.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_BOOL  size_display_enabled( void )
{
#ifdef NO_DEBUG_ALLOC
    return( FALSE );
#else
    static  VIO_BOOL  first = TRUE;
    static  VIO_BOOL  enabled;

    if( first )
    {
        enabled = VIO_ENV_EXISTS( "ALLOC_SIZE" );
        first = FALSE;
    }

    return( enabled );
#endif
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_stop_sequence_number
@INPUT      : 
@OUTPUT     : 
@RETURNS    : which allocation number
@DESCRIPTION: Returns the number at which allocation should stop.  This is
              used for debugging.  For instance, if an error message indicates
              a problem with the 100'th alloc of the program, then do a
              SETENV STOP_ALLOC_AT 100 and run the program from the debugger.
              It will stop at the requested allocation.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  int  get_stop_sequence_number( void )
{
    static   int   first = TRUE;
    static   int   stop_sequence_number = -1;
    VIO_STR         str;

    if( first )
    {
        first = FALSE;
        str = getenv( "STOP_ALLOC_AT" );
        if( str == NULL ||
            sscanf( str, "%d", &stop_sequence_number ) != 1 )
            stop_sequence_number = -1;
    }

    return( stop_sequence_number );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_current_sequence_number
@INPUT      : 
@OUTPUT     : 
@RETURNS    : the index of this alloc
@DESCRIPTION: Returns the count of how many allocations have been done, so that
              each allocation can be assigned a value equal to its cardinality
              in the set of allocations over the life of the program.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : 1993            David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  int  get_current_sequence_number( void )
{
    static   int  current_sequence_number = 0;

    ++current_sequence_number;

    if( current_sequence_number == get_stop_sequence_number() )
        handle_internal_error( "get_current_sequence_number" );

    return( current_sequence_number );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : record_ptr
@INPUT      : ptr
            : n_bytes
            : source_file
            : line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Records the information about a single allocation in the list.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  record_ptr_alloc_check(
    void      *ptr,
    size_t    n_bytes,
    VIO_STR    source_file,
    int       line_number )
{
    update_struct  update_ptrs;
    skip_entry     *entry;

    if( alloc_checking_enabled() )
    {
        check_initialized_alloc_list( &alloc_list );

        if( n_bytes == 0 )
        {
            print_source_location( source_file, line_number, -1 );
            print_error( ": Alloc called with zero size.\n" );
            abort_if_allowed();
        }
        else if( ptr == (void *) 0 )
        {
            print_source_location( source_file, line_number, -1 );
            print_error( ": Alloc returned a NIL pointer.\n" );
            abort_if_allowed();
        }
        else
        {
            (void) find_pointer_position( &alloc_list, ptr, &update_ptrs );

            if( check_overlap( &alloc_list, &update_ptrs, ptr, n_bytes, &entry))
            {
                print_source_location( source_file, line_number, -1 );
                print_error( 
                 ": Alloc returned a pointer overlapping an existing block:\n"
                 );
                print_source_location( entry->source_file, entry->line_number,
                                       entry->sequence_number );
                print_error( "\n" );
                abort_if_allowed();
            }
            else
                insert_ptr_in_alloc_list( &alloc_list,
                           &update_ptrs, ptr, n_bytes,
                           source_file, line_number,
                           get_current_sequence_number() );
        }
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : change_ptr
@INPUT      : old_ptr
            : new_ptr
            : n_bytes
            : source_file
            : line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Changes the information (mainly the n_bytes) associated with a
            : given pointer.  This function is called from the def_alloc
            : macros after a realloc().
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  change_ptr_alloc_check(
    void      *old_ptr,
    void      *new_ptr,
    size_t    n_bytes,
    VIO_STR    source_file,
    int       line_number )
{
    VIO_STR         orig_source;
    int            orig_line;
    int            sequence_number;
    skip_entry     *entry;
    update_struct  update_ptrs;

    if( alloc_checking_enabled() )
    {
        check_initialized_alloc_list( &alloc_list );

        if( n_bytes == 0 )
        {
            print_source_location( source_file, line_number, -1 );
            print_error( ": VIO_Realloc called with zero size.\n" );
            abort_if_allowed();
        }
        else if( !remove_ptr_from_alloc_list( &alloc_list, old_ptr,
                      &orig_source, &orig_line, &sequence_number ) )
        {
            print_source_location( source_file, line_number, -1 );
            print_error( ": Tried to realloc a pointer not already alloced.\n");
            abort_if_allowed();
        }
        else
        {
            (void) find_pointer_position( &alloc_list, new_ptr, &update_ptrs );

            if( check_overlap( &alloc_list, &update_ptrs, new_ptr, n_bytes,
                               &entry ) )
            {
                print_source_location( source_file, line_number, -1 );
                print_error( 
               ": VIO_Realloc returned a pointer overlapping an existing block:\n");
                print_source_location( entry->source_file, entry->line_number,
                                       entry->sequence_number );
                print_error( "\n" );
                abort_if_allowed();
            }
            else
                insert_ptr_in_alloc_list( &alloc_list,
                       &update_ptrs, new_ptr, n_bytes,
                       orig_source, orig_line, sequence_number );
        }
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : unrecord_ptr
@INPUT      : ptr
            : source_file
            : line_number
@OUTPUT     : 
@RETURNS    : TRUE if ptr was in list
@DESCRIPTION: Removes the entry for the given ptr from the list.  Called by
            : the macros during a FREE.  Returns TRUE if the pointer was
            : in the list.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  unrecord_ptr_alloc_check(
    void     *ptr,
    VIO_STR   source_file,
    int      line_number )
{
    VIO_BOOL  was_previously_alloced;
    VIO_STR   orig_source;
    int      orig_line;
    int      sequence_number;

    was_previously_alloced = TRUE;

    if( alloc_checking_enabled() )
    {
        check_initialized_alloc_list( &alloc_list );

        if( ptr == (void *) 0 )
        {
            print_source_location( source_file, line_number, -1 );
            print_error( ": Tried to free a NIL pointer.\n" );
            abort_if_allowed();
            was_previously_alloced = FALSE;
        }
        else if( !remove_ptr_from_alloc_list( &alloc_list, ptr, &orig_source,
                                              &orig_line, &sequence_number ) )
        {
            print_source_location( source_file, line_number, -1 );
            print_error( ": Tried to free a pointer not alloced.\n" );
            abort_if_allowed();
            was_previously_alloced = FALSE;
        }
    }

    return( was_previously_alloced );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : output_alloc_to_file
@INPUT      : filename
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Outputs a list of all memory allocated to the given file.  Usually
            : done at the end of the program to see if there is any memory that
            : was orphaned.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  output_alloc_to_file(
    VIO_STR   filename )
{
    FILE     *file;
    VIO_STR   date_str;

    if( alloc_checking_enabled() )
    {
        check_initialized_alloc_list( &alloc_list );

        if( memory_still_alloced( &alloc_list ) )
        {
            print_error( "\n" );
            print_error( "\n" );
            print_error( "A memory leak was found in this program.\n" );
            if( filename != NULL )
                print_error(
                    "A description has been recorded in the file %s.\n",
                       filename );
            print_error(
               "Please report this file to the author of the program.\n" );
            print_error( "\n" );

            if( filename != NULL && filename[0] != (char) 0 )
                file = fopen( filename, "w" );
            else
                file = stdout;

            if( file != NULL )
            {
                date_str = get_date();

                (void) fprintf( file, "Alloc table at %s\n", date_str );

                delete_string( date_str );

                output_alloc_list( file, &alloc_list );

                if( file != stdout )
                    (void) fclose( file );
            }
        }
    }
}
#endif

#ifndef  NO_DEBUG_ALLOC

VIOAPI  void  print_alloc_source_line(
    VIO_STR  filename,
    int     line_number )
{
    print_error( "    Source position: %s:%d\n", filename, line_number );
}

#endif
