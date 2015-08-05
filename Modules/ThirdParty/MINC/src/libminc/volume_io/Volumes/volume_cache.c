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


#define   HASH_FUNCTION_CONSTANT          0.6180339887498948482
#define   HASH_TABLE_SIZE_FACTOR          3

#define   DEFAULT_BLOCK_SIZE              64
#define   DEFAULT_CACHE_THRESHOLD         -1
#define   DEFAULT_MAX_BYTES_IN_CACHE      100000000

static  VIO_BOOL  n_bytes_cache_threshold_set = FALSE;
static  int      n_bytes_cache_threshold = DEFAULT_CACHE_THRESHOLD;

static  VIO_BOOL  default_cache_size_set = FALSE;
static  int      default_cache_size = DEFAULT_MAX_BYTES_IN_CACHE;


static  VIO_Cache_block_size_hints   block_size_hint = RANDOM_VOLUME_ACCESS;
static  VIO_BOOL  default_block_sizes_set = FALSE;
static  int      default_block_sizes[VIO_MAX_DIMENSIONS] = {
                                                     DEFAULT_BLOCK_SIZE,
                                                     DEFAULT_BLOCK_SIZE,
                                                     DEFAULT_BLOCK_SIZE,
                                                     DEFAULT_BLOCK_SIZE,
                                                     DEFAULT_BLOCK_SIZE };

static  void  alloc_volume_cache(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume );

#ifdef  CACHE_DEBUGGING
static  void  initialize_cache_debug(
    VIO_volume_cache_struct  *cache );

static  void  record_cache_hit(
    VIO_volume_cache_struct  *cache );

static  void  record_cache_prev_hit(
    VIO_volume_cache_struct  *cache );

static  void  record_cache_no_hit(
    VIO_volume_cache_struct  *cache );
#endif

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_n_bytes_cache_threshold
@INPUT      : threshold
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the threshold number of bytes which decides if a volume
              is small enough to be held entirely in memory, or whether it
              should be cached.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_n_bytes_cache_threshold(
    int  threshold )
{
    n_bytes_cache_threshold = threshold;
    n_bytes_cache_threshold_set = TRUE;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_n_bytes_cache_threshold
@INPUT      : 
@OUTPUT     : 
@RETURNS    : number of bytes
@DESCRIPTION: Returns the number of bytes defining the cache threshold.  If it
              hasn't been set, returns the program initialized value, or the
              value set by the environment variable.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_n_bytes_cache_threshold( void )
{
    int   n_bytes;

    if( !n_bytes_cache_threshold_set )
    {
        if( getenv( "VOLUME_CACHE_THRESHOLD" ) != NULL &&
            sscanf( getenv( "VOLUME_CACHE_THRESHOLD" ), "%d", &n_bytes ) == 1 )
        {
            n_bytes_cache_threshold = n_bytes;
        }
        n_bytes_cache_threshold_set = TRUE;
    }

    return( n_bytes_cache_threshold );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_default_max_bytes_in_cache
@INPUT      : max_bytes 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the default value for the maximum amount of memory
              in a single volume's cache.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_default_max_bytes_in_cache(
    int   max_bytes )
{
    default_cache_size_set = TRUE;
    default_cache_size = max_bytes;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_default_max_bytes_in_cache
@INPUT      : 
@OUTPUT     : 
@RETURNS    : number of bytes
@DESCRIPTION: Returns the maximum number of bytes allowed for a single
              volume's cache.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_default_max_bytes_in_cache( void )
{
    int   n_bytes;

    if( !default_cache_size_set )
    {
        if( getenv( "VOLUME_CACHE_SIZE" ) != NULL &&
            sscanf( getenv( "VOLUME_CACHE_SIZE" ), "%d", &n_bytes ) == 1 )
        {
            default_cache_size = n_bytes;
        }

        default_cache_size_set = TRUE;
    }

    return( default_cache_size );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_default_cache_block_sizes
@INPUT      : block_sizes
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the default values for the volume cache block sizes.
              A non-positive value will result in a block size equal to the
              number of voxels in that dimension of the volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 19, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_default_cache_block_sizes(
    int                      block_sizes[] )
{
    int   dim;

    for_less( dim, 0, VIO_MAX_DIMENSIONS )
        default_block_sizes[dim] = block_sizes[dim];

    default_block_sizes_set = TRUE;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_cache_block_sizes_hint
@INPUT      : hint
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the hint for deciding on block sizes.  This turns off
              the default_block_sizes_set flag, thereby overriding any
              previous calls to set_default_cache_block_sizes().
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 25, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_cache_block_sizes_hint(
    VIO_Cache_block_size_hints  hint )
{
    block_size_hint = hint;
    default_block_sizes_set = FALSE;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_default_cache_block_sizes
@INPUT      : 
@OUTPUT     : block_sizes[]
@RETURNS    : 
@DESCRIPTION: Passes back the size (in voxels) of each dimension of a cache
              block.  If it hasn't been set, returns the program initialized
              value, or the value set by the environment variable.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  get_default_cache_block_sizes(
    int    n_dims,
    int    volume_sizes[],
    int    block_sizes[] )
{
    int   dim, block_size;

    if( !default_block_sizes_set && block_size_hint == SLICE_ACCESS )
    {
        for_less( dim, 0, n_dims - 2 )
            block_sizes[dim] = 1;

        /*--- set the last two dimensions to be entire size of dimension */

        for_less( dim, MAX( 0, n_dims - 2), VIO_MAX_DIMENSIONS )
            block_sizes[dim] = -1;
    }
    else if( !default_block_sizes_set &&
             block_size_hint == RANDOM_VOLUME_ACCESS )
    {
        if( getenv( "VOLUME_CACHE_BLOCK_SIZE" ) == NULL ||
            sscanf( getenv( "VOLUME_CACHE_BLOCK_SIZE" ), "%d", &block_size )
                    != 1 || block_size < 1 )
        {
            block_size = DEFAULT_BLOCK_SIZE;
        }

        for_less( dim, 0, VIO_MAX_DIMENSIONS )
            block_sizes[dim] = block_size;
    }
    else
    {
        for_less( dim, 0, VIO_MAX_DIMENSIONS )
            block_sizes[dim] = default_block_sizes[dim];
    }

    /*--- now change any non-positive values to the correct volume size */

    for_less( dim, 0, VIO_MAX_DIMENSIONS )
    {
        if( block_sizes[dim] <= 0 || block_sizes[dim] > volume_sizes[dim] )
            block_sizes[dim] = volume_sizes[dim];
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_volume_cache
@INPUT      : cache
              volume
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Initializes the cache for a volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  initialize_volume_cache(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume )
{
    int    dim, n_dims, sizes[VIO_MAX_DIMENSIONS];

    n_dims = get_volume_n_dimensions( volume );
    cache->n_dimensions = n_dims;
    cache->writing_to_temp_file = FALSE;

    for_less( dim, 0, VIO_MAX_DIMENSIONS )
        cache->file_offset[dim] = 0;

    cache->minc_file = NULL;
    cache->input_filename = NULL;
    cache->output_filename = NULL;
    cache->original_filename = NULL;
    cache->history = NULL;
    set_default_minc_output_options( &cache->options );
    cache->output_file_is_open = FALSE;
    cache->must_read_blocks_before_use = FALSE;

    get_volume_sizes( volume, sizes );

    get_default_cache_block_sizes( n_dims, sizes, cache->block_sizes );
    cache->max_cache_bytes = get_default_max_bytes_in_cache();

    alloc_volume_cache( cache, volume );

#ifdef CACHE_DEBUGGING
    initialize_cache_debug( cache );
#endif
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_volume_cache
@INPUT      : cache
              volume
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Allocates the volume cache.  Uses the current value of the
              volumes max cache size and block sizes to decide how much to
              allocate.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  alloc_volume_cache(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume )
{
    int    dim, n_dims, sizes[VIO_MAX_DIMENSIONS], block, block_size;
    int    x, block_stride, remainder, block_index;

    get_volume_sizes( volume, sizes );
    n_dims = get_volume_n_dimensions( volume );

    /*--- count number of blocks needed per dimension */

    block_size = 1;
    block_stride = 1;

    for_down( dim, n_dims - 1, 0 )
    {
        cache->blocks_per_dim[dim] = (sizes[dim] - 1) / cache->block_sizes[dim]
                                     + 1;

        ALLOC( cache->lookup[dim], sizes[dim] );
        for_less( x, 0, sizes[dim] )
        {
            remainder = x % cache->block_sizes[dim];
            block_index = x / cache->block_sizes[dim];
            cache->lookup[dim][x].block_index_offset =
                                       block_index * block_stride;
            cache->lookup[dim][x].block_offset = remainder * block_size;
        }

        block_size *= cache->block_sizes[dim];
        block_stride *= cache->blocks_per_dim[dim];
    }

    cache->total_block_size = block_size;
    cache->max_blocks = cache->max_cache_bytes / block_size /
                        get_type_size(get_volume_data_type(volume));

    if( cache->max_blocks < 1 )
        cache->max_blocks = 1;

    /*--- create and initialize an empty hash table */

    cache->hash_table_size = cache->max_blocks * HASH_TABLE_SIZE_FACTOR;

    ALLOC( cache->hash_table, cache->hash_table_size );

    for_less( block, 0, cache->hash_table_size )
        cache->hash_table[block] = NULL;

    /*--- set up the initial pointers */

    cache->previous_block_index = -1;
    cache->head = NULL;
    cache->tail = NULL;
    cache->n_blocks = 0;
}

VIOAPI  VIO_BOOL  volume_cache_is_alloced(
    VIO_volume_cache_struct   *cache )
{
    return( cache->hash_table != NULL );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_block_start
@INPUT      : cache
              block_index
@OUTPUT     : block_start[]
@RETURNS    : 
@DESCRIPTION: Computes the starting voxel indices for a block.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  get_block_start(
    VIO_volume_cache_struct  *cache,
    int                  block_index,
    int                  block_start[] )
{
    int    dim, block_i;

    for_down( dim, cache->n_dimensions-1, 0 )
    {
        block_i = block_index % cache->blocks_per_dim[dim];
        block_start[dim] = block_i * cache->block_sizes[dim];
        block_index /= cache->blocks_per_dim[dim];
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : write_cache_block
@INPUT      : cache
              volume
              block
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Writes out a cache block to the appropriate position in the
              corresponding file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  write_cache_block(
    VIO_volume_cache_struct  *cache,
    VIO_Volume               volume,
    VIO_cache_block_struct   *block )
{
    Minc_file        minc_file;
    int              dim, ind, n_dims;
    int              file_start[VIO_MAX_DIMENSIONS];
    int              file_count[VIO_MAX_DIMENSIONS];
    int              volume_sizes[VIO_MAX_DIMENSIONS];
    int              block_start[VIO_MAX_DIMENSIONS];
    void             *array_data_ptr;

    minc_file = (Minc_file) cache->minc_file;

    get_block_start( cache, block->block_index, block_start );

    get_volume_sizes( volume, volume_sizes );

    for_less( dim, 0, minc_file->n_file_dimensions )
    {
        ind = minc_file->to_volume_index[dim];
        if( ind >= 0 )
        {
            file_start[dim] = cache->file_offset[dim] + block_start[ind];
            file_count[dim] = MIN( volume_sizes[ind] - file_start[dim],
                                   cache->block_sizes[ind] );
        }
        else
        {
            file_start[dim] = cache->file_offset[dim];
            file_count[dim] = 0;
        }
    }

    GET_MULTIDIM_PTR( array_data_ptr, block->array, 0, 0, 0, 0, 0 );
    n_dims = cache->n_dimensions;

#ifdef HAVE_MINC1
    output_minc_hyperslab( (Minc_file) cache->minc_file,
                                  get_multidim_data_type(&block->array),
                                  n_dims, cache->block_sizes, array_data_ptr,
                                  minc_file->to_volume_index,
                                  file_start, file_count );
#elif  defined HAVE_MINC2 
    /*TODO: Write out minc file using MINC2 api*/
#endif
    cache->must_read_blocks_before_use = TRUE;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : flush_cache_blocks
@INPUT      : cache
              volume
              deleting_volume_flag
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Writes out all blocks that have been modified, unless we are
              writing to a temporary file and the volume is being deleted.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  flush_cache_blocks(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume,
    VIO_BOOL               deleting_volume_flag )
{
    VIO_cache_block_struct  *block;

    /*--- don't bother flushing if deleting volume and just writing to temp */

    if( cache->writing_to_temp_file && deleting_volume_flag )
        return;

    /*--- step through linked list, freeing blocks */

    block = cache->head;
    while( block != NULL )
    {
        if( block->modified_flag )
        {
            write_cache_block( cache, volume, block );
            block->modified_flag = FALSE;
        }

        block = block->next_used;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : flush_volume_cache
@INPUT      : volume
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Writes out all blocks that have been modified.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  flush_volume_cache(
    VIO_Volume                volume )
{
    flush_cache_blocks( &volume->cache, volume, FALSE );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : delete_cache_blocks
@INPUT      : cache
              volume
              deleting_volume_flag  - TRUE if deleting the volume
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Deletes all cache blocks, writing out all blocks, if the volume
              has been modified.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  delete_cache_blocks(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume,
    VIO_BOOL               deleting_volume_flag )
{
    int                 block;
    VIO_cache_block_struct  *current, *next;

    /*--- if required, write out cache blocks */

    if( !cache->writing_to_temp_file || !deleting_volume_flag )
        flush_cache_blocks( cache, volume, deleting_volume_flag );

    /*--- step through linked list, freeing blocks */

    current = cache->head;
    while( current != NULL )
    {
        next = current->next_used;
        delete_multidim_array( &current->array );
        FREE( current );
        current = next;
    }

    /*--- initialize cache to no blocks present */

    cache->n_blocks = 0;

    for_less( block, 0, cache->hash_table_size )
        cache->hash_table[block] = NULL;

    cache->previous_block_index = -1;
    cache->head = NULL;
    cache->tail = NULL;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : delete_volume_cache
@INPUT      : cache
              volume
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Deletes the volume cache.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  delete_volume_cache(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume )
{
    int   dim, n_dims;

    delete_cache_blocks( cache, volume, TRUE );

    FREE( cache->hash_table );
    cache->hash_table = NULL;

    n_dims = cache->n_dimensions;
    for_less( dim, 0, n_dims )
    {
        FREE( cache->lookup[dim] );
    }

    delete_string( cache->input_filename );
    delete_string( cache->output_filename );
    delete_string( cache->original_filename );
    delete_string( cache->history );

    delete_minc_output_options( &cache->options );

    /*--- close the file that cache was reading from or writing to */

    if( cache->minc_file != NULL )
    {
      if( cache->output_file_is_open )
      {
#ifdef HAVE_MINC1
            (void) close_minc_output( (Minc_file) cache->minc_file );
#elif defined HAVE_MINC2
            (void) close_minc2_output( (Minc_file) cache->minc_file );
#endif
      } else
#ifdef HAVE_MINC1
            (void) close_minc_input( (Minc_file) cache->minc_file );
#elif defined HAVE_MINC2
            (void) close_minc2_input( (Minc_file) cache->minc_file );
#endif
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_volume_cache_block_sizes
@INPUT      : volume
              block_sizes
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Changes the sizes of the cache blocks for the volume,
              if it is a cached volume.  This flushes the cache blocks,
              since they have changed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 24, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_volume_cache_block_sizes(
    VIO_Volume    volume,
    int       block_sizes[] )
{
    VIO_volume_cache_struct   *cache;
    int                   d, dim, sizes[VIO_N_DIMENSIONS];
    VIO_BOOL               changed;

    if( !volume->is_cached_volume )
        return;

    cache = &volume->cache;

    get_volume_sizes( volume, sizes );

    changed = FALSE;

    for_less( d, 0, get_volume_n_dimensions(volume) )
    {
        if( block_sizes[d] < 1 || block_sizes[d] > sizes[d] )
            block_sizes[d] = sizes[d];

        if( cache->block_sizes[d] != block_sizes[d] )
            changed = TRUE;
    }

    /*--- if the block sizes have not changed, do nothing */

    if( !changed )
        return;

    delete_cache_blocks( cache, volume, FALSE );

    FREE( cache->hash_table );

    for_less( dim, 0, get_volume_n_dimensions( volume ) )
    {
        FREE( cache->lookup[dim] );
    }

    for_less( d, 0, get_volume_n_dimensions(volume) )
        cache->block_sizes[d] = block_sizes[d];

    alloc_volume_cache( cache, volume );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_volume_cache_size
@INPUT      : volume
              max_memory_bytes
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Changes the maximum amount of memory in the cache for this
              volume, if it is a cached volume.  This flushes the cache,
              in order to reallocate the hash table to a new size.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Oct. 24, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_volume_cache_size(
    VIO_Volume    volume,
    int       max_memory_bytes )
{
    int                   dim;
    VIO_volume_cache_struct   *cache;

    if( !volume->is_cached_volume )
        return;

    cache = &volume->cache;

    delete_cache_blocks( cache, volume, FALSE );

    FREE( cache->hash_table );

    for_less( dim, 0, get_volume_n_dimensions( volume ) )
    {
        FREE( cache->lookup[dim] );
    }

    cache->max_cache_bytes = max_memory_bytes;

    alloc_volume_cache( cache, volume );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_cache_output_volume_parameters
@INPUT      : volume
              filename
              file_nc_data_type
              file_signed_flag
              file_voxel_min
              file_voxel_max
              original_filename  - if non-NULL copies auxiliary info from this
              history
              options
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Indicates that rather than using a temporary file for the
              cached volume, read and write to this file with the associated
              parameters (similar to output_modified_volume()).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Nov.  4, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_cache_output_volume_parameters(
    VIO_Volume                      volume,
    VIO_STR                      filename,
    nc_type                     file_nc_data_type,
    VIO_BOOL                     file_signed_flag,
    VIO_Real                        file_voxel_min,
    VIO_Real                        file_voxel_max,
    VIO_STR                      original_filename,
    VIO_STR                      history,
    minc_output_options         *options )

{
    volume->cache.output_filename = create_string( filename );
    volume->cache.file_nc_data_type = file_nc_data_type;
    volume->cache.file_signed_flag = file_signed_flag;
    volume->cache.file_voxel_min = file_voxel_min;
    volume->cache.file_voxel_max = file_voxel_max;
    volume->cache.original_filename = create_string( original_filename );
    volume->cache.history = create_string( history );
    copy_minc_output_options( options, &volume->cache.options );
}
    
/* ----------------------------- MNI Header -----------------------------------
@NAME       : open_cache_volume_input_file
@INPUT      : cache
              volume
              filename
              options
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Opens the volume file for reading into the cache as needed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  open_cache_volume_input_file(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume,
    VIO_STR                filename,
    minc_input_options    *options )
{
    cache->input_filename = create_string( filename );

#ifdef HAVE_MINC1
    cache->minc_file = initialize_minc_input( filename, volume, options );
#elif defined  HAVE_MINC2
    cache->minc_file = initialize_minc2_input( filename, volume, options );
#endif     

    cache->must_read_blocks_before_use = TRUE;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : open_cache_volume_output_file
@INPUT      : cache
              volume
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Opens a volume file for reading and writing cache blocks.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  open_cache_volume_output_file(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume )
{
    VIO_Status  status=VIO_ERROR;
    int         dim, n_dims;
    int         out_sizes[VIO_MAX_DIMENSIONS];
    int         vol_sizes[VIO_MAX_DIMENSIONS];
    VIO_Real    min_value, max_value;
    Minc_file   out_minc_file=NULL;
    VIO_STR     *vol_dim_names=NULL;
    VIO_STR     *out_dim_names=NULL, output_filename;

    n_dims = get_volume_n_dimensions( volume );

    /*--- check if the output filename has been set */

    if( string_length( cache->output_filename ) == 0 )
    {
        cache->writing_to_temp_file = TRUE;
        output_filename = get_temporary_filename();

        cache->file_nc_data_type = get_volume_nc_data_type( volume,
                                          &cache->file_signed_flag );
        get_volume_voxel_range( volume, &cache->file_voxel_min,
                                &cache->file_voxel_max );

        ALLOC( out_dim_names, n_dims );

        vol_dim_names = get_volume_dimension_names( volume );
        get_volume_sizes( volume, vol_sizes );

        for_less( dim, 0, n_dims )
        {
            out_dim_names[dim] = create_string( vol_dim_names[dim] );
            out_sizes[dim] = vol_sizes[dim];
        }

        delete_dimension_names( volume, vol_dim_names );
    }
    else
    {
        cache->writing_to_temp_file = FALSE;
        output_filename = create_string( cache->output_filename );

/*#ifdef HAVE_MINC1*/
        out_dim_names = create_output_dim_names( volume,
                                                 cache->original_filename, 
                                                 &cache->options, out_sizes );
/*#elif defined  HAVE_MINC2*/
/*TODO: adopt for MINC2*/	
/*#endif */
        if( out_dim_names == NULL )
            return( VIO_ERROR );
    }

    get_volume_real_range( volume, &min_value, &max_value );

    set_minc_output_real_range( &cache->options, min_value, max_value );

    /*--- open the file for writing */

#ifdef HAVE_MINC1
    out_minc_file = initialize_minc_output( output_filename,
                                        n_dims, out_dim_names, out_sizes,
                                        cache->file_nc_data_type,
                                        cache->file_signed_flag,
                                        cache->file_voxel_min,
                                        cache->file_voxel_max,
                                        get_voxel_to_world_transform(volume),
                                        volume, &cache->options );
#elif defined  HAVE_MINC2
    out_minc_file = initialize_minc2_output( output_filename,
                                        n_dims, out_dim_names, out_sizes,
                                        cache->file_nc_data_type,
                                        cache->file_signed_flag,
                                        cache->file_voxel_min,
                                        cache->file_voxel_max,
                                        get_voxel_to_world_transform(volume),
                                        volume, &cache->options );
#endif 
    if( out_minc_file == NULL )
        return( VIO_ERROR );

    status = copy_volume_auxiliary_and_history( out_minc_file, output_filename,
                                                cache->original_filename,
                                                cache->history );

    if( status != VIO_OK )
        return( status );

    out_minc_file->converting_to_colour = FALSE;

    /*--- make temp file disappear when the volume is deleted */

    if( string_length( cache->output_filename ) == 0 )
        remove_file( output_filename );

#ifdef HAVE_MINC1
    status = set_minc_output_random_order( out_minc_file );
#elif defined  HAVE_MINC2
    status = set_minc2_output_random_order( out_minc_file );
#endif 

    if( status != VIO_OK )
        return( status );

    /*--- if the volume was previously reading a file, copy the volume to
          the output and close the input file */

    if( cache->minc_file != NULL )
    {
#ifdef HAVE_MINC1
        (void) output_minc_volume( out_minc_file );
        (void) close_minc_input( (Minc_file) cache->minc_file );
#elif defined  HAVE_MINC2
        (void) output_minc2_volume( out_minc_file );
        (void) close_minc2_input( (Minc_file) cache->minc_file );
#endif 

        cache->must_read_blocks_before_use = TRUE;
    }

    cache->minc_file = out_minc_file;

    delete_dimension_names( volume, out_dim_names );

    delete_string( output_filename );

    return( VIO_OK );
}

VIOAPI  void  cache_volume_range_has_changed(
    VIO_Volume   volume )
{
    if( !volume->is_cached_volume )
        return;

    if( volume->cache.minc_file == NULL && volume->cache.n_blocks == 0 )
        return;

    /* This message is not useful.
    print( "Not implemented yet in cache_volume_range_has_changed()\n" );
    */
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_cache_volume_file_offset
@INPUT      : cache
              volume
              file_offset
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the offset in the file for writing volumes.  Used when
              writing several cached volumes to a file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_cache_volume_file_offset(
    VIO_volume_cache_struct   *cache,
    VIO_Volume                volume,
    long                  file_offset[] )
{
    VIO_BOOL  changed;
    int      dim;

    changed = FALSE;

    for_less( dim, 0, VIO_MAX_DIMENSIONS )
    {
        if( cache->file_offset[dim] != (int) file_offset[dim] )
            changed = TRUE;

        cache->file_offset[dim] = (int) file_offset[dim];
    }

    if( changed )
        delete_cache_blocks( cache, volume, FALSE );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : read_cache_block
@INPUT      : cache
              volume
              block
              block_start
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Reads one cache block.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  read_cache_block(
    VIO_volume_cache_struct  *cache,
    VIO_Volume               volume,
    VIO_cache_block_struct   *block,
    int                  block_start[] )
{
    Minc_file        minc_file;
    int              dim, ind, n_dims;
    int              sizes[VIO_MAX_DIMENSIONS];
    int              file_start[VIO_MAX_DIMENSIONS];
    int              file_count[VIO_MAX_DIMENSIONS];
    void             *array_data_ptr;

    minc_file = (Minc_file) cache->minc_file;

    get_volume_sizes( volume, sizes );

    for_less( dim, 0, minc_file->n_file_dimensions )
    {
        ind = minc_file->to_volume_index[dim];
        if( ind >= 0 )
        {
            file_start[dim] = cache->file_offset[dim] + block_start[ind];
            file_count[dim] = MIN( sizes[ind] - file_start[dim],
                                   cache->block_sizes[ind] );
        }
        else
        {
            file_start[dim] = cache->file_offset[dim];
            file_count[dim] = 0;
        }
    }

    n_dims = cache->n_dimensions;
    GET_MULTIDIM_PTR( array_data_ptr, block->array, 0, 0, 0, 0, 0 );

#ifdef HAVE_MINC1
    input_minc_hyperslab( (Minc_file) cache->minc_file,
                                 get_multidim_data_type(&block->array),
                                 n_dims, cache->block_sizes, array_data_ptr,
                                 minc_file->to_volume_index,
                                 file_start, file_count );
#elif defined HAVE_MINC2
    /*TODO: call minc2 api ?*/
#endif 
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : appropriate_a_cache_block
@INPUT      : cache
              volume
@OUTPUT     : block
@RETURNS    : 
@DESCRIPTION: Finds an available cache block, either by allocating one, or
              stealing the least recently used one.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_cache_block_struct  *appropriate_a_cache_block(
    VIO_volume_cache_struct  *cache,
    VIO_Volume               volume )
{
    VIO_cache_block_struct  *block;

    /*--- if can allocate more blocks, do so */

    if( cache->n_blocks < cache->max_blocks )
    {
        ALLOC( block, 1 );

        create_multidim_array( &block->array, 1, &cache->total_block_size,
                               get_volume_data_type(volume) );

        ++cache->n_blocks;
    }
    else  /*--- otherwise, steal the least-recently used block */
    {
        block = cache->tail;

        if( block->modified_flag )
            write_cache_block( cache, volume, block );

        /*--- remove from used list */

        if( block->prev_used == NULL )
            cache->head = block->next_used;
        else
            block->prev_used->next_used = block->next_used;

        if( block->next_used == NULL )
            cache->tail = block->prev_used;
        else
            block->next_used->prev_used = block->prev_used;

        /*--- remove from hash table */

        *block->prev_hash = block->next_hash;
        if( block->next_hash != NULL )
            block->next_hash->prev_hash = block->prev_hash;
    }

    block->modified_flag = FALSE;

    return( block );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : hash_block_index
@INPUT      : key
              table_size
@OUTPUT     : 
@RETURNS    : hash address
@DESCRIPTION: Hashes a block index key into a table index, using 
              multiplicative hashing.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  int  hash_block_index(
    int  key,
    int  table_size )
{
    int    index;
    VIO_Real   v;

    v = (VIO_Real) key * HASH_FUNCTION_CONSTANT;
    
    index = (int) (( v - (VIO_Real) ((int) v)) * (VIO_Real) table_size);

    return( index );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_cache_block_for_voxel
@INPUT      : volume
              x
              y
              z
              t
              v
@OUTPUT     : offset
@RETURNS    : pointer to cache block
@DESCRIPTION: Finds the cache block corresponding to a given voxel, and
              modifies the voxel indices to be block indices.  This function
              gets called for every set or get voxel value, so it must be
              efficient.  On return, offset contains the integer offset
              of the voxel within the cache block.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_cache_block_struct  *get_cache_block_for_voxel(
    VIO_Volume   volume,
    int      x,
    int      y,
    int      z,
    int      t,
    int      v,
    int      *offset )
{
    VIO_cache_block_struct   *block;
    VIO_cache_lookup_struct  *lookup0, *lookup1, *lookup2, *lookup3, *lookup4;
    int                  block_index;
    int                  block_start[VIO_MAX_DIMENSIONS];
    int                  n_dims, hash_index;
    VIO_volume_cache_struct  *cache;

    cache = &volume->cache;
    n_dims = cache->n_dimensions;

    switch( n_dims )
    {
    case 1:
        lookup0 = &cache->lookup[0][x];
        block_index = lookup0->block_index_offset;
        *offset = lookup0->block_offset;
        break;

    case 2:
        lookup0 = &cache->lookup[0][x];
        lookup1 = &cache->lookup[1][y];
        block_index = lookup0->block_index_offset +
                      lookup1->block_index_offset;
        *offset = lookup0->block_offset +
                  lookup1->block_offset;
        break;

    case 3:
        lookup0 = &cache->lookup[0][x];
        lookup1 = &cache->lookup[1][y];
        lookup2 = &cache->lookup[2][z];
        block_index = lookup0->block_index_offset +
                      lookup1->block_index_offset +
                      lookup2->block_index_offset;
        *offset = lookup0->block_offset +
                  lookup1->block_offset +
                  lookup2->block_offset;
        break;

    case 4:
        lookup0 = &cache->lookup[0][x];
        lookup1 = &cache->lookup[1][y];
        lookup2 = &cache->lookup[2][z];
        lookup3 = &cache->lookup[3][t];
        block_index = lookup0->block_index_offset +
                      lookup1->block_index_offset +
                      lookup2->block_index_offset +
                      lookup3->block_index_offset;
        *offset = lookup0->block_offset +
                  lookup1->block_offset +
                  lookup2->block_offset +
                  lookup3->block_offset;
        break;

    case 5:
        lookup0 = &cache->lookup[0][x];
        lookup1 = &cache->lookup[1][y];
        lookup2 = &cache->lookup[2][z];
        lookup3 = &cache->lookup[3][t];
        lookup4 = &cache->lookup[4][v];
        block_index = lookup0->block_index_offset +
                      lookup1->block_index_offset +
                      lookup2->block_index_offset +
                      lookup3->block_index_offset +
                      lookup4->block_index_offset;
        *offset = lookup0->block_offset +
                  lookup1->block_offset +
                  lookup2->block_offset +
                  lookup3->block_offset +
                  lookup4->block_offset;
        break;
    }

    /*--- if this is the same as the last access, just return the last
          block accessed */

    if( block_index == cache->previous_block_index )
    {
#ifdef  CACHE_DEBUGGING
        record_cache_prev_hit( cache );
#endif
        return( cache->previous_block );
    }

    /*--- search the hash table for the block index */

    hash_index = hash_block_index( block_index, cache->hash_table_size );

    block = cache->hash_table[hash_index];

    while( block != NULL && block->block_index != block_index )
    {
        block = block->next_hash;
    }

    /*--- check if it was found in the hash table */

    if( block == NULL )
    {
#ifdef  CACHE_DEBUGGING
        record_cache_no_hit( cache );
#endif

        /*--- find a block to use */

        block = appropriate_a_cache_block( cache, volume );
        block->block_index = block_index;

        /*--- check if the block must be initialized from a file */

        if( cache->must_read_blocks_before_use )
        {
            get_block_start( cache, block_index, block_start );
            read_cache_block( cache, volume, block, block_start );
        }

        /*--- insert the block in cache hash table */

        block->next_hash = cache->hash_table[hash_index];
        if( block->next_hash != NULL )
            block->next_hash->prev_hash = &block->next_hash;
        block->prev_hash = &cache->hash_table[hash_index];
        *block->prev_hash = block;

        /*--- insert the block at the head of the used list */

        block->prev_used = NULL;
        block->next_used = cache->head;

        if( cache->head == NULL )
            cache->tail = block;
        else
            cache->head->prev_used = block;

        cache->head = block;
    }
    else   /*--- block was found in hash table */
    {
#ifdef  CACHE_DEBUGGING
        record_cache_hit( cache );
#endif

        /*--- move block to head of used list */

        if( block != cache->head )
        {
            block->prev_used->next_used = block->next_used;
            if( block->next_used != NULL )
                block->next_used->prev_used = block->prev_used;
            else
                cache->tail = block->prev_used;

            cache->head->prev_used = block;
            block->prev_used = NULL;
            block->next_used = cache->head;
            cache->head = block;
        }

        /*--- move block to beginning of hash chain, so if next access to
              this block, we will save some time */

        if( cache->hash_table[hash_index] != block )
        {
            /*--- remove it from where it is */

            *block->prev_hash = block->next_hash;
            if( block->next_hash != NULL )
                block->next_hash->prev_hash = block->prev_hash;

            /*--- place it at the front of the list */
                
            block->next_hash = cache->hash_table[hash_index];
            if( block->next_hash != NULL )
                block->next_hash->prev_hash = &block->next_hash;
            block->prev_hash = &cache->hash_table[hash_index];
            *block->prev_hash = block;
        }
    }

    /*--- record so if next access is to same block, we save some time */

    cache->previous_block = block;
    cache->previous_block_index = block_index;

    return( cache->previous_block );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_cached_volume_voxel
@INPUT      : volume
              x
              y
              z
              t
              v
@OUTPUT     : 
@RETURNS    : voxel value
@DESCRIPTION: Finds the voxel value for the given voxel in a cached volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Real  get_cached_volume_voxel(
    VIO_Volume   volume,
    int      x,
    int      y,
    int      z,
    int      t,
    int      v )
{
    int                  offset;
    VIO_Real                 value;
    VIO_cache_block_struct   *block;

    if( volume->cache.minc_file == NULL )
        return( get_volume_voxel_min( volume ) );

    block = get_cache_block_for_voxel( volume, x, y, z, t, v, &offset );

    GET_MULTIDIM_1D( value, (VIO_Real), block->array, offset );

    return( value );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_cached_volume_voxel
@INPUT      : volume
              x
              y
              z
              t
              v
              value
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the voxel value for the given voxel in a cached volume.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_cached_volume_voxel(
    VIO_Volume   volume,
    int      x,
    int      y,
    int      z,
    int      t,
    int      v,
    VIO_Real     value )
{
    int                  offset;
    VIO_cache_block_struct   *block;

    if( !volume->cache.output_file_is_open )
    {
        (void) open_cache_volume_output_file( &volume->cache, volume );
        volume->cache.output_file_is_open = TRUE;
    }

    block = get_cache_block_for_voxel( volume, x, y, z, t, v, &offset );

    block->modified_flag = TRUE;

    SET_MULTIDIM_1D( block->array, offset, value );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : cached_volume_has_been_modified
@INPUT      : cache
@OUTPUT     : 
@RETURNS    : TRUE if the volume has been modified since creation
@DESCRIPTION: Determines if the volume has been modified.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Sep. 1, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_BOOL  cached_volume_has_been_modified(
    VIO_volume_cache_struct  *cache )
{
    return( cache->minc_file != NULL );
}

VIOAPI  VIO_BOOL  volume_is_cached(
    VIO_Volume  volume )
{
    return( volume->is_cached_volume );
}

#ifndef  CACHE_DEBUGGING
/* ARGSUSED */
#endif

VIOAPI  void   set_volume_cache_debugging(
    VIO_Volume   volume,
    int      output_every )
{
#ifdef  CACHE_DEBUGGING
    if( output_every >= 1 )
    {
        volume->cache.debugging_on = TRUE;
        volume->cache.output_every = output_every;
    }
    else
    {
        volume->cache.debugging_on = FALSE;
    }
#endif
}

#ifdef  CACHE_DEBUGGING

static  void  initialize_cache_debug(
    VIO_volume_cache_struct  *cache )
{
    int      output_every;
    VIO_STR   debug;

    debug = getenv( "VOLUME_CACHE_DEBUG" );

    cache->debugging_on = (debug != NULL);

    if( debug == NULL || sscanf( debug, "%d", &output_every ) != 1 ||
        output_every < 1 )
    {
        output_every = 1000;
    }

    cache->output_every = output_every;
    cache->n_accesses = 0;
    cache->n_hits = 0;
    cache->n_prev_hits = 0;
}

static  void  increment_n_accesses(
    VIO_volume_cache_struct  *cache )
{
    ++cache->n_accesses;

    if( cache->n_accesses >= cache->output_every )
    {
        print( "VIO_Volume cache:  Hit ratio: %g   Prev ratio: %g\n",
               (VIO_Real) (cache->n_hits + cache->n_prev_hits) /
               (VIO_Real) cache->n_accesses,
               (VIO_Real) cache->n_prev_hits /
               (VIO_Real) cache->n_accesses );

        cache->n_accesses = 0;
        cache->n_hits = 0;
        cache->n_prev_hits = 0;
    }
}

static  void  record_cache_hit(
    VIO_volume_cache_struct  *cache )
{
    if( cache->debugging_on )
    {
        ++cache->n_hits;
        increment_n_accesses( cache );
    }
}

static  void  record_cache_prev_hit(
    VIO_volume_cache_struct  *cache )
{
    if( cache->debugging_on )
    {
        ++cache->n_prev_hits;
        increment_n_accesses( cache );
    }
}

static  void  record_cache_no_hit(
    VIO_volume_cache_struct  *cache )
{
    if( cache->debugging_on )
    {
        increment_n_accesses( cache );
    }
}

#endif
