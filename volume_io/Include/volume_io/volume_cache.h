#ifndef VOL_IO_VOLUME_CACHE_H
#define VOL_IO_VOLUME_CACHE_H

#include "volume.h"

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/volume_cache.h,v 1.11 2005-05-19 21:19:28 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : volume_cache.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Volume block caching mechanism for treating large volumes
              as if they are in memory.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 14, 1995   David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#include  <volume_io/multidim.h>

typedef  enum  { SLICE_ACCESS, RANDOM_VOLUME_ACCESS }
               VIO_Cache_block_size_hints;

#define  CACHE_DEBUGGING
#undef   CACHE_DEBUGGING

typedef  struct  VIO_cache_block_struct
{
    int                         block_index;
    VIO_SCHAR                modified_flag;
    VIO_multidim_array              array;
    struct  VIO_cache_block_struct  *prev_used;
    struct  VIO_cache_block_struct  *next_used;
    struct  VIO_cache_block_struct  **prev_hash;
    struct  VIO_cache_block_struct  *next_hash;
} VIO_cache_block_struct;

typedef  struct
{
    int       block_index_offset;
    int       block_offset;
} VIO_cache_lookup_struct;

typedef struct
{
    int                         n_dimensions;
    int                         file_offset[VIO_MAX_DIMENSIONS];
    VIO_STR                     input_filename;

    VIO_STR                     output_filename;
    nc_type                     file_nc_data_type;
    VIO_BOOL                    file_signed_flag;
    VIO_Real                    file_voxel_min;
    VIO_Real                    file_voxel_max;
    VIO_STR                     original_filename;
    VIO_STR                     history;
    minc_output_options         options;

    VIO_BOOL                    writing_to_temp_file;
    int                         total_block_size;
    int                         block_sizes[VIO_MAX_DIMENSIONS];
    int                         blocks_per_dim[VIO_MAX_DIMENSIONS];
    VIO_BOOL                    output_file_is_open;
    VIO_BOOL                    must_read_blocks_before_use;
    void                        *minc_file;
    int                         n_blocks;
    int                         max_cache_bytes;
    int                         max_blocks;
    int                         hash_table_size;
    VIO_cache_block_struct      *head;
    VIO_cache_block_struct      *tail;
    VIO_cache_block_struct      **hash_table;

    VIO_cache_lookup_struct     *lookup[VIO_MAX_DIMENSIONS];
    VIO_cache_block_struct      *previous_block;
    int                         previous_block_index;

    VIO_BOOL                    debugging_on;
    int                         n_accesses;
    int                         output_every;
    int                         n_hits;
    int                         n_prev_hits;
} VIO_volume_cache_struct;

#endif /* VOL_IO_VOLUME_CACHE_H */
