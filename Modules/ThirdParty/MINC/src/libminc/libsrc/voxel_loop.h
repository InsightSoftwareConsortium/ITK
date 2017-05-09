/* ----------------------------- MNI Header -----------------------------------
@NAME       : voxel_loop.h
@DESCRIPTION: Header file for voxel_loop.c
@GLOBALS    : 
@CREATED    : January 10, 1994 (Peter Neelin)
@MODIFIED   : 
 * $Log: voxel_loop.h,v $
 * Revision 6.4  2005-08-26 21:04:58  bert
 * Use #if rather than #ifdef with MINC2 symbol
 *
 * Revision 6.3  2004/10/15 13:46:52  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.2  2004/04/27 15:43:04  bert
 * Added set_loop_v2format()
 *
 * Revision 6.1  2002/01/14 21:28:26  neelin
 * Moved nd_loop, voxel_loop, ParseArgv and time_stamp from ../progs/Proglib
 * in order to include them in the main minc library.
 *
 * Revision 6.3  2001/11/28 18:39:17  neelin
 * Added get_info_vxoel_index to allow users to get the full multi-dimensional
 * file index of the current voxel.
 * Modifications to allow arg_string to be NULL.
 *
 * Revision 6.2  2000/09/19 14:36:05  neelin
 * Added ability for caller to specify functions for allocating and freeing
 * voxel buffers used in loop. This is particularly useful for embedding
 * the voxel_loop code in other programs, such as Python, which manage memory
 * in their own way.
 *
 * Revision 6.1  1999/10/19 14:45:16  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:23:41  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:24:41  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:00:50  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:31:35  neelin
 * Release of minc version 0.3
 *
 * Revision 1.3  1995/03/21  15:33:07  neelin
 * Changed call to voxel_function to always use proper vector length and
 * set num_voxels to the number of voxels, not multiplying by vector length.
 *
 * Revision 1.2  1995/03/21  14:06:39  neelin
 * Improved interface and added lots of functionality (much for the benefit
 * of mincconcat).
 *
 * Revision 1.1  94/12/14  10:18:21  neelin
 * Initial revision
 * 
 * Revision 2.0  94/09/28  10:36:28  neelin
 * Release of minc version 0.2
 * 
 * Revision 1.3  94/09/28  10:36:22  neelin
 * Pre-release
 * 
 * Revision 1.2  94/01/12  10:19:19  neelin
 * Added logging. Turned off filling. Added miclose for files.
 * 
 * Revision 1.1  94/01/11  15:16:09  neelin
 * Initial revision
 * 
@COPYRIGHT  :
              Copyright 1993 Peter Neelin, McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */

/* Includes */
#include "minc.h"
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Incomplete structure typedefs */

typedef struct Loop_Info Loop_Info;
typedef struct Loop_Options Loop_Options;

/* User function typedefs */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VoxelFunction
@INPUT      : caller_data - pointer to client data.
              num_voxels - number of voxels to process. Note that the
                 total number of input values is 
                 num_voxels * input_vector_length.
              num_input_buffers - number of input buffers to handle
                 on this call - either total number of input files or 1 
                 (for accumulating over files).
              input_vector_length - length of input vector.
              input_data - array of pointers to input buffers (1 for
                 each input file, unless we are accumulating).
              num_output_buffers - number of output buffers to handle
                 on this call - will be the total number of output files
                 unless we are accumulating over files (see 
                 set_loop_accumulate).
              output_vector_length - length of output vector 
                 == input_vector_length or as set by 
                 set_loop_output_vector_size.
              loop_info - pointer that can be passed to functions returning
                 looping information
@OUTPUT     : output_data - array of pointers to output buffers. Set values 
                 to -DBL_MAX to represent illegal, out-of-range values. If 
                 extra buffers are requested by set_loop_accumulate, they will
                 follow the output buffers. 
@RETURNS    : (nothing)
@DESCRIPTION: Typedef for function called by voxel_loop to process data.
---------------------------------------------------------------------------- */
typedef void (*VoxelFunction) 
     (void *caller_data, long num_voxels, 
      int num_input_buffers, int input_vector_length, double *input_data[],
      int num_output_buffers, int output_vector_length, double *output_data[],
      Loop_Info *loop_info);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VoxelInputFileFunction
@INPUT      : caller_data - pointer to client data.
              input_mincid - mincid for current input file
              input_curfile - current input file number (count from zero)
              loop_info - pointer that can be passed to functions returning
                 looping information
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Typedef for function called by voxel_loop to get information
              for each input file.
---------------------------------------------------------------------------- */
typedef void (*VoxelInputFileFunction) 
     (void *caller_data, int input_mincid, int input_curfile,
      Loop_Info *loop_info);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VoxelOutputFileFunction
@INPUT      : caller_data - pointer to client data.
              output_mincid - mincid for current output file
              output_curfile - current output file number (count from zero)
              loop_info - pointer that can be passed to functions returning
                 looping information
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Typedef for function called by voxel_loop to modify the header
              of an output file (in define mode)
---------------------------------------------------------------------------- */
typedef void (*VoxelOutputFileFunction) 
     (void *caller_data, int output_mincid, int output_curfile,
      Loop_Info *loop_info);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VoxelStartFunction
@INPUT      : caller_data - pointer to client data.
              num_voxels - number of voxels to process.
              output_num_buffers - number of output buffers to handle
                 on this call - will be the total number of output files
                 plus the number of extra buffers requested by 
                 set_loop_accumulate.
              output_vector_length - length of output vector = 1 or as set
                 by set_loop_output_vector_size.
              loop_info - pointer that can be passed to functions returning
                 looping information
@OUTPUT     : output_data - array of pointers to output buffers. If 
                 extra buffers are requested by set_loop_accumulate, they will
                 follow the output buffers.
@RETURNS    : (nothing)
@DESCRIPTION: Typedef for function called by voxel_loop to initialize data
              processing when accumulating over files (specified when calling
              set_loop_accumulate).
---------------------------------------------------------------------------- */
typedef void (*VoxelStartFunction) 
     (void *caller_data, long num_voxels,
      int output_num_buffers, int output_vector_length, double *output_data[],
      Loop_Info *loop_info);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VoxelFinishFunction
@INPUT      : caller_data - pointer to client data.
              num_voxels - number of voxels to process.
              output_num_buffers - number of output buffers to handle
                 on this call - will be the total number of output files
                 plus the number of extra buffers requested by 
                 set_loop_accumulate.
              output_vector_length - length of output vector = 1 or as set
                 by set_loop_output_vector_size.
              loop_info - pointer that can be passed to functions returning
                 looping information
@OUTPUT     : output_data - array of pointers to output buffers. If 
                 extra buffers are requested by set_loop_accumulate, they will
                 follow the output buffers.
@RETURNS    : (nothing)
@DESCRIPTION: Typedef for function called by voxel_loop to finish data
              processing when accumulating over files (specified when calling
              set_loop_accumulate).
---------------------------------------------------------------------------- */
typedef void (*VoxelFinishFunction) 
     (void *caller_data, long num_voxels,
      int output_num_buffers, int output_vector_length, double *output_data[],
      Loop_Info *loop_info);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : AllocateBufferFunction
@INPUT      : caller_data - pointer to client data.
              do_allocations - if TRUE, allocate data, if FALSE, free data
              num_input_buffers - number of input buffers to allocate
              num_input_voxels - number of voxels in each input buffer
              input_vector_length - number of values per input voxel
              num_output_buffers - number of output buffers to allocate
              num_output_voxels - number of voxels in each output buffer
              output_vector_length - number of values per output voxel
              num_extra_buffers - number of working buffers to allocate
              num_extra_voxels - number of voxels in each extra buffer
              extra_vector_length - number of values per extra voxel
              loop_info - pointer that can be passed to functions returning
                 looping information
              input_buffers, output_buffers, extra_buffers - if do_allocation 
                 is FALSE, then arrays of pointers to buffers that need
                 to be freed. The pointer arrays should also be freed.
@OUTPUT     : input_buffers, output_buffers, extra_buffers - if do_allocation 
                 is TRUE, then these arrays of buffers should be allocated
                 (both the array of pointers and the buffers). The pointer 
                 array should have length num_xxx_buffers and each buffer
                 should have length num_xxx_voxels*xxx_vector_length and be
                 of type double.
@RETURNS    : (nothing)
@DESCRIPTION: Typedef for function called by voxel_loop to allocate and
              free buffers.
---------------------------------------------------------------------------- */
typedef void (*AllocateBufferFunction) 
     (void *caller_data, int do_allocation,
      int num_input_buffers, int num_input_voxels, int input_vector_length, 
      double ***input_buffers,
      int num_output_buffers, int num_output_voxels, int output_vector_length, 
      double ***output_buffers,
      int num_extra_buffers, int num_extra_voxels, int extra_vector_length, 
      double ***extra_buffers,
      Loop_Info *loop_info);

/* Function declarations */
MNCAPI int voxel_loop(int num_input_files, char *input_files[], 
                      int num_output_files, char *output_files[], 
                      char *arg_string, 
                      Loop_Options *loop_options,
                      VoxelFunction voxel_function, void *caller_data);
MNCAPI Loop_Options *create_loop_options(void);
MNCAPI void free_loop_options(Loop_Options *loop_options);
MNCAPI void set_loop_clobber(Loop_Options *loop_options, 
                             int clobber);
#if MINC2
MNCAPI void set_loop_v2format(Loop_Options *loop_options,
			      int use_v2_format);
#endif /* MINC2 */
MNCAPI void set_loop_verbose(Loop_Options *loop_options, 
                             int verbose);
MNCAPI void set_loop_datatype(Loop_Options *loop_options, 
                              nc_type datatype, int is_signed,
                              double valid_min, double valid_max);
MNCAPI void set_loop_max_open_files(Loop_Options *loop_options, 
                                    int max_open_files);
MNCAPI void set_loop_check_dim_info(Loop_Options *loop_options, 
                                    int check_dim_info);
MNCAPI void set_loop_convert_input_to_scalar(Loop_Options *loop_options, 
                                             int convert_input_to_scalar);
MNCAPI void set_loop_output_vector_size(Loop_Options *loop_options, 
                                        int output_vector_size);
MNCAPI void set_loop_first_input_mincid(Loop_Options *loop_options, 
                                        int input_mincid);
MNCAPI void set_loop_buffer_size(Loop_Options *loop_options,
                                 long buffer_size);
MNCAPI void set_loop_dimension(Loop_Options *loop_options,
                               char *dimension_name);
MNCAPI void set_loop_input_file_function
   (Loop_Options *loop_options,
    VoxelInputFileFunction input_file_function);
MNCAPI void set_loop_output_file_function
   (Loop_Options *loop_options,
    VoxelOutputFileFunction output_file_function);
MNCAPI void set_loop_copy_all_header(Loop_Options *loop_options, 
                                     int copy_all_header);
MNCAPI void set_loop_accumulate(Loop_Options *loop_options, 
                                int do_accumulation,
                                int num_extra_buffers,
                                VoxelStartFunction start_function,
                                VoxelFinishFunction finish_function);
MNCAPI void set_loop_allocate_buffer_function(Loop_Options *loop_options, 
                         AllocateBufferFunction allocate_buffer_function);
MNCAPI void set_loop_labels(Loop_Options *loop_options, 
                             int labels);

MNCAPI void get_info_shape(Loop_Info *loop_info, int ndims,
                           long start[], long count[]);
MNCAPI void get_info_voxel_index(Loop_Info *loop_info, long subscript, 
                                 int ndims, long index[]);
MNCAPI int get_info_current_file(Loop_Info *loop_info);
MNCAPI int get_info_current_mincid(Loop_Info *loop_info);
MNCAPI int get_info_current_index(Loop_Info *loop_info);
MNCAPI int get_info_whole_file(Loop_Info *loop_info);
MNCAPI int get_loop_labels(Loop_Options *loop_options);

#ifdef __cplusplus
}
#endif /* __cplusplus */
