/* ----------------------------- MNI Header -----------------------------------
@NAME       : voxel_loop.c
@DESCRIPTION: Routines to loop through a file doing an operation on a single
              voxel.
@METHOD     : 
@GLOBALS    : 
@CREATED    : January 10, 1994 (Peter Neelin)
@MODIFIED   : 
 * $Log: voxel_loop.c,v $
 * Revision 6.14  2010-03-02 23:24:40  rotor
 *  * libsrc/hdf_convenience.c: removed spurious debug output
 *  * libsrc/minc.h: replaced MAX_NC_OPEN with 32
 *  * libsrc/voxel_loop.c: replaced MAX_NC_OPEN with MI_MAX_NUM_ICV
 *
 * Revision 6.13  2010-03-02 12:23:14  rotor
 *  * ported HDF calls to 1.8.x
 *  * Makefile.am: updated for minccmp
 *
 * Revision 6.12  2008/01/17 02:33:02  rotor
 *  * removed all rcsids
 *  * removed a bunch of ^L's that somehow crept in
 *  * removed old (and outdated) BUGS file
 *
 * Revision 6.11  2008/01/13 09:38:54  stever
 * Avoid compiler warnings about functions and variables that are defined
 * but not used.  Remove some such functions and variables,
 * conditionalize some, and move static declarations out of header files
 * into C files.
 *
 * Revision 6.10  2008/01/13 04:30:28  stever
 * Add braces around static initializers.
 *
 * Revision 6.9  2008/01/12 19:08:14  stever
 * Add __attribute__ ((unused)) to all rcsid variables.
 *
 * Revision 6.8  2006/04/09 15:40:25  bert
 * Minor change
 *
 * Revision 6.7  2005/08/26 21:04:58  bert
 * Use #if rather than #ifdef with MINC2 symbol
 *
 * Revision 6.6  2004/11/01 22:23:14  bert
 * Get rid of minc_def.h, use standard MALLOC() macro
 *
 * Revision 6.5  2004/10/15 13:46:52  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.4  2004/04/27 15:43:29  bert
 * Support MINC 2.0 format
 *
 * Revision 6.3  2003/11/14 16:52:24  stever
 * More last-minute fixes.
 *
 * Revision 6.2  2003/09/18 16:49:46  bert
 * Use fabs instead of ABS
 *
 * Revision 6.1  2002/01/14 21:28:26  neelin
 * Moved nd_loop, voxel_loop, ParseArgv and time_stamp from ../progs/Proglib
 * in order to include them in the main minc library.
 *
 * Revision 6.9  2002/01/14 20:02:39  neelin
 * Force the input buffers to have a minimum size so that large images do
 * not force excessive reading of the input file.
 *
 * Revision 6.8  2001/11/28 18:39:16  neelin
 * Added get_info_vxoel_index to allow users to get the full multi-dimensional
 * file index of the current voxel.
 * Modifications to allow arg_string to be NULL.
 *
 * Revision 6.7  2001/09/18 15:32:27  neelin
 * Create image variable last to allow big images and to fix compatibility
 * problems with 2.3 and 3.x.
 *
 * Revision 6.6  2001/08/16 16:41:32  neelin
 * Added library functions to handle reading of datatype, sign and valid range,
 * plus writing of valid range and setting of default ranges. These functions
 * properly handle differences between valid_range type and image type. Such
 * difference can cause valid data to appear as invalid when double to float
 * conversion causes rounding in the wrong direction (out of range).
 * Modified voxel_loop, volume_io and programs to use these functions.
 *
 * Revision 6.5  2001/08/16 13:32:27  neelin
 * Partial fix for valid_range of different type from image (problems
 * arising from double to float conversion/rounding). NOT COMPLETE.
 *
 * Revision 6.4  2001/04/24 13:38:40  neelin
 * Replaced NC_NAT with MI_ORIGINAL_TYPE.
 *
 * Revision 6.3  2001/04/17 18:40:15  neelin
 * Modifications to work with NetCDF 3.x
 * In particular, changed NC_LONG to NC_INT (and corresponding longs to ints).
 * Changed NC_UNSPECIFIED to NC_NAT.
 * A few fixes to the configure script.
 *
 * Revision 6.2  2000/09/19 14:36:05  neelin
 * Added ability for caller to specify functions for allocating and freeing
 * voxel buffers used in loop. This is particularly useful for embedding
 * the voxel_loop code in other programs, such as Python, which manage memory
 * in their own way.
 *
 * Revision 6.1  1999/10/19 14:45:15  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:23:41  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:24:41  neelin
 * Release of minc version 0.5
 *
 * Revision 4.2  1997/06/20  13:58:35  neelin
 * Fixed bug: when doing accumulation with no output file and with
 * 4D input (or more), had problem setting input start vector. This broke
 * mincconcat for 4D input files.
 *
 * Revision 4.1  1997/05/22  12:41:40  neelin
 * Loosened up checking of start coordinates so that we look at error
 * relative to the total extent of the volume (nelements*step) instead of
 * relative to start value (which may be close to zero).
 *
 * Revision 4.0  1997/05/07  20:00:50  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:31:35  neelin
 * Release of minc version 0.3
 *
 * Revision 1.6  1995/05/11  12:31:29  neelin
 * Removed error messages from ncattdel.
 *
 * Revision 1.5  1995/05/02  16:05:32  neelin
 * Fixed bug in handling more than 30 files (needed to detach from icv).
 * Added more checking of dimensions.
 * Fixed bug in allocation of space for global max/min.
 *
 * Revision 1.4  1995/05/01  20:04:50  neelin
 * Fixed memory leak - not freeing global_minimum/maximum.
 *
 * Revision 1.3  1995/03/21  15:33:07  neelin
 * Changed call to voxel_function to always use proper vector length and
 * set num_voxels to the number of voxels, not multiplying by vector length.
 *
 * Revision 1.2  1995/03/21  14:06:39  neelin
 * Improved interface and added lots of functionality (much for the benefit
 * of mincconcat).
 *
 * Revision 1.1  94/12/14  10:17:19  neelin
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

#include "minc_private.h"
#include <float.h>
#include <math.h>
#include "voxel_loop.h"
#include "nd_loop.h"

/* Minimum number of voxels to put in a buffer. If this is too small,
   then for large images excessive reading can result. If it is
   too large, then for large images too much memory will be used. */
#define MIN_VOXELS_IN_BUFFER 1024

/* Default ncopts values for error handling */
#define NC_OPTS_VAL NC_VERBOSE | NC_FATAL

/* Epsilon for coordinate comparisons */
#define COORD_EPSILON (FLT_EPSILON * 10.0)

/* Typedefs */
typedef struct Loopfile_Info Loopfile_Info;

/* Structure definitions */
struct Loop_Info {
   int current_file;
   int current_index;
   long start[MAX_VAR_DIMS];
   long count[MAX_VAR_DIMS];
   long dimvoxels[MAX_VAR_DIMS];   /* Number of voxels skipped by a step
                                      of one in each dimension */
   Loopfile_Info *loopfile_info;
};

struct Loop_Options {
   int verbose;
   int clobber;
   nc_type datatype;
   int is_signed;
   double valid_range[2];
   int max_open_files;
   int check_all_input_dim_info;
   int convert_input_to_scalar;
   int output_vector_size;        /* 0 = same as input size */
   int input_mincid;
   long total_copy_space;
   char *loop_dimension;
   int num_all_inputs;
   VoxelInputFileFunction input_file_function;
   VoxelOutputFileFunction output_file_function;
   int copy_all_header_info;
   int do_accumulate;
   int num_extra_buffers;
   VoxelStartFunction start_function;
   VoxelFinishFunction finish_function;
   VoxelFunction voxel_function;
   void *caller_data;
   Loop_Info *loop_info;
   int is_floating_type;
   int is_labels;
   AllocateBufferFunction allocate_buffer_function;
#if MINC2
   int v2format;
#endif /* MINC2 */
};

struct Loopfile_Info {
   int cflags;			/* creation flags */
   int num_input_files;
   int num_output_files;
   char **input_files;
   char **output_files;
   int input_all_open;
   int output_all_open;
   int *input_mincid;
   int *output_mincid;
   int *input_icvid;
   int *output_icvid;
   int current_input_file_number;
   int current_output_file_number;
   int headers_only;
   int want_headers_only;
   int sequential_access;
   int can_open_all_input;
};

/* Function prototypes */
PRIVATE int get_loop_dim_size(int inmincid, Loop_Options *loop_options);
PRIVATE void translate_input_coords(int inmincid,
                                    long chunk_cur[], long input_cur[],
                                    long chunk_curcount[], 
                                    long input_curcount[],
                                    int *loop_dim_index,
                                    Loop_Options *loop_options);
PRIVATE void check_input_files(Loop_Options *loop_options,
                               Loopfile_Info *loopfile_info);
PRIVATE int input_image_varinq(int mincid, int imgid, char *name,
                               nc_type *datatype, int *ndims, int dim[],
                               int *natts, Loop_Options *loop_options);
PRIVATE void get_dim_info(int mincid, int *ndims, long size[], 
                          char dimname[][MAX_NC_NAME],
                          double start[], double step[],
                          double dircos[][3], int is_regular[],
                          Loop_Options *loop_options);
PRIVATE void setup_output_files(Loop_Options *loop_options, 
                                Loopfile_Info *loopfile_info,
                                char *arg_string);
PRIVATE long get_vector_length(int mincid, Loop_Options *loop_options);
PRIVATE void setup_variables(int inmincid, int outmincid,
                             int output_curfile,
                             char *arg_string, Loop_Options *loop_options);
PRIVATE void update_history(int mincid, char *arg_string);
PRIVATE void setup_icvs(Loop_Options *loop_options, 
                        Loopfile_Info *loopfile_info);
PRIVATE int do_voxel_loop(Loop_Options *loop_options,
                          Loopfile_Info *loopfile_info);
PRIVATE void setup_looping(Loop_Options *loop_options, 
                           Loopfile_Info *loopfile_info,
                           int *ndims,
                           long block_start[], long block_end[], 
                           long block_incr[], long *block_num_voxels,
                           long chunk_incr[], long *chunk_num_voxels);
PRIVATE void initialize_file_and_index(Loop_Options *loop_options, 
                                       Loopfile_Info *loopfile_info,
                                       int do_loop,
                                       int *ifile, int *dim_index,
                                       int *dummy_index);
PRIVATE int finish_file_and_index(Loop_Options *loop_options, 
                                  Loopfile_Info *loopfile_info,
                                  int do_loop,
                                  int ifile, int dim_index,
                                  int dummy_index);
PRIVATE void increment_file_and_index(Loop_Options *loop_options, 
                                      Loopfile_Info *loopfile_info,
                                      int do_loop,
                                      int *ifile, int *dim_index,
                                      int *dummy_index);
PRIVATE Loopfile_Info *initialize_loopfile_info(int num_input_files,
                                                char *input_files[],
                                                int num_output_files,
                                                char *output_files[],
                                                Loop_Options *loop_options);
PRIVATE void cleanup_loopfile_info(Loopfile_Info *loopfile_info);
PRIVATE int get_input_numfiles(Loopfile_Info *loopfile_info);
PRIVATE int get_output_numfiles(Loopfile_Info *loopfile_info);
PRIVATE char *get_input_filename(Loopfile_Info *loopfile_info, int file_num);
PRIVATE void set_input_headers_only(Loopfile_Info *loopfile_info,
                                    int headers_only);
PRIVATE void set_input_sequential(Loopfile_Info *loopfile_info,
                                  int sequential_access);
PRIVATE int get_input_mincid(Loopfile_Info *loopfile_info,
                             int file_num);
PRIVATE int get_output_mincid(Loopfile_Info *loopfile_info,
                              int file_num);
PRIVATE int create_output_file(Loopfile_Info *loopfile_info,
                               int file_num);
PRIVATE int get_input_icvid(Loopfile_Info *loopfile_info,
                            int file_num);
PRIVATE int get_output_icvid(Loopfile_Info *loopfile_info,
                             int file_num);
PRIVATE int create_input_icvid(Loopfile_Info *loopfile_info,
                               int file_num);
PRIVATE int create_output_icvid(Loopfile_Info *loopfile_info,
                                int file_num);
PRIVATE Loop_Info *create_loop_info(void);
PRIVATE void initialize_loop_info(Loop_Info *loop_info);
PRIVATE void free_loop_info(Loop_Info *loop_info);
PRIVATE void set_info_shape(Loop_Info *loop_info, long start[], long count[]);
PRIVATE void set_info_current_file(Loop_Info *loop_info, int current_file);
PRIVATE void set_info_current_index(Loop_Info *loop_info, int current_index);
PRIVATE void set_info_loopfile_info(Loop_Info *loop_info, 
                                    Loopfile_Info *loopfile_info);

/* ----------------------------- MNI Header -----------------------------------
@NAME       : voxel_loop
@INPUT      : num_input_files - number of input files.
              input_files - array of names of input files.
              num_output_files - number of output files.
              output_files - array of names of output files.
              arg_string - string for history.
              loop_options - pointer to structure containing loop options.
              voxel_function - user function to process a group of voxels.
                 See description in header file.
              caller_data - data that will be passed to voxel_function
@OUTPUT     : (none)
@RETURNS    : non-zero if an error occurs.
@DESCRIPTION: Routine to loop through the voxels of a file and call a function
              to operate on each voxel.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 10, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int voxel_loop(int num_input_files, char *input_files[], 
                      int num_output_files, char *output_files[], 
                      char *arg_string, 
                      Loop_Options *loop_options,
                      VoxelFunction voxel_function, void *caller_data)
{
   Loopfile_Info *loopfile_info;
   int need_to_free_loop_options;
   int old_ncopts;
   int status;

   //(void)fprintf(stderr, "About to loop, max_buffer is %d\n", loop_options->total_copy_space);
   
   /* Save ncopts and set it to default value */
   old_ncopts =get_ncopts();
   set_ncopts(NC_OPTS_VAL);

   /* Check that there is at least one input file */
   if (num_input_files < 1) {
      (void) fprintf(stderr, "There must be at least one input file.\n");
      exit(EXIT_FAILURE);
   }
   if (num_output_files < 0) {
      (void) fprintf(stderr, "Negative number of output files!\n");
      exit(EXIT_FAILURE);
   }

   /* Initialize loop options if needed */
   need_to_free_loop_options = FALSE;
   if (loop_options == NULL) {
      loop_options = create_loop_options();
      need_to_free_loop_options = TRUE;
   }
   loop_options->voxel_function = voxel_function;
   loop_options->caller_data = caller_data;

   /* Make sure that Loop_Info structure is initialized */
   initialize_loop_info(loop_options->loop_info);

   /* Initialize looping info */
   loopfile_info = initialize_loopfile_info(num_input_files, input_files,
                                            num_output_files, output_files,
                                            loop_options);

   /* Check that input files match */
   set_input_headers_only(loopfile_info, TRUE);
   check_input_files(loop_options, loopfile_info);
   set_input_headers_only(loopfile_info, FALSE);

   /* Set up variables in output file */
   setup_output_files(loop_options, loopfile_info, arg_string);

   /* Setup icv's */
   setup_icvs(loop_options, loopfile_info);

   /* Loop through the voxels */
   status = do_voxel_loop(loop_options, loopfile_info);

   /* Clean up looping info */
   cleanup_loopfile_info(loopfile_info);

   /* Free loop options if needed */
   if (need_to_free_loop_options) {
      free_loop_options(loop_options);
   }

   /* Restore ncopts */
   set_ncopts(old_ncopts);

   return status;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_loop_dim_size
@INPUT      : inmincid - input minc id
              loop_options - Options for loops
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to get the size of the looping dimension for the given
              input file
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 24, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_loop_dim_size(int inmincid, Loop_Options *loop_options)
{
   int dimid;
   long dim_length;
   int ndims, dim[MAX_VAR_DIMS];
   int found;
   int idim;

   /* Look for dimension */
   dimid = MI_ERROR;
   if (loop_options->loop_dimension != NULL) {
      set_ncopts(0);
      dimid = ncdimid(inmincid, loop_options->loop_dimension);
      set_ncopts(NC_OPTS_VAL);
   }
   if (dimid == MI_ERROR) return 1;
   (void) ncdiminq(inmincid, dimid, NULL, &dim_length);

   /* Get image variable info */
   (void) ncvarinq(inmincid, ncvarid(inmincid, MIimage), NULL, NULL, 
                   &ndims, dim, NULL);

   /* Check to see if the dimension subscripts the image */
   found = FALSE;
   for (idim=0; idim < ndims; idim++) {
      if (dimid == dim[idim]) found = TRUE;
   }

   /* Return appropriate value */
   if (found)
      return dim_length;
   else
      return 1;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : translate_input_coords
@INPUT      : inmincid - input minc id
              chunk_cur - start for current chunk
              chunk_curcount - count for current chunk
              loop_options - Options for loops
@OUTPUT     : input_cur - start for input file
              input_curcount - count for input file
              loop_dim_index - index in input_cur for looping dimension
@RETURNS    : (nothing)
@DESCRIPTION: Routine to translate the input hyperslab coords from those 
              excluding the looping dimension to ones appropriate for the 
              input file. Also returns the index (offset in input_cur) for 
              the looping dimension (so that the caller can modify this 
              index).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 24, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void translate_input_coords(int inmincid,
                                    long chunk_cur[], long input_cur[],
                                    long chunk_curcount[], 
                                    long input_curcount[],
                                    int *loop_dim_index,
                                    Loop_Options *loop_options)
{
   int dimid;
   int ndims, dim[MAX_VAR_DIMS];
   int idim, jdim;

   /* Look for dimension */
   dimid = MI_ERROR;
   if (loop_options->loop_dimension != NULL) {
      set_ncopts(0);
      dimid = ncdimid(inmincid, loop_options->loop_dimension);
      set_ncopts(NC_OPTS_VAL);
   }

   /* Get image variable info */
   (void) ncvarinq(inmincid, ncvarid(inmincid, MIimage), NULL, NULL, 
                   &ndims, dim, NULL);

   /* Copy the hyperslab coordinates and get the index */
   *loop_dim_index = ndims;
   jdim = 0;
   for (idim=0; idim < ndims; idim++) {
      if (dimid != dim[idim]) {
         input_cur[idim] = chunk_cur[jdim];
         input_curcount[idim] = chunk_curcount[jdim];
         jdim++;
      }
      else {
         input_cur[idim] = 0;
         input_curcount[idim] = 1;
         *loop_dim_index = idim;
      }
   }

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : check_input_files
@INPUT      : loop_options - Options for loops
              loopfile_info - Information describing looping stuff and files
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to check input files for consistency.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void check_input_files(Loop_Options *loop_options,
                               Loopfile_Info *loopfile_info)
{
   int ifile, idim, jdim;
   int first_ndims, ndims;
   int input_mincid;
   long first_size[MAX_VAR_DIMS], size[MAX_VAR_DIMS];
   char first_dimname[MAX_VAR_DIMS][MAX_NC_NAME];
   char dimname[MAX_VAR_DIMS][MAX_NC_NAME];
   double extent;
   double first_start[MAX_VAR_DIMS], start[MAX_VAR_DIMS];
   double first_step[MAX_VAR_DIMS], step[MAX_VAR_DIMS];
   double start_diff, step_diff;
   double first_dircos[MAX_VAR_DIMS][3], dircos[MAX_VAR_DIMS][3];
   double dircos_diff, dircos_cumdiff;
   int first_is_regular[MAX_VAR_DIMS], is_regular[MAX_VAR_DIMS];

   /* Keep track of number of inputs (files and looping dimension) */
   loop_options->num_all_inputs = 0;

   /* Loop over files. For the first file, we only get the dimension info, 
      we don't check it. */
   for (ifile = 0; ifile < get_input_numfiles(loopfile_info); ifile++) {

      /* Get mincid */
      input_mincid = get_input_mincid(loopfile_info, ifile);

      /* Add up number of inputs */
      loop_options->num_all_inputs += 
         get_loop_dim_size(input_mincid, loop_options);

      /* Get dimension information for this file */
      if (ifile == 0) {
         get_dim_info(input_mincid, &first_ndims, first_size, first_dimname,
                      first_start, first_step, first_dircos,
                      first_is_regular, loop_options);
      }
      else {
         get_dim_info(input_mincid, &ndims, size, dimname, start, step,
                      dircos, is_regular, loop_options);
         
         /* Check number of dimensions */
         if (ndims != first_ndims) {
            (void) fprintf(stderr, 
               "Files %s and %s have different numbers of dimensions\n",
                           get_input_filename(loopfile_info,ifile),
                           get_input_filename(loopfile_info,0));
            exit(EXIT_FAILURE);
         }

         /* Loop over dimensions */
         for (idim = 0; idim < first_ndims; idim++) {

            /* Check dimension sizes */
            if (size[idim] != first_size[idim]) {
               (void) fprintf(stderr, 
                  "Files %s and %s have different sizes of dimensions\n",
                              get_input_filename(loopfile_info,ifile),
                              get_input_filename(loopfile_info,0));
               exit(EXIT_FAILURE);
            }

            /* Check optional dimension stuff */
            if (loop_options->check_all_input_dim_info) {

               /* Check names */
               if (strcmp(dimname[idim], first_dimname[idim]) != 0) {
                  (void) fprintf(stderr, 
                     "Files %s and %s have different dimension names\n",
                                 get_input_filename(loopfile_info,ifile),
                                 get_input_filename(loopfile_info,0));
                  exit(EXIT_FAILURE);
               }

               /* Check coordinates */
               start_diff = start[idim] - first_start[idim];
               extent = ((double) first_size[idim]) * first_step[idim];
               if (extent != 0.0) start_diff /= extent;
               step_diff = step[idim] - first_step[idim];
               if (first_step[idim] != 0.0) step_diff /= first_step[idim];
               dircos_cumdiff = 0.0;
               for (jdim=0; jdim < 3; jdim++) {
                  dircos_diff = first_dircos[idim][jdim] - dircos[idim][jdim];
                  if (first_dircos[idim][jdim] != 0.0)
                     dircos_diff /= first_dircos[idim][jdim];
                  dircos_cumdiff += fabs(dircos_diff);
               }
               if (fabs(start_diff) > COORD_EPSILON) {
                  (void) fprintf(stderr, 
                     "Files %s and %s have different start coordinates (%s)\n",
                                 get_input_filename(loopfile_info,ifile),
                                 get_input_filename(loopfile_info,0),
                                 first_dimname[idim]);
                  exit(EXIT_FAILURE);
               }
               if (fabs(step_diff) > COORD_EPSILON) {
                  (void) fprintf(stderr, 
                     "Files %s and %s have different step coordinates (%s)\n",
                                 get_input_filename(loopfile_info,ifile),
                                 get_input_filename(loopfile_info,0),
                                 first_dimname[idim]);
                  exit(EXIT_FAILURE);
               }
               if (dircos_cumdiff > COORD_EPSILON) {
                  (void) fprintf(stderr, 
                     "Files %s and %s have different direction cosines (%s)\n",
                                 get_input_filename(loopfile_info,ifile),
                                 get_input_filename(loopfile_info,0),
                                 first_dimname[idim]);
                  exit(EXIT_FAILURE);
               }
               if (first_is_regular[idim] != is_regular[idim]) {
                  (void) fprintf(stderr, 
                     "Files %s and %s have different coordinate spacings (%s)\n",
                                 get_input_filename(loopfile_info,ifile),
                                 get_input_filename(loopfile_info,0),
                                 first_dimname[idim]);
                  exit(EXIT_FAILURE);
               }

            }      /* If check all dimension info */

         }      /* End of loop over dimensions */

      }      /* End of if ifile == 0 else */

      /* Call the user's function if needed */
      if (loop_options->input_file_function != NULL) {
         set_info_current_file(loop_options->loop_info, ifile);
         set_info_loopfile_info(loop_options->loop_info, loopfile_info);
         loop_options->input_file_function(loop_options->caller_data,
                                           input_mincid, ifile,
                                           loop_options->loop_info);
         set_info_loopfile_info(loop_options->loop_info, NULL);
      }

   }      /* End of loop over files */   
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : input_image_varinq
@INPUT      : mincid - Minc id of file
              imgid - id of image variable
              loop_options - structure containing looping options
@OUTPUT     : name - name of variable
              datatype - type of variable
              ndims - number of dimensions
              dim - array of dimension ids
              natts - number of attributes
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Routine to call ncvarinq for the image variable, suppressing
              the dimension specified in loop_options.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int input_image_varinq(int mincid, int imgid, char *name,
                               nc_type *datatype, int *ndims, int dim[],
                               int *natts, Loop_Options *loop_options)
{
   int dimid;
   int old_ncopts;
   int idim, jdim;
   int status;
   int nimgdims;
   char dimname[MAX_NC_NAME];

   /* Get loop dimension id */
   dimid = MI_ERROR;
   if (loop_options->loop_dimension != NULL) {
      old_ncopts =get_ncopts(); set_ncopts(0);
      dimid = ncdimid(mincid, loop_options->loop_dimension);
      set_ncopts(old_ncopts);
   }

   /* Call ncvarinq. If there is no loop dim, or an error occurred, then
      return. */
   status = ncvarinq(mincid, imgid, name, datatype, ndims, dim, natts);
   if ((dimid == MI_ERROR) || (status == MI_ERROR)) 
      return status;

   /* Get number of image dimensions */
   nimgdims = 2;
   if (*ndims > 0) {
      (void) ncdiminq(mincid, dim[*ndims-1], dimname, NULL);
      if (strcmp(dimname, MIvector_dimension) == 0)
         nimgdims++;
   }

   /* Look for the loop dimension */
   jdim = 0;
   for (idim=0; idim < *ndims; idim++) {
      if (dim[idim] != dimid) {
         dim[jdim] = dim[idim];
         jdim++;
      }
      else if (idim >= *ndims-nimgdims) {
         (void) fprintf(stderr, 
                        "Don't use an image dimension as a loop dimension.\n");
         exit(EXIT_FAILURE);
      }
   }

   /* Save number of dimensions */
   *ndims = jdim;

   return status;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_dim_info
@INPUT      : mincid - Minc id of file
@OUTPUT     : ndims - number of dimensions
              size  - array of sizes of dimensions
              dimname - array of dimension names
              start - array of starts for dimensions
              step  - array of steps for dimensions
              dircos - array of direction cosines
              is_regular - array of flags indicating whether dimension is
                 regularly spaced or not
              loop_options - looping options
@RETURNS    : (nothing)
@DESCRIPTION: Routine to get dimension information for a file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void get_dim_info(int mincid, int *ndims, long size[], 
                          char dimname[][MAX_NC_NAME],
                          double start[], double step[],
                          double dircos[][3], int is_regular[],
                          Loop_Options *loop_options)
{
   int imgid, varid;
   int idim, jdim;
   int dim[MAX_VAR_DIMS];
   int att_length;
   char string[MAX_NC_NAME];
   char *thename;
   int old_ncopts;
   enum {XAXIS, YAXIS, ZAXIS, OAXIS} dimtype;
   static double default_dircos[][3] = {
      { 1.0, 0.0, 0.0 },
      { 0.0, 1.0, 0.0 },
      { 0.0, 0.0, 1.0 },
      { 0.0, 0.0, 0.0 }
   };
   char regstring[MI_MAX_ATTSTR_LEN];

   /* Get image variable info */
   imgid = ncvarid(mincid, MIimage);
   (void) input_image_varinq(mincid, imgid, NULL, NULL, ndims, dim, NULL,
                             loop_options);

   /* Loop through dimensions */
   for (idim=0; idim < *ndims; idim++) {
      if (dimname == NULL) thename = string;
      else thename = dimname[idim];
      (void) ncdiminq(mincid, dim[idim], thename, &size[idim]);

      /* Set default coordinate info */
      if (start != NULL) start[idim] = 0.0;
      if (step != NULL) step[idim] = 1.0;
      if (dircos != NULL) {
         if ((strcmp(thename, MIxspace) == 0) ||
             (strcmp(thename, MIxfrequency) == 0))
            dimtype = XAXIS;
         else if ((strcmp(thename, MIyspace) == 0) ||
                  (strcmp(thename, MIyfrequency) == 0))
            dimtype = YAXIS;
         else if ((strcmp(thename, MIzspace) == 0) ||
                  (strcmp(thename, MIzfrequency) == 0))
            dimtype = ZAXIS;
         else
            dimtype = OAXIS;
         for (jdim=0; jdim < 3; jdim++)
            dircos[idim][jdim] = default_dircos[dimtype][jdim];
      }
      if (is_regular != NULL)
         is_regular[idim] = TRUE;

      /* Get the coordinate info */
      old_ncopts =get_ncopts(); set_ncopts(0);
      varid = ncvarid(mincid, thename);
      if (varid != MI_ERROR) {
         if (start != NULL)
            (void) miattget1(mincid, varid, MIstart, NC_DOUBLE, &start[idim]);
         if (step != NULL)
            (void) miattget1(mincid, varid, MIstep, NC_DOUBLE, &step[idim]);
         if (dircos != NULL)
            (void) miattget(mincid, varid, MIdirection_cosines, NC_DOUBLE,
                            3, dircos[idim], &att_length);
         if (is_regular != NULL) {
            if (miattgetstr(mincid, varid, MIspacing, 
                            sizeof(regstring), regstring) != NULL) {
               if (strcmp(regstring, MI_REGULAR) == 0)
                  is_regular[idim] = TRUE;
               else if (strcmp(regstring, MI_IRREGULAR) == 0)
                  is_regular[idim] = FALSE;
            }
         }
      }
      set_ncopts(old_ncopts);
   }
   
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : setup_output_files
@INPUT      : loop_options - Options controlling looping
              loopfile_info - Looping information
              arg_string - string for history
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to setup the the output files
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void setup_output_files(Loop_Options *loop_options, 
                                Loopfile_Info *loopfile_info,
                                char *arg_string)
{
   int inmincid, outmincid;
   int ifile;

   /* Get mincid for first input file */
   inmincid = get_input_mincid(loopfile_info, 0);

   /* Create output files */
   for (ifile=0; ifile < get_output_numfiles(loopfile_info); ifile++) {
      outmincid = create_output_file(loopfile_info, ifile);
      setup_variables(inmincid, outmincid, ifile, arg_string, loop_options);
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_vector_length
@INPUT      : mincid - minc file id
              loop_options - looping options (if NULL, uses ncvarinq)
@OUTPUT     : (none)
@RETURNS    : Length of vector dimension or zero if no such dimension.
@DESCRIPTION: Routine to get the length of the vector dimension in a minc file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE long get_vector_length(int mincid, Loop_Options *loop_options)
{
   int imgid;
   int ndims;
   int dim[MAX_VAR_DIMS];
   char dimname[MAX_NC_NAME];
   long vector_length;

   /* Get image variable id */
   imgid = ncvarid(mincid, MIimage);

   /* Get the image dimension info */
   if (loop_options != NULL) {
      (void) input_image_varinq(mincid, imgid, NULL, NULL, &ndims, dim, NULL, 
                                loop_options);
   }
   else {
      (void) ncvarinq(mincid, imgid, NULL, NULL, &ndims, dim, NULL);
   }

   /* Check for vector dimension */
   (void) ncdiminq(mincid, dim[ndims-1], dimname, &vector_length);
   if ((strcmp(dimname, MIvector_dimension) != 0) || (ndims <= 2)) {
      vector_length = 0;
   }

   return vector_length;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : setup_variables
@INPUT      : inmincid - input minc file id
              outmincid - output minc file id
              output_curfile - current output file number (counting from zero)
              arg_string - string for history
              loop_options - options controlling loop behaviour
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to setup the variables in the output file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 10, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void setup_variables(int inmincid, int outmincid,
                             int output_curfile,
                             char *arg_string, Loop_Options *loop_options)
{
   int inimgid, outimgid, maxid, minid, varid;
   int indim[MAX_VAR_DIMS], outdim[MAX_VAR_DIMS];
   nc_type datatype;
   int idim, odim, ivar, in_ndims, out_ndims, in_nimgdims, out_nimgdims;
   char dimname[MAX_NC_NAME + 1];
   int nvars, varlist[MAX_VAR_DIMS*2];
   long dimlength;
   int input_vector_length;
   int changing_vector_dim;
   double valid_range[2];

   /* Get image variable id for input file */
   inimgid = ncvarid(inmincid, MIimage);

   /* Get the list of dimensions subscripting the image variable */
   (void) input_image_varinq(inmincid, inimgid, NULL, &datatype, 
                             &in_ndims, indim, NULL, loop_options);

   /* Get the length of the input vector dimension */
   input_vector_length = get_vector_length(inmincid, loop_options);
   if (loop_options->convert_input_to_scalar && (input_vector_length > 0)) {
      input_vector_length = 0;
      in_ndims--;
   }

   /* Get the number of image dimensions in the input file */
   in_nimgdims = (input_vector_length == 0 ? 2 : 3);

   /* Are we changing the length of the vector dimension (or removing it?).
      For an output vector size of 1, we don't want an output vector
      dimension. */
   changing_vector_dim = 
      ((loop_options->output_vector_size != 0) &&
       (loop_options->output_vector_size != input_vector_length));
   if (loop_options->output_vector_size == 1)
      changing_vector_dim = (input_vector_length != 0);

   /* Work out number of output dimensions and image dimensions */
   out_ndims = in_ndims;
   out_nimgdims = in_nimgdims;
   if (changing_vector_dim && (input_vector_length == 0)) {
      out_ndims++;
      out_nimgdims++;
   }
   else if (changing_vector_dim && (loop_options->output_vector_size <= 1)) {
      out_ndims--;
      out_nimgdims--;
   }
      
   /* Set up the output minc file */

   /* Loop, creating output dimensions */
   odim = 0;
   nvars = 0;
   for (idim=0; idim < in_ndims; idim++) {

      /* Check for a change in vector dimension length */
      if ((idim != in_ndims-1) || (input_vector_length == 0) || 
          !changing_vector_dim) {

         /* Copy the dimension */
         (void) ncdiminq(inmincid, indim[idim], dimname, &dimlength);
         outdim[odim] = ncdimdef(outmincid, dimname, dimlength);

         /* Copy the dimension variables if we are not copying the 
            whole header */
         if (!loop_options->copy_all_header_info) {
            set_ncopts(0);
            for (ivar=0; ivar < 2; ivar++) {
               if (ivar == 1) 
                  (void) strncat(dimname, "-width", MAX_NC_NAME);
               varlist[nvars] = ncvarid(inmincid, dimname);
               if (varlist[nvars] != MI_ERROR) {
                  (void) micopy_var_def(inmincid, varlist[nvars], outmincid);
                  nvars++;
               }
            }
            set_ncopts(NC_OPTS_VAL);
         }
         
         odim++;
      }
   }

   /* Create the output vector dimension if needed */
   if (changing_vector_dim && (loop_options->output_vector_size > 1)) {
      outdim[odim] = ncdimdef(outmincid, MIvector_dimension, 
                              (long) loop_options->output_vector_size);
   }

   /* Copy other variables in file, if appropriate */
   if (loop_options->copy_all_header_info) {
      nvars = 0;
      set_ncopts(0);
      varlist[nvars] = inimgid;
      if (varlist[nvars] != MI_ERROR) nvars++;
      varlist[nvars] = ncvarid(inmincid, MIimagemax);
      if (varlist[nvars] != MI_ERROR) nvars++;
      varlist[nvars] = ncvarid(inmincid, MIimagemin);
      if (varlist[nvars] != MI_ERROR) nvars++;
      if (loop_options->loop_dimension != NULL) {
         (void) strncpy(dimname, loop_options->loop_dimension, MAX_NC_NAME);
         varlist[nvars] = ncvarid(inmincid, dimname);
         if (varlist[nvars] != MI_ERROR) nvars++;
         (void) strncat(dimname, "-width", MAX_NC_NAME);
         varlist[nvars] = ncvarid(inmincid, dimname);
         if (varlist[nvars] != MI_ERROR) nvars++;
      }
      (void) micopy_all_var_defs(inmincid, outmincid, 
                                 nvars, varlist);
      set_ncopts(NC_OPTS_VAL);
   }

   /* Add the time stamp to the history */
   update_history(outmincid, arg_string);
 
   /* Call the user's function if needed */
   if (loop_options->output_file_function != NULL) {
      set_info_current_file(loop_options->loop_info, 0);
      loop_options->output_file_function(loop_options->caller_data,
                                         outmincid, output_curfile,
                                         loop_options->loop_info);
   }

   /* Create the image-min/max variables */
   if( loop_options->is_labels )
   {
      maxid = micreate_std_variable(outmincid, MIimagemax, NC_DOUBLE, 
                                 0, NULL);
      minid = micreate_std_variable(outmincid, MIimagemin, NC_DOUBLE, 
                                 0, NULL);
   } else {
      maxid = micreate_std_variable(outmincid, MIimagemax, NC_DOUBLE, 
                                 out_ndims-out_nimgdims, outdim);
      minid = micreate_std_variable(outmincid, MIimagemin, NC_DOUBLE, 
                                 out_ndims-out_nimgdims, outdim);
   }
   set_ncopts(0);
   (void) micopy_all_atts(inmincid, ncvarid(inmincid, MIimagemax),
                          outmincid, maxid);
   (void) micopy_all_atts(inmincid, ncvarid(inmincid, MIimagemin),
                          outmincid, minid);
   set_ncopts(NC_OPTS_VAL);

   /* Create the image variable */
   if (loop_options->datatype != MI_ORIGINAL_TYPE) {
      datatype = loop_options->datatype;
   }
   loop_options->is_floating_type = 
      ((datatype == NC_FLOAT) || (datatype == NC_DOUBLE));
   outimgid = micreate_std_variable(outmincid, MIimage, datatype, 
                                    out_ndims, outdim);
   (void) micopy_all_atts(inmincid, inimgid, outmincid, outimgid);
   if (loop_options->is_floating_type) {
      set_ncopts(0);
      (void) ncattdel(outmincid, outimgid, MIsigntype);
      set_ncopts(NC_OPTS_VAL);
      valid_range[0] = 0;
      valid_range[1] = 1;
      (void) miset_valid_range(outmincid, outimgid, valid_range);
   }
   else if (loop_options->datatype != MI_ORIGINAL_TYPE) {
      if (loop_options->is_signed)
         (void) miattputstr(outmincid, outimgid, MIsigntype, MI_SIGNED);
      else
         (void) miattputstr(outmincid, outimgid, MIsigntype, MI_UNSIGNED);
      if ((loop_options->valid_range[1] > loop_options->valid_range[0])) {
         (void) miset_valid_range(outmincid, outimgid, 
                                  loop_options->valid_range);
      }
      else {
         set_ncopts(0);
         (void) ncattdel(outmincid, outimgid, MIvalid_range);  /*VF: why is this needed?*/
         set_ncopts(NC_OPTS_VAL);
      }
   }
   (void) miattputstr(outmincid, outimgid, MIcomplete, MI_FALSE);

   /* Put the file in data mode */
   (void) ncsetfill(outmincid, NC_NOFILL);
   (void) ncendef(outmincid);

   /* Copy over variable values */
   set_ncopts(0);
   if (loop_options->copy_all_header_info) {
      (void) micopy_all_var_values(inmincid, outmincid, nvars, varlist);
   }
   else {
      for (ivar=0; ivar < nvars; ivar++) {
         (void) ncvarinq(inmincid, varlist[ivar], dimname, NULL, NULL,
                         NULL, NULL);
         varid = ncvarid(outmincid, dimname);
         if (varid != MI_ERROR) {
            (void) micopy_var_values(inmincid, varlist[ivar], 
                                     outmincid, varid);
         }
      }
   }
   set_ncopts(NC_OPTS_VAL);
         

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : update_history
@INPUT      : mincid - id of output minc file
              arg_string - string giving list of arguments
@OUTPUT     : (nothing)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to update the history global variable in the output 
              minc file
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : August 26, 1993 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void update_history(int mincid, char *arg_string)
{
   nc_type datatype;
   int att_length;
   char *string;

   if (arg_string == NULL) return;

   /* Get the history attribute length */
   set_ncopts(0);
   if ((ncattinq(mincid, NC_GLOBAL, MIhistory, &datatype,
                 &att_length) == MI_ERROR) ||
       (datatype != NC_CHAR))
      att_length = 0;
   att_length += strlen(arg_string) + 1;

   /* Allocate a string and get the old history */
   string = MALLOC(att_length, char);
   string[0] = '\0';
   (void) miattgetstr(mincid, NC_GLOBAL, MIhistory, att_length, 
                      string);
   set_ncopts(NC_OPTS_VAL);

   /* Add the new command and put the new history. */
   (void) strcat(string, arg_string);
   (void) miattputstr(mincid, NC_GLOBAL, MIhistory, string);
   FREE(string);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : setup_icvs
@INPUT      : loop_options - loop option info
              loopfile_info - looping information
@OUTPUT     : (nothing)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to set up the input and output icv's.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void setup_icvs(Loop_Options *loop_options, 
                        Loopfile_Info *loopfile_info)
{
   int ifile;
   int icvid;

   /* Loop through input icv's, setting their values. Attaching is
      done by get_input_icvid. */
   for (ifile=0; ifile < get_input_numfiles(loopfile_info); ifile++) {
      icvid = create_input_icvid(loopfile_info, ifile);
      (void) miicv_setint(icvid, MI_ICV_TYPE, NC_DOUBLE);
      (void) miicv_setint(icvid, MI_ICV_DO_NORM, TRUE);
      (void) miicv_setint(icvid, MI_ICV_USER_NORM, TRUE);
      (void) miicv_setint(icvid, MI_ICV_DO_FILLVALUE, TRUE);
      if (loop_options->convert_input_to_scalar) {
         (void) miicv_setint(icvid, MI_ICV_DO_DIM_CONV, TRUE);
         (void) miicv_setint(icvid, MI_ICV_DO_SCALAR, TRUE);
         (void) miicv_setint(icvid, MI_ICV_XDIM_DIR, MI_ICV_ANYDIR);
         (void) miicv_setint(icvid, MI_ICV_YDIM_DIR, MI_ICV_ANYDIR);
         (void) miicv_setint(icvid, MI_ICV_ZDIM_DIR, MI_ICV_ANYDIR);
         (void) miicv_setint(icvid, MI_ICV_KEEP_ASPECT, FALSE);
      }
   }

   /* Loop through output icv's, setting their values. Attaching is
      done by get_input_icvid. */
   for (ifile=0; ifile < get_output_numfiles(loopfile_info); ifile++) {
      icvid = create_output_icvid(loopfile_info, ifile);
      (void) miicv_setint(icvid, MI_ICV_TYPE, NC_DOUBLE);
      if( loop_options->is_labels )
      {
         (void) miicv_setint(icvid, MI_ICV_DO_NORM, FALSE);
         (void) miicv_setint(icvid, MI_ICV_USER_NORM, FALSE);
         (void) miicv_setint(icvid, MI_ICV_DO_RANGE, FALSE);
         
      } else {
         (void) miicv_setint(icvid, MI_ICV_DO_NORM, TRUE);
         (void) miicv_setint(icvid, MI_ICV_USER_NORM, TRUE);
      }
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : do_voxel_loop
@INPUT      : loop_options - user options for looping
              loopfile_info - information on files used in loop
@OUTPUT     : (none)
@RETURNS    : non-zero if an error occurs.
@DESCRIPTION: Routine to loop through the voxels and do something to each one
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 10, 1994 (Peter Neelin)
@MODIFIED   : November 30, 1994 (P.N.)
---------------------------------------------------------------------------- */
PRIVATE int do_voxel_loop(Loop_Options *loop_options,
                          Loopfile_Info *loopfile_info)
{
   long block_start[MAX_VAR_DIMS], block_end[MAX_VAR_DIMS];
   long block_incr[MAX_VAR_DIMS];
   long block_cur[MAX_VAR_DIMS], block_curcount[MAX_VAR_DIMS];
   long chunk_start[MAX_VAR_DIMS], chunk_end[MAX_VAR_DIMS];
   long chunk_incr[MAX_VAR_DIMS];
   long chunk_cur[MAX_VAR_DIMS], chunk_curcount[MAX_VAR_DIMS];
   long input_cur[MAX_VAR_DIMS], input_curcount[MAX_VAR_DIMS];
   long firstfile_cur[MAX_VAR_DIMS], firstfile_curcount[MAX_VAR_DIMS];
   double **input_buffers, **output_buffers, **extra_buffers;
   double **results_buffers;
   long chunk_num_voxels, block_num_voxels, ivox;
   int outmincid, imgid, maxid, minid;
   double *data, minimum, maximum, valid_range[2];
   double *global_minimum, *global_maximum;
   int ifile, ofile, ibuff, ndims, idim;
   int num_output_files;
   int num_input_buffers, num_output_buffers, num_extra_buffers;
   int input_vector_length, output_vector_length;
   int modify_vector_count;
   int current_input;
   int input_icvid, input_mincid;
   int loop_dim_index;
   int dim_index;
   int outer_file_loop;
   int dummy_index;
   int input_curfile;
   nc_type file_datatype;
   int status_code;
   int result_code = EXIT_SUCCESS;

   /* Get number of files, buffers, etc. */
   num_output_files = get_output_numfiles(loopfile_info);
   num_input_buffers = (loop_options->do_accumulate ? 1 : 
                        loop_options->num_all_inputs);
   num_extra_buffers = loop_options->num_extra_buffers;
   num_output_buffers = num_output_files + num_extra_buffers;
   input_vector_length = 
      get_vector_length(get_input_mincid(loopfile_info, 0), loop_options);
   if ((input_vector_length == 0) || (loop_options->convert_input_to_scalar))
      input_vector_length = 1;
   if (num_output_files > 0) {
      output_vector_length = 
         get_vector_length(get_output_mincid(loopfile_info, 0), NULL);
      if (output_vector_length == 0) output_vector_length = 1;
   }
   else
      output_vector_length = 1;
   modify_vector_count = (input_vector_length != output_vector_length);

   /* Initialize all of the counters to reasonable values */
   (void) miset_coords(MAX_VAR_DIMS, 0, block_start);
   (void) miset_coords(MAX_VAR_DIMS, 0, block_end);
   (void) miset_coords(MAX_VAR_DIMS, 0, block_incr);
   (void) miset_coords(MAX_VAR_DIMS, 0, block_cur);
   (void) miset_coords(MAX_VAR_DIMS, 0, block_curcount);
   (void) miset_coords(MAX_VAR_DIMS, 0, chunk_start);
   (void) miset_coords(MAX_VAR_DIMS, 0, chunk_end);
   (void) miset_coords(MAX_VAR_DIMS, 0, chunk_incr);
   (void) miset_coords(MAX_VAR_DIMS, 0, chunk_cur);
   (void) miset_coords(MAX_VAR_DIMS, 0, chunk_curcount);
   (void) miset_coords(MAX_VAR_DIMS, 0, input_cur);
   (void) miset_coords(MAX_VAR_DIMS, 0, input_curcount);
   (void) miset_coords(MAX_VAR_DIMS, 0, firstfile_cur);
   (void) miset_coords(MAX_VAR_DIMS, 0, firstfile_curcount);

   /* Get block and chunk looping information */
   setup_looping(loop_options, loopfile_info, &ndims,
                 block_start, block_end, 
                 block_incr, &block_num_voxels,
                 chunk_incr, &chunk_num_voxels);

   /* Allocate space for buffers */

   if (loop_options->allocate_buffer_function != NULL) {
      loop_options->allocate_buffer_function
         (loop_options->caller_data, TRUE, 
          num_input_buffers, chunk_num_voxels, input_vector_length, 
          &input_buffers, 
          num_output_files, block_num_voxels, output_vector_length, 
          &output_buffers, 
          num_extra_buffers, chunk_num_voxels, output_vector_length, 
          &extra_buffers, 
          loop_options->loop_info);
      
   }
   else {

      /* Allocate input buffers */
      input_buffers = MALLOC(num_input_buffers, double *);
      for (ibuff=0; ibuff < num_input_buffers; ibuff++) {
         input_buffers[ibuff] = MALLOC(chunk_num_voxels * input_vector_length, 
                                       double);
      }

      /* Allocate output buffers */
      if (num_output_files > 0) {
         output_buffers = MALLOC(num_output_files, double *);
         for (ibuff=0; ibuff < num_output_files; ibuff++) {
            output_buffers[ibuff] = MALLOC(block_num_voxels * 
                                           output_vector_length, double);
         }
      }

      /* Allocate extra buffers */
      if (num_extra_buffers > 0) {
         extra_buffers = MALLOC(num_extra_buffers, double *);
         for (ibuff=0; ibuff < num_extra_buffers; ibuff++) {
            extra_buffers[ibuff] = MALLOC(chunk_num_voxels *
                                          output_vector_length, double);
         }
      }

   }

   /* Set up the results pointers */
   if (num_output_buffers > 0) {
      results_buffers = MALLOC(num_output_buffers, double *);
      for (ibuff=0; ibuff < num_output_buffers; ibuff++) {
         if (ibuff < num_output_files) {
            results_buffers[ibuff] = output_buffers[ibuff];
         }
         else {
            results_buffers[ibuff] = extra_buffers[ibuff-num_output_files];
         }
      }
   }

   /* Initialize global min and max */
   if (num_output_files > 0) {
      global_minimum = MALLOC(num_output_files, double);
      global_maximum = MALLOC(num_output_files, double);
      for (ofile=0; ofile < num_output_files; ofile++) {
         global_minimum[ofile] = DBL_MAX;
         global_maximum[ofile] = -DBL_MAX;
      }
   }
   else {
      global_minimum = NULL;
      global_maximum = NULL;
   }

   /* Initialize loop info - just to be safe */
   initialize_loop_info(loop_options->loop_info);

   /* Print log message */
   if (loop_options->verbose) {
      (void) printf("Processing:");
      (void) fflush(stdout);
   }

   /* Outer loop over files, if appropriate */
   outer_file_loop = (loop_options->do_accumulate && 
                      (num_output_buffers <= 0));
   for (initialize_file_and_index(loop_options, loopfile_info,
                                  outer_file_loop, &ifile, &dim_index,
                                  &dummy_index);
        finish_file_and_index(loop_options, loopfile_info,
                              outer_file_loop, ifile, dim_index,
                              dummy_index);
        increment_file_and_index(loop_options, loopfile_info,
                                 outer_file_loop, &ifile, &dim_index,
                                 &dummy_index)) {

      /* Loop through blocks (image-max/min do not vary over blocks) */

      nd_begin_looping(block_start, block_cur, ndims);

      while (!nd_end_of_loop(block_cur, block_end, ndims)) {

         nd_update_current_count(block_cur, block_incr, block_end,
                                 block_curcount, ndims);

         /* Set results_buffers to beginning of output buffers */
         for (ofile=0; ofile < num_output_files; ofile++) {
            results_buffers[ofile] = output_buffers[ofile];
         }

         /* Loop through chunks (space for input buffers) */
         for (idim=0; idim < ndims; idim++) {
            chunk_start[idim] = block_cur[idim];
            chunk_end[idim] = block_cur[idim] + block_curcount[idim];
         }

         nd_begin_looping(chunk_start, chunk_cur, ndims);

         while (!nd_end_of_loop(chunk_cur, chunk_end, ndims)) {

            nd_update_current_count(chunk_cur, chunk_incr, chunk_end,
                                    chunk_curcount, ndims);

            /* Print log message */
            if (loop_options->verbose) {
               (void) printf(".");
               (void) fflush(stdout);
            }

            /* Calculate number of voxels in a chunk */
            chunk_num_voxels = 1;
            for (idim=0; idim < ndims; idim++)
               chunk_num_voxels *= chunk_curcount[idim];
            chunk_num_voxels /= input_vector_length;

            /* Translate start and count for file and save in loop_info */
            if (outer_file_loop)
               input_curfile = ifile;
            else
               input_curfile = 0;
            input_mincid = get_input_mincid(loopfile_info, input_curfile);
            translate_input_coords(input_mincid, chunk_cur, firstfile_cur,
                                   chunk_curcount, firstfile_curcount,
                                   &loop_dim_index, loop_options);

            /* Save start and count and file and index in loop_info */
            set_info_shape(loop_options->loop_info, 
                           firstfile_cur, firstfile_curcount);
            set_info_current_file(loop_options->loop_info, 0);
            set_info_current_index(loop_options->loop_info, 0);

            /* Initialize results buffers if necessary */
            if (loop_options->do_accumulate) {
               if (loop_options->start_function != NULL) {
                  loop_options->start_function
                     (loop_options->caller_data,
                      chunk_num_voxels,
                      num_output_buffers,
                      output_vector_length,
                      results_buffers,
                      loop_options->loop_info);
               }
            }

            /* Get the input buffers and accumulate them if needed */
            current_input = 0;
            for (initialize_file_and_index(loop_options, loopfile_info,
                                           !outer_file_loop, &ifile, 
                                           &dim_index, &dummy_index);
                 finish_file_and_index(loop_options, loopfile_info,
                                       !outer_file_loop, ifile, 
                                       dim_index, dummy_index);
                 increment_file_and_index(loop_options, loopfile_info,
                                          !outer_file_loop, &ifile, 
                                          &dim_index, &dummy_index)) {

               /* Get input icvid and mincid and translate coords for file.
                  We need to do this each time in case we have an outer
                  file loop. */
               input_icvid = get_input_icvid(loopfile_info, ifile);
               (void) miicv_inqint(input_icvid, MI_ICV_CDFID, 
                                   &input_mincid);
               translate_input_coords(input_mincid, chunk_cur, input_cur,
                                      chunk_curcount, input_curcount,
                                      &loop_dim_index, loop_options);


               /* Read buffer */
               ibuff = (loop_options->do_accumulate ? 0 : current_input);
               input_cur[loop_dim_index] = dim_index;
               status_code = miicv_get(input_icvid,
                                       input_cur, input_curcount, 
                                       input_buffers[ibuff]);
               if (status_code != MI_NOERROR) {
                 result_code = EXIT_FAILURE;
               }
               if (loop_options->do_accumulate) {
                  set_info_shape(loop_options->loop_info, 
                                 input_cur, input_curcount);
                  set_info_current_file(loop_options->loop_info, ifile);
                  set_info_current_index(loop_options->loop_info, dim_index);
                  set_info_loopfile_info(loop_options->loop_info, 
                                         loopfile_info);
                  loop_options->voxel_function(loop_options->caller_data,
                                               chunk_num_voxels, 
                                               num_input_buffers, 
                                               input_vector_length,
                                               input_buffers,
                                               num_output_buffers, 
                                               output_vector_length,
                                               results_buffers,
                                               loop_options->loop_info);
                  set_info_loopfile_info(loop_options->loop_info, NULL);
               }

               current_input++;

            }            /* Inner loop over files and dimension index */

            /* Do something with the buffers or finish accumulation */
            set_info_shape(loop_options->loop_info, 
                           firstfile_cur, firstfile_curcount);
            set_info_current_file(loop_options->loop_info, 0);
            set_info_current_index(loop_options->loop_info, 0);
            if (loop_options->do_accumulate) {
               if (loop_options->finish_function != NULL) {
                  loop_options->finish_function(loop_options->caller_data,
                                                chunk_num_voxels, 
                                                num_output_buffers,
                                                output_vector_length,
                                                results_buffers,
                                                loop_options->loop_info);
               }
            }
            else {
               loop_options->voxel_function(loop_options->caller_data,
                                            chunk_num_voxels, 
                                            num_input_buffers, 
                                            input_vector_length,
                                            input_buffers,
                                            num_output_buffers, 
                                            output_vector_length,
                                            results_buffers,
                                            loop_options->loop_info);
            }

            /* Increment results_buffers through output buffers */
            for (ofile=0; ofile < num_output_files; ofile++) {
               results_buffers[ofile] += 
                  chunk_num_voxels * output_vector_length;
            }

            nd_increment_loop(chunk_cur, chunk_start, chunk_incr, 
                              chunk_end, ndims);

         }     /* End of loop through chunks */

         /* Write out output buffers */

         for (ofile=0; ofile < num_output_files; ofile++) {
            outmincid = get_output_mincid(loopfile_info, ofile);
            maxid = ncvarid(outmincid, MIimagemax);
            minid = ncvarid(outmincid, MIimagemin);
            data = output_buffers[ofile];

            /* Find the max and min */
            minimum = DBL_MAX;
            maximum = -DBL_MAX;
            for (ivox=0; ivox < block_num_voxels*output_vector_length; 
                 ivox++) {
               if (data[ivox] != -DBL_MAX) {
                  if (data[ivox] < minimum) minimum = data[ivox];
                  if (data[ivox] > maximum) maximum = data[ivox];
               }
            }
            if ((minimum == DBL_MAX) && (maximum == -DBL_MAX)) {
               minimum = 0.0;
               maximum = 0.0;
            }

            /* Save global min and max */
            if (minimum < global_minimum[ofile]) 
               global_minimum[ofile] = minimum;
            if (maximum > global_maximum[ofile]) 
               global_maximum[ofile] = maximum;

            /* Write out the max and min */
            
            if( ! loop_options->is_labels )
            {
               (void) mivarput1(outmincid, maxid, block_cur, 
                              NC_DOUBLE, NULL, &maximum);
               (void) mivarput1(outmincid, minid, block_cur, 
                              NC_DOUBLE, NULL, &minimum);
            }
            /* Write out the values */
            if (modify_vector_count)
               block_curcount[ndims-1] = output_vector_length;
            (void) miicv_put(get_output_icvid(loopfile_info, ofile), 
                             block_cur, block_curcount, data);
         }          /* End of loop through output files */

         nd_increment_loop(block_cur, block_start, block_incr, 
                           block_end, ndims);

      }     /* End of loop through chunks */

   }     /* End of outer loop through files and dimension indices */

   /* Data has been completely written */
   for (ofile=0; ofile < num_output_files; ofile++) {
      outmincid = get_output_mincid(loopfile_info, ofile);
      imgid = ncvarid(outmincid, MIimage);
      (void) miattputstr(outmincid, imgid, MIcomplete, MI_TRUE);
      if (loop_options->is_floating_type) {
         if ((global_minimum[ofile] == DBL_MAX) && 
             (global_maximum[ofile] == -DBL_MAX)) {
            global_minimum[ofile] = 0.0;
            global_maximum[ofile] = 0.0;
         }
         valid_range[0] = global_minimum[ofile];
         valid_range[1] = global_maximum[ofile];

         /* Force truncation of valid_range to match float image */
         if ((ncvarinq(outmincid, imgid, NULL, &file_datatype, NULL, NULL,
                       NULL) != MI_ERROR) && (file_datatype == NC_FLOAT)) {
            valid_range[0] = (float) valid_range[0];
            valid_range[1] = (float) valid_range[1];
         }

         (void) miset_valid_range(outmincid, imgid, valid_range);

      }
      if( loop_options->is_labels )
      {
         /*Have to write out global valid range and global image range*/
         
         if ((global_minimum[ofile] == DBL_MAX) && 
             (global_maximum[ofile] == -DBL_MAX)) {
            global_minimum[ofile] = 0.0;
            global_maximum[ofile] = 0.0;
         }
         valid_range[0] = global_minimum[ofile];
         valid_range[1] = global_maximum[ofile];
         if( valid_range[0] == valid_range[1] ) 
         {
            /* HACK:
             * special case, the whole image contains the same value
             * to make minc tools happy we have to artificially create some range...
             * 
             */
            if(valid_range[1]>0) valid_range[0]-=1;
            else valid_range[1]+=1;
         }
         (void) mivarput1(outmincid, minid, 0, NC_DOUBLE, NULL, &valid_range[0]);
         (void) mivarput1(outmincid, maxid, 0, NC_DOUBLE, NULL, &valid_range[1]);
         (void) miset_valid_range(outmincid, imgid, valid_range);
      }
   }

   /* Print log message */
   if (loop_options->verbose) {
      (void) printf("Done\n");
      (void) fflush(stdout);
   }

   /* Free results pointer array, but not its buffers, since these
      were allocate as output_buffers and extra_buffers */
   if (num_output_buffers > 0) {
      FREE(results_buffers);
   }

   /* Free the buffers */
   if (loop_options->allocate_buffer_function != NULL) {
      loop_options->allocate_buffer_function
         (loop_options->caller_data, FALSE, 
          num_input_buffers, chunk_num_voxels, input_vector_length, 
          &input_buffers, 
          num_output_files, block_num_voxels, output_vector_length, 
          &output_buffers, 
          num_extra_buffers, chunk_num_voxels, output_vector_length, 
          &extra_buffers, 
          loop_options->loop_info);
   }
   else {

      /* Free input buffers */
      for (ibuff=0; ibuff < num_input_buffers; ibuff++) {
         FREE(input_buffers[ibuff]);
      }
      FREE(input_buffers);

      /* Free output buffers */
      if (num_output_files > 0) {
         for (ibuff=0; ibuff < num_output_files; ibuff++) {
            FREE(output_buffers[ibuff]);
         }
         FREE(output_buffers);
      }

      /* Free extra buffers */
      if (num_extra_buffers > 0) {
         for (ibuff=0; ibuff < num_extra_buffers; ibuff++) {
            FREE(extra_buffers[ibuff]);
         }
         FREE(extra_buffers);
      }

   }

   /* Free max and min arrays */
   if (num_output_files > 0) {
      FREE(global_minimum);
      FREE(global_maximum);
   }

   return result_code;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : setup_looping
@INPUT      : loop_options - users options controlling looping
              loopfile_info - information on files
@OUTPUT     : ndims - number of dimensions
              block_start - vector specifying start of block
              block_end - end of block
              block_incr - increment for stepping through blocks
              block_num_voxels - number of voxels in block
              chunk_incr - increment for stepping through chunks
              chunk_num_voxels - number of voxels in chunk
@RETURNS    : (nothing)
@DESCRIPTION: Routine to set up vectors giving blocks and chunks through
              which we will loop.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 2, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void setup_looping(Loop_Options *loop_options, 
                           Loopfile_Info *loopfile_info,
                           int *ndims,
                           long block_start[], long block_end[], 
                           long block_incr[], long *block_num_voxels,
                           long chunk_incr[], long *chunk_num_voxels)
{
   int inmincid;
   int total_ndims, scalar_ndims, idim;
   int input_vector_length, output_vector_length;
   int num_input_buffers;
   int vector_data;
   int nimgdims;
   long size[MAX_VAR_DIMS];
   long max_voxels_in_buffer;

   /* Get input mincid */
   inmincid = get_input_mincid(loopfile_info, 0);

   /* Get number of dimensions and their sizes */
   get_dim_info(inmincid, &total_ndims, size, NULL, NULL, NULL, NULL, NULL,
                loop_options);

   /* Get vector lengths */
   input_vector_length = get_vector_length(inmincid, loop_options);
   if (get_output_numfiles(loopfile_info) > 0)
      output_vector_length = 
         get_vector_length(get_output_mincid(loopfile_info, 0), NULL);
   else
      output_vector_length = 0;

   /* Check for vector data and whether we are adding a dimension */
   vector_data = ((input_vector_length > 0) || (output_vector_length > 0));
   if ((input_vector_length == 0) && (output_vector_length > 0)) {
      total_ndims++;
      size[total_ndims-1] = 1;
   }
   scalar_ndims = (vector_data ? total_ndims - 1 : total_ndims);

   /* Get number of image dimensions */
   nimgdims = (vector_data ? 3 : 2);

   /* Set vector lengths properly */
   if (input_vector_length <= 0) input_vector_length = 1;
   if (output_vector_length <= 0) output_vector_length = 1;

   /* Set vectors */
   *block_num_voxels = 1;
   for (idim=0; idim < total_ndims; idim++) {
      block_start[idim] = 0;
      block_end[idim] = size[idim];
      if (idim < total_ndims - nimgdims)
         block_incr[idim] = 1;
      else 
         block_incr[idim] = size[idim];
      *block_num_voxels *= block_incr[idim];
      chunk_incr[idim] = 1;
   }
   if (vector_data) {
      *block_num_voxels /= input_vector_length;
      idim = total_ndims-1;
      chunk_incr[idim] = block_incr[idim];
   }

   /* Figure out chunk size. Enforce a minimum chunk size. */
   *chunk_num_voxels = 1;
   num_input_buffers = (loop_options->do_accumulate ? 1 : 
                        loop_options->num_all_inputs);
   max_voxels_in_buffer = 
      (loop_options->total_copy_space/((long) sizeof(double)) - 
       get_output_numfiles(loopfile_info) * *block_num_voxels *
       output_vector_length) / 
          (num_input_buffers * input_vector_length + 
           loop_options->num_extra_buffers * output_vector_length);
   if (max_voxels_in_buffer < MIN_VOXELS_IN_BUFFER) {
      max_voxels_in_buffer = MIN_VOXELS_IN_BUFFER;
   }
   if (max_voxels_in_buffer > 0) {
      for (idim=scalar_ndims-1; idim >= 0; idim--) {
         chunk_incr[idim] = max_voxels_in_buffer / *chunk_num_voxels;
         if (chunk_incr[idim] > block_incr[idim])
            chunk_incr[idim] = block_incr[idim];
         *chunk_num_voxels *= chunk_incr[idim];
      }
   }

   /* Set ndims */
   *ndims = total_ndims;
                
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_file_and_index
@INPUT      : loop_options - users options controlling looping
              loopfile_info - information on files
              do_loop - TRUE if looping stuff should really be done
@OUTPUT     : ifile - file counter
              dim_index - dimension index counter
              dummy_index - counter used when do_loop is FALSE
@RETURNS    : (nothing)
@DESCRIPTION: Routine to initialize the file and index loop. These 
              three routines allow looping through files and dimension 
              indices at two levels. If do_loop is TRUE, then normal looping 
              is done (increment dim_index fastest, then ifile). If do_loop is
              FALSE, then only one loop is performed (using dummy_index).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void initialize_file_and_index(Loop_Options *loop_options, 
                                       Loopfile_Info *loopfile_info,
                                       int do_loop,
                                       int *ifile, int *dim_index,
                                       int *dummy_index)
     /* ARGSUSED */
{
   if (do_loop) {
      *ifile = 0;
      *dim_index = 0;
   }
   else {
      *dummy_index = 0;
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : finish_file_and_index
@INPUT      : loop_options - users options controlling looping
              loopfile_info - information on files
              do_loop - TRUE if looping stuff should really be done
              ifile - file counter
              dim_index - dimension index counter
              dummy_index - counter used when do_loop is FALSE
@RETURNS    : TRUE while there are more buffers to process.
@DESCRIPTION: Routine to test for the end of the file and index loop. These 
              three routines allow looping through files and dimension 
              indices at two levels. If do_loop is TRUE, then normal looping 
              is done (increment dim_index fastest, then ifile). If do_loop is
              FALSE, then only one loop is performed (using dummy_index).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int finish_file_and_index(Loop_Options *loop_options, 
                                  Loopfile_Info *loopfile_info,
                                  int do_loop,
                                  int ifile, int dim_index,
                                  int dummy_index)
     /* ARGSUSED */
{
   if (do_loop) {
      return (ifile < get_input_numfiles(loopfile_info));
   }
   else {
      return (dummy_index < 1);
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : increment_file_and_index
@INPUT      : loop_options - users options controlling looping
              loopfile_info - information on files
              do_loop - TRUE if looping stuff should really be done
@OUTPUT     : ifile - file counter
              dim_index - dimension index counter
              dummy_index - counter used when do_loop is FALSE
@RETURNS    : (nothing)
@DESCRIPTION: Routine to increment the file and index loop. These 
              three routines allow looping through files and dimension 
              indices at two levels. If do_loop is TRUE, then normal looping 
              is done (increment dim_index fastest, then ifile). If do_loop is
              FALSE, then only one loop is performed (using dummy_index).
              Note that dummy_index is not touched if do_loop is TRUE.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void increment_file_and_index(Loop_Options *loop_options, 
                                      Loopfile_Info *loopfile_info,
                                      int do_loop,
                                      int *ifile, int *dim_index,
                                      int *dummy_index)
{
   int input_mincid;

   if (do_loop) {
      (*dim_index)++;
      input_mincid = get_input_mincid(loopfile_info, *ifile);
      if (*dim_index >= get_loop_dim_size(input_mincid, loop_options)) {
         *dim_index = 0;
         (*ifile)++;
      }
   }
   else {
      (*dummy_index)++;
   }
}


/* ------------ Routines controlling looping over files ------------ */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_loopfile_info
@INPUT      : num_input_files - number of input files
              input_files - list of input file names
              num_output_files - list of output file names
              output_files - list of output file names
              loop_options - user options for looping
@OUTPUT     : (none)
@RETURNS    : pointer to Loopfile_Info structure
@DESCRIPTION: Routine to set up looping information for these files.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE Loopfile_Info *initialize_loopfile_info(int num_input_files,
                                                char *input_files[],
                                                int num_output_files,
                                                char *output_files[],
                                                Loop_Options *loop_options)
{
   int num_free_files, num_files, ifile;
   Loopfile_Info *loopfile_info;

   /* Allocate structure */
   loopfile_info = MALLOC(1, Loopfile_Info);

   /* Save clobber info */
   if (loop_options->clobber) {
       loopfile_info->cflags = NC_CLOBBER;
   }
   else {
       loopfile_info->cflags = NC_NOCLOBBER;
   }

#if MINC2
   if (loop_options->v2format) {
       loopfile_info->cflags |= MI2_CREATE_V2;
   }
#endif /* MINC2 */

   /* Save number of input and output files */
   loopfile_info->num_input_files = num_input_files;
   loopfile_info->num_output_files = num_output_files;

   /* Save input file names (just copy pointers, not strings) */
   if (num_input_files > 0) {
      loopfile_info->input_files = MALLOC(num_input_files, char *);
      for (ifile=0; ifile < num_input_files; ifile++)
         loopfile_info->input_files[ifile] = input_files[ifile];
   }
   else
      loopfile_info->input_files = NULL;

   /* Save output file names (just copy pointers, not strings) */
   if (num_output_files > 0) {
      loopfile_info->output_files = MALLOC(num_output_files, char *);
      for (ifile=0; ifile < num_output_files; ifile++)
         loopfile_info->output_files[ifile] = output_files[ifile];
   }
   else
      loopfile_info->output_files = NULL;

   /* Keep track of number of files that we can open */
   num_free_files = loop_options->max_open_files;
   if (num_free_files > MI_MAX_NUM_ICV) num_free_files = MI_MAX_NUM_ICV;

   /* Check to see if we can open output files (we must leave room for one
      input file) */
   if (num_output_files < num_free_files-1) {
      loopfile_info->output_all_open = TRUE;
      num_files = num_output_files;
   }
   else {
      loopfile_info->output_all_open = FALSE;
      num_files = 1;
   }
   num_free_files -= num_files;
   loopfile_info->output_mincid = MALLOC(num_files, int);
   loopfile_info->output_icvid = MALLOC(num_files, int);
   for (ifile=0; ifile < num_files; ifile++) {
      loopfile_info->output_mincid[ifile] = MI_ERROR;
      loopfile_info->output_icvid[ifile] = MI_ERROR;
   }
   loopfile_info->current_input_file_number = -1;

   /* Check whether sequential access would be better */
   loopfile_info->sequential_access = 
      (loop_options->do_accumulate &&
       ((num_output_files + loop_options->num_extra_buffers) <= 0));

   /* Check to see if we can open input files */
   if (num_input_files < num_free_files) { 
      loopfile_info->can_open_all_input = TRUE;
      num_files = num_input_files;
   }
   else {
      loopfile_info->can_open_all_input = FALSE;
      num_files = 1;
   }
   num_free_files -= num_files;
   loopfile_info->input_mincid = MALLOC(num_files, int);
   loopfile_info->input_icvid = MALLOC(num_files, int);
   for (ifile=0; ifile < num_files; ifile++) {
      loopfile_info->input_mincid[ifile] = MI_ERROR;
      loopfile_info->input_icvid[ifile] = MI_ERROR;
   }
   loopfile_info->current_output_file_number = -1;

   /* Check for an already open input file */
   if (loop_options->input_mincid != MI_ERROR) {
      loopfile_info->input_mincid[0] = loop_options->input_mincid;
      loopfile_info->current_input_file_number = 0;
   }

   /* Check whether we want to open all input files */
   loopfile_info->input_all_open = (! loopfile_info->sequential_access) &&
      loopfile_info->can_open_all_input;

   /* Set default for expanding compressed files */
   loopfile_info->headers_only = FALSE;
   loopfile_info->want_headers_only = FALSE;

   /* Return the loopfile_info structure */
   return loopfile_info;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : cleanup_loopfile_info
@INPUT      : loopfile_info - looping information
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to clean up looping information.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void cleanup_loopfile_info(Loopfile_Info *loopfile_info)
{
   int num_files, ifile;

   /* Close input files and free icv's */
   if (loopfile_info->can_open_all_input)
      num_files = loopfile_info->num_input_files;
   else
      num_files = 1;
   for (ifile=0; ifile < num_files; ifile++) {
      if (loopfile_info->input_icvid[ifile] != MI_ERROR)
         (void) miicv_free(loopfile_info->input_icvid[ifile]);
      if (loopfile_info->input_mincid[ifile] != MI_ERROR)
         (void) miclose(loopfile_info->input_mincid[ifile]);
   }

   /* Close output files and free icv's */
   if (loopfile_info->output_all_open)
      num_files = loopfile_info->num_output_files;
   else
      num_files = 1;
   for (ifile=0; ifile < num_files; ifile++) {
      (void) miicv_free(loopfile_info->output_icvid[ifile]);
      (void) miclose(loopfile_info->output_mincid[ifile]);
   }

   /* Free input arrays */
   if (loopfile_info->input_files != NULL)
      FREE(loopfile_info->input_files);
   if (loopfile_info->input_mincid != NULL)
      FREE(loopfile_info->input_mincid);
   if (loopfile_info->input_icvid != NULL)
      FREE(loopfile_info->input_icvid);

   /* Free output arrays */
   if (loopfile_info->output_files != NULL)
      FREE(loopfile_info->output_files);
   if (loopfile_info->output_mincid != NULL)
      FREE(loopfile_info->output_mincid);
   if (loopfile_info->output_icvid != NULL)
      FREE(loopfile_info->output_icvid);

   /* Free the structure */
   FREE(loopfile_info);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_input_numfiles
@INPUT      : loopfile_info - looping information
@OUTPUT     : (none)
@RETURNS    : Number of input files
@DESCRIPTION: Routine to get the number of input files.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_input_numfiles(Loopfile_Info *loopfile_info)
{
   return loopfile_info->num_input_files;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_output_numfiles
@INPUT      : loopfile_info - looping information
@OUTPUT     : (none)
@RETURNS    : Number of output files
@DESCRIPTION: Routine to get the number of output files.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_output_numfiles(Loopfile_Info *loopfile_info)
{
   return loopfile_info->num_output_files;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_input_filename
@INPUT      : loopfile_info - looping information
              file_num - input file number
@OUTPUT     : (none)
@RETURNS    : Pointer to string containing input file name for file ifile.
@DESCRIPTION: Routine to get name of an input file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE char *get_input_filename(Loopfile_Info *loopfile_info, int file_num)
{
   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_input_files)) {
      (void) fprintf(stderr, "Bad input file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   return loopfile_info->input_files[file_num];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_input_headers_only
@INPUT      : loopfile_info - looping information
              headers_only - TRUE if we only need input headers, FALSE
                 if we need the whole file.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to modify the Loopfile_Info structure so that
              in future we get either whole minc files or only the headers.
              The change is only made if it makes sense (ie. we are processing
              files sequentially).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void set_input_headers_only(Loopfile_Info *loopfile_info,
                                    int headers_only)
{
   int num_files, ifile;
   int icvid, mincid;

   /* Change the indication that we want to have headers only */
   loopfile_info->want_headers_only = headers_only;

   /* If headers_only is not changing, don't do anything */
   if ((headers_only && loopfile_info->headers_only) ||
       (!headers_only && !loopfile_info->headers_only)) {
      return;
   }

   /* Check to see if it makes sense to change to headers only */
   if (headers_only && loopfile_info->input_all_open) return;

   /* Change the value */
   loopfile_info->headers_only = headers_only;

   /* If we are going to headers_only == FALSE, then loop through icv's and 
      files, making sure that they are detached and closed (we will need to 
      re-open them */
   if (!loopfile_info->headers_only) {
      num_files = (loopfile_info->can_open_all_input ?
                   loopfile_info->num_input_files : 1);
      for (ifile=0; ifile < num_files; ifile++) {
         icvid = loopfile_info->input_icvid[ifile];
         mincid = MI_ERROR;
         if (icvid != MI_ERROR) {
            (void) miicv_inqint(icvid, MI_ICV_CDFID, &mincid);
            if (mincid != MI_ERROR) {
               (void) miicv_detach(icvid);
               (void) miclose(mincid);
            }
         }
         if ((loopfile_info->input_mincid[ifile] != MI_ERROR) &&
             (loopfile_info->input_mincid[ifile] != mincid)) {
            (void) miclose(loopfile_info->input_mincid[ifile]);
         }
         loopfile_info->input_mincid[ifile] = MI_ERROR;
      }
         
   }

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_input_sequential
@INPUT      : loopfile_info - looping information
              sequential_access - TRUE if we want to open only one file at a
                 time, FALSE otherwise.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to modify the Loopfile_Info structure so that
              files are opened one at a time.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 1, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void set_input_sequential(Loopfile_Info *loopfile_info,
                                  int sequential_access)
{
   int old_input_all_open;
   int ifile, num_files;
   int mincid = MI_ERROR, icvid;
   int current_input_file_number;

   /* Set flag for sequential access */
   loopfile_info->sequential_access = sequential_access;

   /* Change status of input_all_open */
   old_input_all_open = loopfile_info->input_all_open;
   loopfile_info->input_all_open = (! loopfile_info->sequential_access) &&
      loopfile_info->can_open_all_input;

   /* Check if input_all_open has changed */
   if (!old_input_all_open && loopfile_info->input_all_open) {
      current_input_file_number = loopfile_info->current_input_file_number;
      if (current_input_file_number >= 0) {
         mincid = loopfile_info->input_mincid[0];
         loopfile_info->input_mincid[0] = MI_ERROR;
         loopfile_info->input_mincid[current_input_file_number] = mincid;
      }
   }
   else if (old_input_all_open && !loopfile_info->input_all_open) {
      if (loopfile_info->can_open_all_input)
         num_files = loopfile_info->num_input_files;
      else
         num_files = 1;
      for (ifile=0; ifile < num_files; ifile++) {
         icvid = loopfile_info->input_icvid[ifile];
         if (icvid != MI_ERROR) {
            (void) miicv_inqint(icvid, MI_ICV_CDFID, &mincid);
            (void) miicv_detach(icvid);
            if (mincid != MI_ERROR) {
               (void) miclose(mincid);
            }
         }
         if ((loopfile_info->input_mincid[ifile] != MI_ERROR) &&
             (loopfile_info->input_mincid[ifile] != mincid))
            (void) miclose(loopfile_info->input_mincid[ifile]);
         loopfile_info->input_mincid[ifile] = MI_ERROR;
      }
   }

   /* Call set_input_headers_only in case want_headers_only is different
      from headers_only */
   set_input_headers_only(loopfile_info, loopfile_info->want_headers_only);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_input_mincid
@INPUT      : loopfile_info - looping information
              file_num - input file number
@OUTPUT     : (none)
@RETURNS    : Id of minc file
@DESCRIPTION: Routine to get the minc id for an input file. The file number
              corresponds to the file's position in the input_files list
              (counting from zero).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_input_mincid(Loopfile_Info *loopfile_info,
                             int file_num)
{
   int index;
   int created_tempfile;
   char *filename;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_input_files)) {
      (void) fprintf(stderr, "Bad input file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not */
   if (loopfile_info->input_all_open) {
      index = file_num;
   }
   else {
      index = 0;
      if ((loopfile_info->input_mincid[index] != MI_ERROR) &&
          (loopfile_info->current_input_file_number != file_num)) {
         if (loopfile_info->input_icvid[index] != MI_ERROR)
            (void) miicv_detach(loopfile_info->input_icvid[index]);
         (void) miclose(loopfile_info->input_mincid[index]);
         loopfile_info->input_mincid[index] = MI_ERROR;
      }
      loopfile_info->current_input_file_number = file_num;
   }

   /* Open the file if it hasn't been already */
   if (loopfile_info->input_mincid[index] == MI_ERROR) {
      filename = miexpand_file(loopfile_info->input_files[file_num], NULL,
                               loopfile_info->headers_only,
                               &created_tempfile);
      if (!filename) {
         fprintf(stderr, "Could not expand file \"%s\"!\n", loopfile_info->input_files[file_num]);
         exit(EXIT_FAILURE);
      }
      loopfile_info->input_mincid[index] = miopen(filename, NC_NOWRITE);
      if (created_tempfile) {
         (void) remove(filename);
      }
      FREE(filename);
   }

   return loopfile_info->input_mincid[index];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_output_mincid
@INPUT      : loopfile_info - looping information
              file_num - output file number
@OUTPUT     : (none)
@RETURNS    : Id of minc file
@DESCRIPTION: Routine to get the minc id for an output file. The file number
              corresponds to the file's position in the output_files list
              (counting from zero).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_output_mincid(Loopfile_Info *loopfile_info,
                              int file_num)
{
   int index;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_output_files)) {
      (void) fprintf(stderr, "Bad output file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not */
   if (loopfile_info->output_all_open) {
      index = file_num;
   }
   else {
      index = 0;
      if ((loopfile_info->output_mincid[index] != MI_ERROR) &&
          (loopfile_info->current_output_file_number != file_num)) {
         if (loopfile_info->output_icvid[index] != MI_ERROR)
            (void) miicv_detach(loopfile_info->output_icvid[index]);
         (void) miclose(loopfile_info->output_mincid[index]);
         loopfile_info->output_mincid[index] = MI_ERROR;
      }
      loopfile_info->current_output_file_number = file_num;
   }

   /* Open the file if it hasn't been already */
   if (loopfile_info->output_mincid[index] == MI_ERROR) {
      loopfile_info->output_mincid[index] =
         miopen(loopfile_info->output_files[file_num], NC_WRITE);
   }

   return loopfile_info->output_mincid[index];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_output_file
@INPUT      : loopfile_info - looping information
              file_num - output file number
@OUTPUT     : (none)
@RETURNS    : Id of minc file
@DESCRIPTION: Routine to create an output file. The file number
              corresponds to the file's position in the output_files list
              (counting from zero).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int create_output_file(Loopfile_Info *loopfile_info,
                               int file_num)
{
   int index;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_output_files)) {
      (void) fprintf(stderr, "Bad output file number %d for create.\n", 
                     file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not */
   if (loopfile_info->output_all_open) {
      index = file_num;
   }
   else {
      index = 0;
      if ((loopfile_info->output_mincid[index] != MI_ERROR) &&
          (loopfile_info->current_output_file_number != file_num)) {
         (void) miclose(loopfile_info->output_mincid[index]);
         loopfile_info->output_mincid[index] = MI_ERROR;
      }
      loopfile_info->current_output_file_number = file_num;
   }

   /* Create the file */
   if (loopfile_info->output_mincid[index] != MI_ERROR) {
      (void) fprintf(stderr, "File %s has already been created\n",
                     loopfile_info->output_files[file_num]);
      exit(EXIT_FAILURE);
   }
   loopfile_info->output_mincid[index] =
      micreate(loopfile_info->output_files[file_num], 
               loopfile_info->cflags);

   return loopfile_info->output_mincid[index];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_input_icvid
@INPUT      : loopfile_info - looping information
              file_num - input file number
@OUTPUT     : (none)
@RETURNS    : Id of icv for the specified file
@DESCRIPTION: Routine to get the icv id for an input file. The file number
              corresponds to the file's position in the input_files list
              (counting from zero).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_input_icvid(Loopfile_Info *loopfile_info,
                            int file_num)
{
   int mincid, icv_mincid, icvid;
   int index;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_input_files)) {
      (void) fprintf(stderr, "Bad input file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not - get the correct index */
   if (loopfile_info->can_open_all_input) {
      index = file_num;
   }
   else {
      index = 0;
   }

   /* Check to see if the icv is attached to the correct minc file. If
      not, re-attach it. */
   icvid = loopfile_info->input_icvid[index];
   mincid = get_input_mincid(loopfile_info, file_num);
   if (icvid != MI_ERROR)
      (void) miicv_inqint(icvid, MI_ICV_CDFID, &icv_mincid);
   else
      icv_mincid = MI_ERROR;
   if (mincid != icv_mincid) {
      (void) miicv_attach(icvid, mincid, ncvarid(mincid, MIimage));
   }
   return icvid;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_output_icvid
@INPUT      : loopfile_info - looping information
              file_num - output file number
@OUTPUT     : (none)
@RETURNS    : Id of icv for the specified file
@DESCRIPTION: Routine to get the icv id for an output file. The file number
              corresponds to the file's position in the output_files list
              (counting from zero).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int get_output_icvid(Loopfile_Info *loopfile_info,
                             int file_num)
{
   int mincid, icv_mincid, icvid;
   int index;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_output_files)) {
      (void) fprintf(stderr, "Bad output file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not - get the correct index */
   if (loopfile_info->output_all_open) {
      index = file_num;
   }
   else {
      index = 0;
   }

   /* Check to see if the icv is attached to the correct minc file. If
      not, re-attach it. */
   icvid = loopfile_info->output_icvid[index];
   mincid = get_output_mincid(loopfile_info, file_num);
   if (icvid != MI_ERROR)
      (void) miicv_inqint(icvid, MI_ICV_CDFID, &icv_mincid);
   else
      icv_mincid = MI_ERROR;
   if (mincid != icv_mincid) {
      (void) miicv_attach(icvid, mincid, ncvarid(mincid, MIimage));
   }
   return icvid;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_input_icvid
@INPUT      : loopfile_info - looping information
              file_num - input file number
@OUTPUT     : (none)
@RETURNS    : Id of icv for the specified file
@DESCRIPTION: Routine to create the icv id for an input file. The file number
              corresponds to the file's position in the input_files list
              (counting from zero). If the icv already exists, just 
              return it. If there are too many files, then only one
              icv will be used.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int create_input_icvid(Loopfile_Info *loopfile_info,
                               int file_num)
{
   int index;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_input_files)) {
      (void) fprintf(stderr, "Bad input file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not - get the correct index */
   if (loopfile_info->can_open_all_input) {
      index = file_num;
   }
   else {
      index = 0;
   }

   /* Check to see if icv exists - if not create it */
   if (loopfile_info->input_icvid[index] == MI_ERROR) {
      loopfile_info->input_icvid[index] = miicv_create();
   }

   return loopfile_info->input_icvid[index];
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_output_icvid
@INPUT      : loopfile_info - looping information
              file_num - output file number
@OUTPUT     : (none)
@RETURNS    : Id of icv for the specified file
@DESCRIPTION: Routine to create the icv id for an output file. The file number
              corresponds to the file's position in the output_files list
              (counting from zero). If the icv already exists, just 
              return it. If there are too many files, then only one
              icv will be used.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 30, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int create_output_icvid(Loopfile_Info *loopfile_info,
                                int file_num)
{
   int index;

   /* Check for bad file_num */
   if ((file_num < 0) || (file_num >= loopfile_info->num_output_files)) {
      (void) fprintf(stderr, "Bad output file number %d\n", file_num);
      exit(EXIT_FAILURE);
   }

   /* Check to see if all files are open or not - get the correct index */
   if (loopfile_info->output_all_open) {
      index = file_num;
   }
   else {
      index = 0;
   }

   /* Check to see if icv exists - if not create it */
   if (loopfile_info->output_icvid[index] == MI_ERROR) {
      loopfile_info->output_icvid[index] = miicv_create();
   }

   return loopfile_info->output_icvid[index];
}


/* ------------ Routines to set loop options ------------ */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_loop_options
@INPUT      : (none)
@OUTPUT     : (none)
@RETURNS    : Pointer to Loop_Options structure
@DESCRIPTION: Routine to create and initialize the loop options structure.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI Loop_Options *create_loop_options(void)
{
   Loop_Options *loop_options;

   /* Allocate structure */
   loop_options = MALLOC(1, Loop_Options);

   /* Fill in the defaults */
   loop_options->clobber = FALSE;
   loop_options->verbose = TRUE;
   loop_options->datatype = MI_ORIGINAL_TYPE;
   loop_options->is_signed = TRUE;
   loop_options->valid_range[0] = 0.0;
   loop_options->valid_range[1] = 0.0;
   loop_options->max_open_files = MI_MAX_NUM_ICV - 2;
   loop_options->check_all_input_dim_info = TRUE;
   loop_options->convert_input_to_scalar = FALSE;
   loop_options->output_vector_size = 0;
   loop_options->input_mincid = MI_ERROR;
   
   loop_options->total_copy_space = miget_cfg_int(MICFG_MAXBUF) * 1024;
   if(loop_options->total_copy_space == 0){
      loop_options->total_copy_space = MI2_DEF_BUFF_SIZE * 1024;
      }
   
   loop_options->loop_dimension = NULL;
   loop_options->num_all_inputs = 0;
   loop_options->input_file_function = NULL;
   loop_options->output_file_function = NULL;
   loop_options->copy_all_header_info = TRUE;
   loop_options->do_accumulate = FALSE;
   loop_options->num_extra_buffers = 0;
   loop_options->start_function = NULL;
   loop_options->finish_function = NULL;
   loop_options->voxel_function = NULL;
   loop_options->caller_data = NULL;
   loop_options->loop_info = create_loop_info();

   loop_options->allocate_buffer_function = NULL;

   loop_options->is_labels = FALSE; /* for backward compatibility*/
   
#if MINC2
   loop_options->v2format = FALSE; /* Use MINC 2.0 file format (HDF5)? */
#endif /* MINC2 */

   /* Return the structure pointer */
   return loop_options;
}

MNCAPI void set_loop_labels(Loop_Options *loop_options, 
                             int labels)
{
  loop_options->is_labels = labels;
}

MNCAPI int get_loop_labels(Loop_Options *loop_options)
{
  return loop_options->is_labels;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_loop_options
@INPUT      : loop_options - pointer to structure to cleanup
@OUTPUT     : (none)
@RETURNS    : (none)
@DESCRIPTION: Routine to cleanup and free the Loop_Options structure
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void free_loop_options(Loop_Options *loop_options)
{
   free_loop_info(loop_options->loop_info);
   if (loop_options->loop_dimension != NULL)
      FREE(loop_options->loop_dimension);
   FREE(loop_options);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_clobber
@INPUT      : loop_options - user options for looping
              verbose - TRUE if output files should be clobbered
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn output clobber on or off
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_clobber(Loop_Options *loop_options, 
                             int clobber)
{
   loop_options->clobber = clobber;
}

#if MINC2
/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_v2format
@INPUT      : loop_options - user options for looping
              v2format - TRUE if output files should use MINC 2.0 format
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn output clobber on or off
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : October 8, 2003 (Bert Vincent)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_v2format(Loop_Options *loop_options, int v2format)
{
   loop_options->v2format = v2format;
}
#endif /* MINC2 */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_verbose
@INPUT      : loop_options - user options for looping
              verbose - TRUE if logging should be done.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn logging on or off.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_verbose(Loop_Options *loop_options, 
                             int verbose)
{
   loop_options->verbose = verbose;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_datatype
@INPUT      : loop_options - user options for looping
              datatype - NetCDF datatype for output (MI_ORIGINAL_TYPE means
                 use input type)
              is_signed - TRUE if type is signed
              valid_min - valid minimum for type (if valid_min >= valid_max,
                 then defaults are used)
              valid_max - valid maximum for type
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to set the output file datatype, sign and range.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_datatype(Loop_Options *loop_options, 
                              nc_type datatype, int is_signed,
                              double valid_min, double valid_max)
{
   loop_options->datatype = datatype;
   loop_options->is_signed = is_signed;
   loop_options->valid_range[0] = valid_min;
   loop_options->valid_range[1] = valid_max;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_max_open_files
@INPUT      : loop_options - user options for looping
              max_open_files - maximum number of open files allowed (between
                 1 and MI_MAX_NUM_ICV)
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to set the maximum number of open minc files.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_max_open_files(Loop_Options *loop_options, 
                                    int max_open_files)
{
   if ((max_open_files <= 0) || (max_open_files > MI_MAX_NUM_ICV)) {
      (void) fprintf(stderr, 
                     "Bad number of files %d in set_loop_max_open_files\n",
                     max_open_files);
      exit(EXIT_FAILURE);
   }

   loop_options->max_open_files = max_open_files;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_check_dim_info
@INPUT      : loop_options - user options for looping
              check_dim_info - TRUE if all dimension information in input
                 files should be check, FALSE otherwise
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn dimension info checking on or off.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 16, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_check_dim_info(Loop_Options *loop_options, 
                                    int check_dim_info)
{
   loop_options->check_all_input_dim_info = check_dim_info;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_convert_input_to_scalar
@INPUT      : loop_options - user options for looping
              convert_input_to_scalar - TRUE if input should be converted
                 to scalar values
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to allow input to be converted to scalar values
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_convert_input_to_scalar(Loop_Options *loop_options, 
                                             int convert_input_to_scalar)
{
   loop_options->convert_input_to_scalar = convert_input_to_scalar;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_output_vector_size
@INPUT      : loop_options - user options for looping
              output_vector_size - length of vector dimension for output.
                 0 means keep the size the same as the input.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn set the length of the vector dimension for 
              output.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_output_vector_size(Loop_Options *loop_options, 
                                        int output_vector_size)
{
   if (output_vector_size < 0) {
      (void) fprintf(stderr, 
                     "Bad vector size %d in set_loop_output_vector_size\n",
                     output_vector_size);
      exit(EXIT_FAILURE);
   }
   loop_options->output_vector_size = output_vector_size;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_first_input_mincid
@INPUT      : loop_options - user options for looping
              input_mincid - id of first minc file (already opened).
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn allow the user to pass in an already opened minc
              file.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_first_input_mincid(Loop_Options *loop_options, 
                                        int input_mincid)
{
   loop_options->input_mincid = input_mincid;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_buffer_size
@INPUT      : loop_options - user options for looping
              buffer_size - maximum amount of buffer space to use (in bytes).
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn set a limit on the amount of buffer space used.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_buffer_size(Loop_Options *loop_options,
                                 long buffer_size)
{
   if (buffer_size <= 0) {
      (void) fprintf(stderr, "Bad buffer size %d in set_loop_buffer_size\n",
                     (int) buffer_size);
   }

   loop_options->total_copy_space = buffer_size;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_dimension
@INPUT      : loop_options - user options for looping
              dimension_name - name of dimension that is treated like a series
                 of input files.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to allow a dimension to be treated like a series of
              input files (one buffer per dimension element per file).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 24, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_dimension(Loop_Options *loop_options,
                               char *dimension_name)
{
   if (loop_options->loop_dimension != NULL)
      FREE(loop_options->loop_dimension);

   if ((dimension_name == NULL) || ((int)strlen(dimension_name) <= 0)) {
      loop_options->loop_dimension = NULL;
   }
   else {
      loop_options->loop_dimension = strdup(dimension_name);
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_input_file_function
@INPUT      : loop_options - user options for looping
              input_file_function - function to be called for each
                 input file so that the user can extract any extract
                 information.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to allow the user to define a function to be called
              for each input file before processing is done.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_input_file_function
   (Loop_Options *loop_options,
    VoxelInputFileFunction input_file_function)
{
   loop_options->input_file_function = input_file_function;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_output_file_function
@INPUT      : loop_options - user options for looping
              output_file_function - function to be called for each
                 output file so that the user can modify the header (file
                 is in define mode).
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to allow the user to define a function to be called
              for each output file so that things can be added to the header. 
              The file will be in define mode and should remain that way.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_output_file_function
   (Loop_Options *loop_options,
    VoxelOutputFileFunction output_file_function)
{
   loop_options->output_file_function = output_file_function;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_copy_all_header
@INPUT      : loop_options - user options for looping
              copy_all_header - TRUE if all header info should be copied
                 to output file
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn copying of all header info off.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 13, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_copy_all_header(Loop_Options *loop_options, 
                                     int copy_all_header)
{
   loop_options->copy_all_header_info = copy_all_header;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_accumulate
@INPUT      : loop_options - user options for looping
              do_accumulation - TRUE if accumulating should be done,
                 FALSE otherwise.
              num_extra_buffers - number of extra buffers to allocate.
              start_function - function to be called before looping with 
                 all output and extra buffers as arguments. NULL means
                 don't call any function.
              finish_function - function to be called after looping with
                 all output and extra buffers as arguments. NULL means
                 don't call any function.
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to turn allow an accumulation mode of looping. Instead
              of loading all input files and then calling the voxel routine,
              the voxel routine is called after for each input file. The
              user can provide a start_function that is called to initialize
              the output buffers and a finish_function that is called to
              finish calculations. The user can also ask for extra buffers.
              These are treated like output buffers (at the end of the list)
              but are not written out.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : December 6, 1994 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void set_loop_accumulate(Loop_Options *loop_options, 
                                int do_accumulation,
                                int num_extra_buffers,
                                VoxelStartFunction start_function,
                                VoxelFinishFunction finish_function)
{
   loop_options->do_accumulate = do_accumulation;

   /* Turning off accumulation */
   if (!do_accumulation) {
      loop_options->num_extra_buffers = 0;
      loop_options->start_function = NULL;
      loop_options->finish_function = NULL;
   }

   /* Turning on accumulation */
   else {
      if (num_extra_buffers < 0) {
         (void) fprintf(stderr, 
                        "Bad num_extra_buffers %d in set_loop_accumulate\n",
                        num_extra_buffers);
         exit(EXIT_FAILURE);
      }
      loop_options->do_accumulate = TRUE;
      loop_options->num_extra_buffers = num_extra_buffers;
      loop_options->start_function = start_function;
      loop_options->finish_function = finish_function;
   }
   
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_loop_allocate_buffer_function
@INPUT      : loop_options - user options for looping
              allocate_buffer_function - user function that allocates and frees
                 input_data
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to set the function that allocates and frees input_data
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug 29, 2000 (J. Taylor)
@MODIFIED   : Sept. 19, 2000 (P. Neelin)
---------------------------------------------------------------------------- */
MNCAPI void set_loop_allocate_buffer_function(Loop_Options *loop_options, 
                         AllocateBufferFunction allocate_buffer_function)

{
   loop_options->allocate_buffer_function = allocate_buffer_function;
}

/* ------------ Routines to set and get loop info ------------ */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : create_loop_info
@INPUT      : (none)
@OUTPUT     : (none)
@RETURNS    : Pointer to Loop_Info structure
@DESCRIPTION: Routine to create and initialize the loop info structure.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE Loop_Info *create_loop_info(void)
{
   Loop_Info *loop_info;

   /* Allocate structure */
   loop_info = MALLOC(1, Loop_Info);

   /* Fill in the defaults */
   initialize_loop_info(loop_info);

   /* Return the structure pointer */
   return loop_info;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : initialize_loop_info
@INPUT      : loop_info - pointer to Loop_Info structure
@OUTPUT     : (none)
@RETURNS    : (nothing)
@DESCRIPTION: Routine to initialize the loop info structure.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : February 28, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void initialize_loop_info(Loop_Info *loop_info)
{
   int idim;

   /* Fill in the defaults */
   loop_info->current_file = 0;
   loop_info->current_index = 0;
   for (idim=0; idim < MAX_VAR_DIMS; idim++) {
      loop_info->start[idim] = 0;
      loop_info->count[idim] = 0;
   }
   loop_info->loopfile_info = NULL;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_loop_info
@INPUT      : loop_info - pointer to structure to cleanup
@OUTPUT     : (none)
@RETURNS    : (none)
@DESCRIPTION: Routine to cleanup and free the Loop_Info structure
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void free_loop_info(Loop_Info *loop_info)
{
   FREE(loop_info);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_info_shape
@INPUT      : loop_info - info structure pointer
              start - start of hyperslab
              count - count of hyperslab
@OUTPUT     : (none)
@RETURNS    : (none)
@DESCRIPTION: Routine to set the hyperslab shape (start and count)
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void set_info_shape(Loop_Info *loop_info, long start[], long count[])
{
   int idim;
   long size;

   /* Save the shape of the current chunk */
   for (idim=0; idim < MAX_VAR_DIMS; idim++) {
      loop_info->start[idim] = start[idim];
      loop_info->count[idim] = count[idim];
   }

   /* Figure out how many voxels will be skipped by a step of one 
      in each dimension */
   loop_info->dimvoxels[MAX_VAR_DIMS-1] = 1;
   for (idim=MAX_VAR_DIMS-2; idim >= 0; idim--) {
      size = loop_info->count[idim+1];
      if (size <= 0) size = 1;
      loop_info->dimvoxels[idim] = size * loop_info->dimvoxels[idim+1];
   }

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_info_shape
@INPUT      : loop_info - info structure pointer
              ndims - number of dimensions to copy (uses MAX_VAR_DIMS
                 if ndims == 0)
@OUTPUT     : start - start of hyperslab
              count - count of hyperslab
@RETURNS    : (none)
@DESCRIPTION: Routine to get the current hyperslab shape (start and count)
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void get_info_shape(Loop_Info *loop_info, int ndims,
                           long start[], long count[])
{
   int idim;

   if ((ndims <= 0) || (ndims > MAX_VAR_DIMS))
      ndims = MAX_VAR_DIMS;
   for (idim=0; idim < ndims; idim++) {
      start[idim] = loop_info->start[idim];
      count[idim] = loop_info->count[idim];
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_info_voxel_index
@INPUT      : loop_info - info structure pointer
              subscript - 1-D voxel index into data buffers of voxel function
              ndims - number of dimensions in index array (uses MAX_VAR_DIMS
                 if ndims == 0)
@OUTPUT     : index - multi-dimensional voxel index into file
@RETURNS    : (none)
@DESCRIPTION: Routine to get the current voxel index.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : November 28, 2001 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI void get_info_voxel_index(Loop_Info *loop_info, long subscript, 
                                 int ndims, long index[])
{
   int idim;
   long this_index;

   /* Check the array size */
   if ((ndims <= 0) || (ndims > MAX_VAR_DIMS))
      ndims = MAX_VAR_DIMS;

   /* Convert the 1-D subscript into a multi-dim index and add it to
      the start index of the chunk */
   for (idim=0; idim < ndims; idim++) {
      this_index = subscript / loop_info->dimvoxels[idim];
      index[idim] = loop_info->start[idim] + this_index;
      subscript -= this_index * loop_info->dimvoxels[idim];
   }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_info_current_file
@INPUT      : loop_info - info structure pointer
              current_file - number of current input file (for 
                 accumulation)
@OUTPUT     : (none)
@RETURNS    : (none)
@DESCRIPTION: Routine to set the current input file number.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : February 28, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void set_info_current_file(Loop_Info *loop_info, int current_file)
{

   loop_info->current_file = current_file;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_info_current_file
@INPUT      : loop_info - info structure pointer
@OUTPUT     : (none)
@RETURNS    : Number of current input file (for accumulating over files)
@DESCRIPTION: Routine to get the current input file number.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : February 28, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int get_info_current_file(Loop_Info *loop_info)
{

   return loop_info->current_file;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_info_current_mincid
@INPUT      : loop_info - info structure pointer
@OUTPUT     : (none)
@RETURNS    : Minc id for current input file (for accumulating over files)
@DESCRIPTION: Routine to get the current input file mincid.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 8, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int get_info_current_mincid(Loop_Info *loop_info)
{

   if (loop_info->loopfile_info == NULL) return MI_ERROR;
   return get_input_mincid(loop_info->loopfile_info, loop_info->current_file);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_info_current_index
@INPUT      : loop_info - info structure pointer
              current_index - number of current dimension index (for 
                 accumulation over files and dimension)
@OUTPUT     : (none)
@RETURNS    : (none)
@DESCRIPTION: Routine to set the current dimension index.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : February 28, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void set_info_current_index(Loop_Info *loop_info, int current_index)
{

   loop_info->current_index = current_index;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_info_current_index
@INPUT      : loop_info - info structure pointer
@OUTPUT     : (none)
@RETURNS    : Number of current dimension index (for accumulating over files)
@DESCRIPTION: Routine to get the current dimension index.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : February 28, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int get_info_current_index(Loop_Info *loop_info)
{

   return loop_info->current_index;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_info_loopfile_info
@INPUT      : loop_info - info structure pointer
              loopfile_info - loopfile info structure pointer
@OUTPUT     : (none)
@RETURNS    : (none)
@DESCRIPTION: Routine to set the loopfile info structure for future queries
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 7, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE void set_info_loopfile_info(Loop_Info *loop_info, 
                                    Loopfile_Info *loopfile_info)
{

   loop_info->loopfile_info = loopfile_info;

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_info_whole_file
@INPUT      : loop_info - info structure pointer
@OUTPUT     : (none)
@RETURNS    : Id of current minc file
@DESCRIPTION: Routine to change minc file handling to get the whole input 
              file, not just the header (should be called from within the 
              input_file_function).
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 7, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int get_info_whole_file(Loop_Info *loop_info)
{
   Loopfile_Info *loopfile_info;

   /* Check for NULL pointer */
   if (loop_info->loopfile_info == NULL) return MI_ERROR;
   loopfile_info = loop_info->loopfile_info;

   /* Make the input non-sequential (hold the files open if possible), and
      ask for whole files */
   set_input_sequential(loopfile_info, FALSE);
   set_input_headers_only(loopfile_info, FALSE);

   /* Return the current minc file id */
   if (loop_info->current_file >= 0)
      return get_input_mincid(loopfile_info, loop_info->current_file);
   else
      return MI_ERROR;

}

/* kate: indent-mode cstyle; indent-width 3; replace-tabs on; */
