/* ----------------------------- MNI Header -----------------------------------
@NAME       : netcdf_convenience.c
@DESCRIPTION: File of convenience functions for netcdf routines. There
              is nothing MINC specific about these routines, they just
              provide convenient ways of getting netcdf data. The
              routines MI_convert_type, mivarget and mivarget1 break this
              rule by making use of the MINC variable attribute MIsigntype
              to determine the sign of an integer variable.
@METHOD     : Routines included in this file :
              public :
                 miexpand_file
                 miopen
                 micreate
                 miclose
                 miattget
                 miattget1
                 miattgetstr
                 miattputint
                 miattputdbl
                 miattputstr
                 mivarget
                 mivarget1
                 mivarput
                 mivarput1
                 miset_coords
                 mitranslate_coords
                 micopy_all_atts
                 micopy_var_def
                 micopy_var_values
                 micopy_all_var_defs
                 micopy_all_var_values
                 micreate_tempfile
                 miget_cfg_bool
                 miget_cfg_int
                 miget_cfg_str
              private :
                 execute_decompress_command
                 MI_vcopy_action
@CREATED    : July 27, 1992. (Peter Neelin, Montreal Neurological Institute)
@MODIFIED   : 
 * $Log: netcdf_convenience.c,v $
 * Revision 6.21  2008-01-17 02:33:02  rotor
 *  * removed all rcsids
 *  * removed a bunch of ^L's that somehow crept in
 *  * removed old (and outdated) BUGS file
 *
 * Revision 6.20  2008/01/12 19:08:14  stever
 * Add __attribute__ ((unused)) to all rcsid variables.
 *
 * Revision 6.19  2005/08/26 21:04:58  bert
 * Use #if rather than #ifdef with MINC2 symbol
 *
 * Revision 6.18  2005/05/20 16:49:51  bert
 * Avoid direct usage of H5Fis_hdf5(), replace with hdf_access() function
 *
 * Revision 6.17  2005/05/20 15:39:45  bert
 * Remove and/or conditionalize test code for memory-mapped files (see HDF5_MMAP_TEST)
 *
 * Revision 6.16  2004/12/14 23:53:46  bert
 * Get rid of compilation warnings
 *
 * Revision 6.15  2004/12/03 21:52:35  bert
 * Minor changes for Windows build
 *
 * Revision 6.14  2004/10/15 13:48:33  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.13  2004/06/04 18:16:25  bert
 * Create and add an 'ident' attribute when a file is created
 *
 * Revision 6.12  2004/04/30 18:57:39  bert
 * Explicitly cast return values of NULL to char * where appropriate to make IRIX MIPSpro compiler happy
 *
 * Revision 6.11  2004/04/27 15:50:41  bert
 * The full 2.0 treatment
 *
 * Revision 6.10  2004/03/23 21:16:05  bert
 * Conditionally include fcntl.h
 *
 * Revision 6.9  2003/12/01 22:45:58  stever
 * Check for fork(); use for file decompression if available
 *
 * Revision 6.8  2003/03/17 16:15:33  bert
 * Added micreate_tempfile() to resolve issues with tempfile naming and creation, especially to suppress those annoying GNU linker messages about tempnam() and tmpnam().
 *
 * Revision 6.7  2001/08/20 13:19:15  neelin
 * Added function miattget_with_sign to allow the caller to specify the sign
 * of the input attribute since this information is ambiguous. This is
 * necessary for the valid_range attribute which should have the same sign
 * as the image data. Modified miget_valid_range to make use of this function.
 *
 * Revision 6.6  2001/04/24 14:49:39  neelin
 * In execute_decompress_command, close all file handles in child after
 * fork to avoid problems with buffer flushing.
 *
 * Revision 6.5  2001/04/17 18:40:14  neelin
 * Modifications to work with NetCDF 3.x
 * In particular, changed NC_LONG to NC_INT (and corresponding longs to ints).
 * Changed NC_UNSPECIFIED to NC_NAT.
 * A few fixes to the configure script.
 *
 * Revision 6.4  2000/09/13 14:02:00  neelin
 * Added support for bzip files. (Modified patch from Steve Robbins)
 *
 * Revision 6.3  2000/02/02 18:43:29  neelin
 * Fixed bug in miexpand_file that would call fclose with a NULL file handle.
 * For newer versions of glibc, this would cause a seg fault.
 *
 * Revision 6.2  1999/10/19 14:45:11  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.1  1997/10/06 12:54:08  neelin
 * Changed call to tmpnam to tempnam so that TMPDIR variable is checked when
 * creating temporary files.
 *
 * Revision 6.0  1997/09/12  13:24:54  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:25:53  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:07:52  neelin
 * Release of minc version 0.4
 *
 * Revision 3.3  1997/04/10  19:22:18  neelin
 * Removed redefinition of NULL and added pointer casts in appropriate places.
 *
 * Revision 3.2  1995/09/29  14:34:09  neelin
 * Modified micopy_all_atts to handle MI_ERROR being passed in as a varid.
 *
 * Revision 3.1  1995/06/12  20:43:52  neelin
 * Modified miexpand_file and miopen to try adding compression exetensions
 * to filenames if the first open fails.
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.6  1995/03/14  14:36:35  neelin
 * Got rid of broken pipe messages from miexpand_file by using exec
 * in system call.
 *
 * Revision 2.5  1995/02/08  19:14:44  neelin
 * More changes for irix 5 lint.
 *
 * Revision 2.4  1995/02/08  19:01:06  neelin
 * Moved private function declarations from minc_routines.h to appropriate file.
 *
 * Revision 2.3  1995/01/24  08:34:11  neelin
 * Added optional tempfile argument to miexpand_file.
 *
 * Revision 2.2  95/01/23  08:28:19  neelin
 * Changed name of midecompress_file to miexpand_file.
 * 
 * Revision 2.1  95/01/20  15:20:33  neelin
 * Added midecompress_file with ability to decompress only the header of a file.
 * 
 * Revision 2.0  94/09/28  10:38:13  neelin
 * Release of minc version 0.2
 * 
 * Revision 1.11  94/09/28  10:37:19  neelin
 * Pre-release
 * 
 * Revision 1.10  93/11/03  12:28:04  neelin
 * Added miopen, micreate, miclose routines.
 * miopen will uncompress files before opening them, if needed.
 * 
 * Revision 1.9  93/08/11  12:06:28  neelin
 * Added RCS logging in source.
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
#include "ParseArgv.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if HAVE_SYS_STAT_H
#include <sys/stat.h>           /* For S_IREAD, S_IWRITE */
#endif

#include <ctype.h>

#if MINC2
#undef ncopen
#undef ncclose
#undef nccreate
#include "hdf_convenience.h"
#endif /* MINC2 defined */

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

/* Private functions */
PRIVATE int MI_vcopy_action(int ndims, long start[], long count[], 
                            long nvalues, void *var_buffer, void *caller_data);


#if MINC2
/* These flags are used to count miopen() calls which open either an HDF5
 * file or a NetCDF file.  The exact count is not important; the library
 * will automatically choose to create a HDF5 output file if only HDF5
 * files have been opened.
 */
static int mi_nc_files = 0;
static int mi_h5_files = 0;

#endif /* MINC2 defined */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : execute_decompress_command
@INPUT      : command - command to execute
              infile - input file
              outfile - output file
              header_only - ignored
@OUTPUT     : (none)
@RETURNS    : status of decompress command (zero = success)
@DESCRIPTION: Routine to execute a decompression command on a minc file.
              The command must take a file name argument and must send 
              to standard output.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines, external decompression programs
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int execute_decompress_command(char *command, const char *infile,
                                       char *outfile, int header_only)
{
   char whole_command[1024];
   int status;


#if !(HAVE_WORKING_FORK && HAVE_SYSTEM && HAVE_POPEN)
   fprintf(stderr,"Can't decompress %s because system is not available!\n",infile);
   
   return 1;

#else      /* Unix */

   /* we now ignore header_only and always uncompress the whole
    * file as the previous "header only" hack that used to work
    * on MINC1 files doesn't work reliably with MINC2 */
   (void) sprintf(whole_command, "exec %s %s > %s 2> /dev/null", 
                  command, infile, outfile);
   status = system(whole_command);

   /* Return the status */
   return status;

#endif         /* ifndef unix else */
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miexpand_file
@INPUT      : path  - name of file to open.
              tempfile - user supplied name for temporary file. If 
                 NULL, then the routine generates its own name.
              header_only - TRUE if only the header needs to be expanded.
@OUTPUT     : created_tempfile - TRUE if a temporary file was created, FALSE
                 if no file was created (either because the original file
                 was not compressed or because of an error).
@RETURNS    : name of uncompressed file (either original or a temporary file)
              or NULL if an error occurred during decompression. The caller 
              must free the string. If a system error occurs on file open or 
              the decompression type is unknown, then the original file name
              is returned.
@DESCRIPTION: Routine to expand a compressed minc file. If the original file 
              is not compressed then its name is returned. If the name of a 
              temporary file is returned, then *created_tempfile is set to
              TRUE. If header_only is TRUE, then only the header part of the 
              file will be expanded - the data part may or may not be present.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines, external decompression programs
@CREATED    : January 20, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI char *miexpand_file(const char *path, char *tempfile, int header_only,
                           int *created_tempfile)
{
   typedef enum 
      {BZIPPED, GZIPPED, COMPRESSED, PACKED, ZIPPED, UNKNOWN} Compress_type;
   int status, oldncopts, first_ncerr, iext;
   char *newfile, *compfile;
   const char *extension;
   FILE *fp;
   Compress_type compress_type;
   static struct {
      char *extension;
      Compress_type type;
   } compression_code_list[] = {
      {".bz", BZIPPED},
      {".bz2", BZIPPED},
      {".gz", GZIPPED},
      {".Z", COMPRESSED},
      {".z", PACKED},
      {".zip", ZIPPED}
   };
   static int complist_length = 
      sizeof(compression_code_list) / sizeof(compression_code_list[0]);
   static int max_compression_code_length = 5;

   MI_SAVE_ROUTINE_NAME("miexpand_file");

   /* We have not created a temporary file yet */
   *created_tempfile = FALSE;

#if MINC2
   if (hdf_access(path)) {
      newfile = strdup(path);
      MI_RETURN(newfile);
   }
#endif /* MINC2 defined */

   /* Try to open the file (close it again immediately) */
   oldncopts =get_ncopts(); set_ncopts(0);
   status = ncopen(path, NC_NOWRITE);
   if (status != MI_ERROR) {
      (void) ncclose(status);
   }
   set_ncopts(oldncopts);

   /* If there is no error then return the original file name */
   if (status != MI_ERROR) {
      newfile = strdup(path);
      MI_RETURN(newfile);
   }

   /* Save the error code */
   first_ncerr = ncerr;

   /* Check for the system error that doesn't show */
   if (first_ncerr == NC_NOERR) {
      fp = fopen(path, "r");
      if (fp == NULL) {
         first_ncerr = NC_SYSERR;
      }
      else {
         (void) fclose(fp);
      }
   }

   /* Get the file extension */
   extension = strrchr(path, '.');
   if (extension == NULL) {
      extension = &path[strlen(path)];
   }

   /* Determine the type */
   compress_type = UNKNOWN;
   for (iext = 0; iext < complist_length; iext++) {
      if (STRINGS_EQUAL(extension, compression_code_list[iext].extension)) {
         compress_type = compression_code_list[iext].type;
         break;
      }
   }

   /* If there was a system error and it's not already a compressed file, 
      then maybe there exists a compressed version (with appropriate 
      extension). Loop through the list of extensions. */
   compfile = NULL;
   if ((first_ncerr == NC_SYSERR) && (compress_type == UNKNOWN)) {
      compfile = MALLOC(strlen(path) + max_compression_code_length + 2, char);
      for (iext=0; iext < complist_length; iext++) {
         (void) strcat(strcpy(compfile, path), 
                       compression_code_list[iext].extension);
         fp = fopen(compfile, "r");
         if (fp != NULL) {
            (void) fclose(fp);
            break;
         }
      }
      if (iext >= complist_length) {
         FREE(compfile);
         newfile = strdup(path);
         MI_RETURN(newfile);
      }
      compress_type = compression_code_list[iext].type;
      path = compfile;
   }

   /* If there was a system error or we don't know what to do 
      with the file, then return the original file name */
   else if ((first_ncerr == NC_SYSERR) || (compress_type == UNKNOWN)) {
      newfile = strdup(path);
      MI_RETURN(newfile);
   }

   /* Create a temporary file name */
   if (tempfile == NULL) {
      newfile = micreate_tempfile();
   }
   else {
      newfile = strdup(tempfile);
   }
   *created_tempfile = TRUE;

   /* Try to use gunzip */
   if ((compress_type == GZIPPED) || 
       (compress_type == COMPRESSED) ||
       (compress_type == PACKED) ||
       (compress_type == ZIPPED)) {
      status = execute_decompress_command("gunzip -c", path, newfile, 
                                          header_only);
   }
   else if (compress_type == BZIPPED) {
      status = execute_decompress_command("bunzip2 -c", path, newfile, 
                                          header_only);
   }

   /* If that doesn't work, try something else */
   if (status != 0) {
      if (compress_type == COMPRESSED) {
         status = execute_decompress_command("zcat", path, newfile, 
                                             header_only);
      }
      else if (compress_type == PACKED) {
         status = execute_decompress_command("pcat", path, newfile, 
                                             header_only);
      }
   }

   /* Free the compressed file name, if necessary */
   if (compfile != NULL) {
      FREE(compfile);
      path = NULL;
   }

   /* Check for failure to uncompress the file */
   if (status != 0) {
      (void) remove(newfile);
      *created_tempfile = FALSE;
      FREE(newfile);
      MI_LOG_ERROR(MI_MSG_UNCMPFAIL);
      MI_RETURN((char *)NULL);  /* Explicit cast needed for MIPSpro cc */
   }

   /* Return the new file name */
   MI_RETURN(newfile);

}

static int
is_netcdf_file(const char *filename)
{
   unsigned char magic[4];
   size_t nread;
   FILE *fp = fopen(filename, "rb");
   if (fp == NULL) {
      return 0;
   }
   nread = fread(magic, 1, 4, fp);
   fclose(fp);
   if (nread != 4) {
      return 0;
   }
   return (magic[0] == 'C' && magic[1] == 'D' && magic[2] == 'F' &&
           (magic[3] == 1 || magic[3] == 2));
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miopen
@INPUT      : path  - name of file to open
              mode  - NC_WRITE or NC_NOWRITE to indicate whether file should
                 be opened for write or read-only.
@OUTPUT     : (nothing)
@RETURNS    : file id or MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine ncopen, but will de-compress (temporarily)
              read-only files as needed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : November 2, 1993 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miopen(const char *path, int mode)
{
   int status, oldncopts, created_tempfile;
   char *tempfile;
#if MINC2
   int hmode;
#endif /* MINC2 defined */

   MI_SAVE_ROUTINE_NAME("miopen");

   if (is_netcdf_file(path)) {
      /* Try to open the file */
      oldncopts =get_ncopts(); set_ncopts(0);
      status = ncopen(path, mode);
      set_ncopts(oldncopts);

      if (status != MI_ERROR) {
         mi_nc_files++;         /* Count open netcdf files */
      }
      MI_RETURN(status);
   }

#if MINC2
   if (mode & NC_WRITE) {
     hmode = H5F_ACC_RDWR;
   } 
   else {
     hmode = H5F_ACC_RDONLY;
   }

   status = hdf_open(path, hmode);

   /* If there is no error then return */
   if (status >= 0) {
      mi_h5_files++;           /* Count open HDF5 files */
      MI_RETURN(status);
   }
#endif /* MINC2 defined */

   /* If the user wants to modify the file then return an error, since
    * we don't allow write access to compressed files.
    */
   if (mode & NC_WRITE) {
      MI_LOG_ERROR(MI_MSG_NOWRITECMP);
      MI_RETURN(MI_ERROR);
   }

   /* Try to expand the file */
   tempfile = miexpand_file(path, NULL, FALSE, &created_tempfile);

   /* Check for error */
   if (tempfile == NULL) {
      MI_RETURN(MI_ERROR);
   }

   /* Open the temporary file and unlink it so that it will disappear when
      the file is closed */
   if (is_netcdf_file(tempfile)) {
      oldncopts =get_ncopts();
      set_ncopts(0);
      status = ncopen(tempfile, mode);
      set_ncopts(oldncopts);
      if (status != MI_ERROR) {
         mi_nc_files++;
      }
   }
#if MINC2
   else {
      status = hdf_open(tempfile, hmode);
      if (status >= 0) {
         mi_h5_files++;
      }
   }
#endif /* MINC2 defined */

   if (created_tempfile) {
      remove(tempfile);
   }
   
   if (status < 0) {
      MI_LOG_ERROR(MI_MSG_OPENFILE, tempfile);
   }
   
   free(tempfile);/*free memory allocated in miexpand_file*/
   
   MI_RETURN(status);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micreate
@INPUT      : path  - name of file to create
              cmode - NC_CLOBBER or NC_NOCLOBBER
@OUTPUT     : (nothing)
@RETURNS    : file id or MI_ERROR (=-1) when an error occurs
@DESCRIPTION: A wrapper for routine nccreate, allowing future enhancements.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : November 2, 1993 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
#if MINC2
MNCAPI int micreatex(const char *path, int cmode, struct mi2opts *opts_ptr)
{
    int fd;

    MI_SAVE_ROUTINE_NAME("micreate");

    if ((cmode & MI2_CREATE_V1) != 0) {
        fd = nccreate(path, cmode);
    }
    else if (miget_cfg_bool(MICFG_FORCE_V2) || (cmode & MI2_CREATE_V2) != 0) {
	fd = hdf_create(path, cmode, opts_ptr);
    }
    else {
        if (mi_nc_files == 0 && mi_h5_files != 0) {
            /* Create an HDF5 file. */
            fd = hdf_create(path, cmode, opts_ptr);
        }
        else {
            /* Create a NetCDF file. */
            fd = nccreate(path, cmode);
        }
    }
    if (fd < 0) {
	MI_LOG_ERROR(MI_MSG_CREATEFILE, path);
    }
    else {
        char ident[128];

        micreate_ident(ident, sizeof(ident));
        miattputstr(fd, NC_GLOBAL, "ident", ident);
        miattputstr(fd, NC_GLOBAL, "minc_version", MINC_VERSION);
    }
    MI_RETURN(fd);
}

MNCAPI int micreate(const char *path, int cmode)
{
    MI_SAVE_ROUTINE_NAME("micreate");

    MI_RETURN(micreatex(path, cmode, NULL));
}

#else

MNCAPI int micreate(char *path, int cmode)
{
    int fd;

    MI_SAVE_ROUTINE_NAME("micreate");

    /* Create a NetCDF file. */
    fd = nccreate(path, cmode);
    if (fd < 0) {
	MI_LOG_ERROR(MI_MSG_CREATEFILE, path);
    }
    MI_RETURN(fd);
}
#endif /* MINC2 not defined */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miclose
@INPUT      : cdfid - id of file to close
@OUTPUT     : (nothing)
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: A wrapper for routine ncclose, allowing future enhancements.
              read-only files as needed.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : November 2, 1993 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miclose(int cdfid)
{
   int status;

   MI_SAVE_ROUTINE_NAME("miclose");

#if MINC2
   if (MI2_ISH5OBJ(cdfid)) {
       status = hdf_close(cdfid);
   }
   else {
       status = ncclose(cdfid);
   }
#else
   status = ncclose(cdfid);
#endif /* MINC2 not defined */

   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_CLOSEFILE);
   }
   MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattget
@INPUT      : cdfid      - cdf file id
              varid      - variable id
              name       - name of attribute
              datatype   - type that calling routine wants (one of the valid
                 netcdf data types, excluding NC_CHAR)
              max_length - maximum length to return (number of elements)
@OUTPUT     : value      - value of attribute
              att_length - actual length of attribute (number of elements)
                 If NULL, then no value is returned.
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine ncattget, but the calling routine specifies
              the form in which data should be returned (datatype), as well
              as the maximum number of elements to get. The datatype can
              only be a numeric type. If the attribute in the file is of type 
              NC_CHAR, then an error is returned. The actual length of the
              vector is returned in att_length.
@METHOD     : 
@GLOBALS    : 
@CALLS      : miattget_with_sign
@CREATED    : July 27, 1992 (Peter Neelin)
@MODIFIED   : August 20, 2001 (P.N.)
                 - changed to call miattget_with_sign
---------------------------------------------------------------------------- */
MNCAPI int miattget(int cdfid, int varid, const char *name, nc_type datatype,
                    int max_length, void *value, int *att_length)
{
    int status;

    MI_SAVE_ROUTINE_NAME("miattget");

    status = miattget_with_sign(cdfid, varid, name, 
				NULL, datatype, NULL, 
				max_length, value, att_length);

    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattget_with_sign
@INPUT      : cdfid      - cdf file id
              varid      - variable id
              name       - name of attribute
              insign     - sign of input attribute. If NULL, then use default.
              datatype   - type that calling routine wants (one of the valid
                 netcdf data types, excluding NC_CHAR)
              outsign    - sign of type for calling routine. If NULL, then use
                 default
              max_length - maximum length to return (number of elements)
@OUTPUT     : value      - value of attribute
              att_length - actual length of attribute (number of elements)
                 If NULL, then no value is returned.
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine miattget, but the calling routine specifies
              the sign of the attribute in the file (which may be ambiguous)
              as well as the sign of the return type. Sign strings can be
              MI_SIGNED, MI_UNSIGNED, an empty string or NULL - the latter
              two mean use the default for the type.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines and MI_convert_type
@CREATED    : August 20, 2001 (Peter Neelin)
                 - slightly modified version of old miattget
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miattget_with_sign(int cdfid, int varid, const char *name, 
                              char *insign, nc_type datatype, char *outsign,
                              int max_length, void *value, int *att_length)
{
   nc_type att_type;          /* Type of attribute */
   int actual_length;         /* Actual length of attribute */
   void *att_value;           /* Pointer to attribute value */
   int status;                /* Status of nc routine */
   int att_sign, data_sign;   /* Integer sign values */

   MI_SAVE_ROUTINE_NAME("miattget_with_sign");

   /* Inquire about the attribute */
   status = ncattinq(cdfid, varid, name, &att_type, &actual_length);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_FINDATTR, name);
       MI_RETURN(MI_ERROR);
   }

   /* Save the actual length of the attribute */
   if (att_length != NULL)
      *att_length = actual_length;

   /* Check that the attribute type is numeric */
   if ((datatype==NC_CHAR) || (att_type==NC_CHAR)) {
      MI_LOG_ERROR(MI_MSG_ATTRNOTNUM, name);
      MI_RETURN(MI_ERROR);
   }

   /* Check to see if the type requested is the same as the attribute type
      and that the length is less than or equal to max_length. If it is, just 
      get the value. */
   if ((datatype == att_type) && (actual_length <= max_length)) {
       status = ncattget(cdfid, varid, name, value);
       if (status < 0) {
           MI_LOG_ERROR(MI_MSG_READATTR, name);
       }
       MI_RETURN(status);
   }

   /* Otherwise, get space for the attribute */
   if ((att_value = MALLOC(actual_length * nctypelen(att_type), char))
                      == NULL) {
       MI_LOG_ERROR(MI_MSG_NOMEMATTR, name);
       MI_RETURN(MI_ERROR);
   }

   /* Get the attribute */
   if (ncattget(cdfid, varid, name, att_value)==MI_ERROR) {
      FREE(att_value);
      MI_LOG_ERROR(MI_MSG_READATTR, name);
      MI_RETURN(MI_ERROR);
   }

   /* Get the signs */
   att_sign = MI_get_sign_from_string(att_type, insign);
   data_sign = MI_get_sign_from_string(datatype, outsign);

   /* Get the values.
      Call MI_convert_type with :
         MI_convert_type(number_of_values,
                         intype,  insign,  invalues,
                         outtype, outsign, outvalues,
                         icvp) */
   status=MI_convert_type(MIN(max_length, actual_length), 
                          att_type, att_sign, att_value,
                          datatype, data_sign, value,
                          NULL);
   FREE(att_value);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_CONVATTR, name);
   }
   MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattget1
@INPUT      : cdfid      - cdf file id
              varid      - variable id
              name       - name of attribute
              datatype   - type that calling routine wants (one of the valid
                 netcdf data types, excluding NC_CHAR)
@OUTPUT     : value      - value of attribute
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine miattget, but the its gets only one value
              of an attribute. If the attribute is longer, then an error
              occurs.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines and miattget
@CREATED    : July 27, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miattget1(int cdfid, int varid, const char *name, nc_type datatype,
                    void *value)
{
   int att_length;      /* Actual length of the attribute */
   int status;

   MI_SAVE_ROUTINE_NAME("miattget1");

   /* Get the attribute value and its actual length */
   status = miattget(cdfid, varid, name, datatype, 1, value, &att_length);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_FINDATTR, name);
       MI_RETURN(MI_ERROR);
   }

   /* Check that the length is 1 */
   if (att_length != 1) {
      MI_LOG_ERROR(MI_MSG_ATTRNOTSCALAR, name);
      MI_RETURN(MI_ERROR);
   }

   MI_RETURN(MI_NOERROR);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattgetstr
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              name     - name of attribute
              maxlen   - maximum length of string to be copied
@OUTPUT     : value    - string returned
@RETURNS    : pointer to string value or NULL if an error occurred.
@DESCRIPTION: Gets a character attribute, copying up to maxlen characters
              into value and adding a terminating '\0' if necessary. A
              pointer to the string is returned to facilitate use.
              If the attribute is non-character, a NULL pointer is returned.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : July 28, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI char *miattgetstr(int cdfid, int varid, const char *name,
                         int maxlen, char *value)
{
   nc_type att_type;          /* Type of attribute */
   int att_length;            /* Length of attribute */
   char *att_value;           /* Pointer to attribute value */

   MI_SAVE_ROUTINE_NAME("miattgetstr");

   /* Inquire about the attribute */
   if (ncattinq(cdfid, varid, name, &att_type, &att_length)==MI_ERROR) {
       MI_LOG_ERROR(MI_MSG_FINDATTR, name);
       MI_RETURN((char *)NULL); /* Explicit cast for MIPSpro cc */
   }

   /* Check that the attribute type is character */
   if (att_type!=NC_CHAR) {
       MI_LOG_ERROR(MI_MSG_ATTRNOTSTR, name);
       MI_RETURN((char *)NULL); /* Explicit cast for MIPSpro cc */
   }

   /* Check to see if the attribute length is less than maxlen. 
      If it is, just get the value. */
   if (att_length <= maxlen) {
      if (ncattget(cdfid, varid, name, value) == MI_ERROR) {
	  MI_LOG_ERROR(MI_MSG_READATTR, name);
	  MI_RETURN((char *)NULL); /* Explicit cast for MIPSpro cc */
      }
      /* Check the last character for a '\0' */
      if (value[att_length-1] != '\0') {
         if (att_length==maxlen)
            value[att_length-1] = '\0';
         else
            value[att_length]   = '\0';
      }
      MI_RETURN(value);
   }

   /* Otherwise, get space for the attribute */
   if ((att_value = MALLOC(att_length * nctypelen(att_type), char)) ==NULL) {
       MI_LOG_ERROR(MI_MSG_NOMEMATTR, name);
       MI_RETURN((char *)NULL); /* Explicit cast for MIPSpro cc */
   }

   /* Get the attribute */
   if (ncattget(cdfid, varid, name, att_value)==MI_ERROR) {
      FREE(att_value);
      MI_LOG_ERROR(MI_MSG_READATTR, name);
      MI_RETURN((char *)NULL);
   }

   /* Copy the attribute */
   (void) strncpy(value, att_value, (size_t) maxlen-1);
   value[maxlen-1] = '\0';

   /* Free the string */
   FREE(att_value);

   /* Return a pointer to the string */
   MI_RETURN(value);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattputint
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              name     - name of attribute
              value    - integer value for attribute
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs
@DESCRIPTION: Convenience routine for calling ncattput with integers.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : November 25, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miattputint(int cdfid, int varid, const char *name, int value)
{
    int lvalue;
    int status;

    MI_SAVE_ROUTINE_NAME("miattputint");

    lvalue = value;
    status = ncattput(cdfid, varid, name, NC_INT, 1, &lvalue);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_WRITEATTR, name);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattputdbl
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              name     - name of attribute
              value    - double value for attribute
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs
@DESCRIPTION: Convenience routine for calling ncattput with doubles.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : August 5, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miattputdbl(int cdfid, int varid, const char *name, double value)
{
    int status;
    MI_SAVE_ROUTINE_NAME("miattputdbl");
    status = ncattput(cdfid, varid, name, NC_DOUBLE, 1, &value);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_WRITEATTR, name);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miattputstr
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              name     - name of attribute
              value    - string value for attribute
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs
@DESCRIPTION: Convenience routine for calling ncattput with character strings.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : July 28, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miattputstr(int cdfid, int varid, const char *name, const char *value)
{
    int status;
    MI_SAVE_ROUTINE_NAME("miattputstr");

    status = ncattput(cdfid, varid, name, NC_CHAR, 
                       strlen(value) + 1, value);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_WRITEATTR, name);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mivarget
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              start    - vector of coordinates of corner of hyperslab
              count    - vector of edge lengths of hyperslab
              datatype - type that calling routine wants (one of the valid
                 netcdf data types, excluding NC_CHAR)
              sign     - sign that calling routine wants (one of
                 EMPTY_STRING (or NULL) = use default sign
                 MI_SIGNED              = signed values
                 MI_UNSIGNED            = unsigned values
@OUTPUT     : values   - value of variable
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine ncvarget, but the calling routine specifies
              the form in which data should be returned (datatype), as well
              as the sign. The datatype can only be a numeric type. If the 
              variable in the file is of type NC_CHAR, then an error is 
              returned.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int mivarget(int cdfid, int varid, long start[], long count[],
                    nc_type datatype, const char *sign, void *values)
{
    int status;
    MI_SAVE_ROUTINE_NAME("mivarget");

    status = MI_varaccess(MI_PRIV_GET, cdfid, varid, start, count,
			  datatype, MI_get_sign_from_string(datatype, sign),
			  values, NULL, NULL);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_READVAR, varid);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mivarget1
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              mindex   - vector of coordinates of value to get
              datatype - type that calling routine wants (one of the valid
                 netcdf data types, excluding NC_CHAR)
              sign     - sign that calling routine wants (one of
                 EMPTY_STRING (or NULL) = use default sign
                 MI_SIGNED              = signed values
                 MI_UNSIGNED            = unsigned values
@OUTPUT     : value    - value of variable
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine ncvarget1, but the calling routine specifies
              the form in which data should be returned (datatype), as well
              as the sign. The datatype can only be a numeric type. If the 
              variable in the file is of type NC_CHAR, then an error is 
              returned.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int mivarget1(int cdfid, int varid, long mindex[],
                     nc_type datatype, const char *sign, void *value)
{
    int status;
    long count[MAX_VAR_DIMS];

    MI_SAVE_ROUTINE_NAME("mivarget1");
    
    status = MI_varaccess(MI_PRIV_GET, cdfid, varid, mindex, 
			  miset_coords(MAX_VAR_DIMS, 1L, count),
			  datatype, MI_get_sign_from_string(datatype, sign),
			  value, NULL, NULL);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_READVAR, varid);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mivarput
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              start    - vector of coordinates of corner of hyperslab
              count    - vector of edge lengths of hyperslab
              datatype - type that calling routine is passing (one of the valid
                 netcdf data types, excluding NC_CHAR)
              sign     - sign that calling routine is passing (one of
                 EMPTY_STRING (or NULL) = use default sign
                 MI_SIGNED              = signed values
                 MI_UNSIGNED            = unsigned values
              values   - value of variable
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine ncvarput, but the calling routine specifies
              the form in which data is passes (datatype), as well
              as the sign. The datatype can only be a numeric type. If the 
              variable in the file is of type NC_CHAR, then an error is 
              returned.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int mivarput(int cdfid, int varid, long start[], long count[],
                    nc_type datatype, const char *sign, void *values)
{
    int status;
    MI_SAVE_ROUTINE_NAME("mivarput");

    status = MI_varaccess(MI_PRIV_PUT, cdfid, varid, start, count,
			  datatype, MI_get_sign_from_string(datatype, sign),
			  values, NULL, NULL);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_WRITEVAR, varid);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mivarput1
@INPUT      : cdfid    - cdf file id
              varid    - variable id
              mindex   - vector of coordinates of value to put
              datatype - type that calling routine is passing (one of the valid
                 netcdf data types, excluding NC_CHAR)
              sign     - sign that calling routine is passing (one of
                 EMPTY_STRING (or NULL) = use default sign
                 MI_SIGNED              = signed values
                 MI_UNSIGNED            = unsigned values
@OUTPUT     : value    - value of variable
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Similar to routine ncvarput1, but the calling routine specifies
              the form in which data is passed (datatype), as well
              as the sign. The datatype can only be a numeric type. If the 
              variable in the file is of type NC_CHAR, then an error is 
              returned.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int mivarput1(int cdfid, int varid, long mindex[],
                     nc_type datatype, const char *sign, void *value)
{
    int status;
    long count[MAX_VAR_DIMS];

    MI_SAVE_ROUTINE_NAME("mivarput1");

    status = MI_varaccess(MI_PRIV_PUT, cdfid, varid, mindex, 
			  miset_coords(MAX_VAR_DIMS, 1L, count),
			  datatype, MI_get_sign_from_string(datatype, sign),
			  value, NULL, NULL);
    if (status < 0) {
	MI_LOG_ERROR(MI_MSG_WRITEVAR, varid);
    }
    MI_RETURN(status);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : miset_coords
@INPUT      : nvals  - number of values in coordinate vector to set
              value  - value to which coordinates should be set
@OUTPUT     : coords - coordinate vector
@RETURNS    : pointer to coords.
@DESCRIPTION: Sets nvals entries of the vector coords to value.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI long *miset_coords(int nvals, long value, long coords[])
{
   int i;

   MI_SAVE_ROUTINE_NAME("miset_coords");

   for (i=0; i<nvals; i++) {
      coords[i] = value;
   }
   MI_RETURN(coords);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mitranslate_coords
@INPUT      : cdfid     - cdf file id
              invar     - variable subscripted by incoords
              incoords  - coordinates to be copied
              outvar    - variable subscripted by outcoords
@OUTPUT     : outcoords - new coordinates
@RETURNS    : pointer to outcoords or NULL if an error occurred.
@DESCRIPTION: Translates the coordinate vector used for subscripting one
              variable (invar) to a coordinate vector that can be used to
              subscript another (outvar). This is useful when two variables
              have similar dimensions, but not necessarily the same order of
              dimensions. If invar has a dimension that is not in outvar, then
              the corresponding coordinate is ignored. If outvar has a
              dimension that is not in invar, then the corresponding coordinate
              is not modified.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI long *mitranslate_coords(int cdfid, 
                                int invar,  long incoords[],
                                int outvar, long outcoords[])
{
   int in_ndims, in_dim[MAX_VAR_DIMS], out_ndims, out_dim[MAX_VAR_DIMS];
   int i,j;

   MI_SAVE_ROUTINE_NAME("mitranslate_coords");

   /* Inquire about the dimensions of the two variables */
   if ( (ncvarinq(cdfid, invar,  NULL, NULL, &in_ndims,  in_dim,  NULL)
                    == MI_ERROR) ||
        (ncvarinq(cdfid, outvar, NULL, NULL, &out_ndims, out_dim, NULL)
                    == MI_ERROR)) {
      MI_LOG_ERROR(MI_MSG_FINDVAR, invar);
      MI_RETURN((long *) NULL);
   }

   /* Loop through out_dim, looking for dimensions in in_dim */
   for (i=0; i<out_ndims; i++) {
      for (j=0; j<in_ndims; j++) {
         if (out_dim[i]==in_dim[j]) break;
      }
      /* If we found the dimension, then copy it */
      if (j<in_ndims) {
         outcoords[i] = incoords[j];
      }
   }

   MI_RETURN(outcoords);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micopy_all_atts
@INPUT      : incdfid  - input cdf file id
              invarid  - input variable id
              outcdfid - output cdf file id
              outvarid - output variable id
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs
@DESCRIPTION: Copies all of the attributes of one variable to another.
              Attributes that already exist in outvarid are not copied.
              outcdfid must be in define mode. This routine will only copy
              global attributes to global attributes or variable attributes
              to variable attributes, but not variable to global or global
              to variable. This means that one can safely use
                 micopy_all_atts(inmincid, ncvarid(inmincid, varname),...)
              if outvarid is known to exist (the problem arises because
              MI_ERROR == NC_GLOBAL). No error is flagged if the a bad
              combination is given.

@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : 
@MODIFIED   : 
---------------------------------------------------------------------------- */

MNCAPI int micopy_all_atts(int incdfid, int invarid, 
                           int outcdfid, int outvarid)
{
   int num_atts;             /* Number of attributes */
   char name[MAX_NC_NAME];   /* Name of attribute */
   int oldncopts;            /* Old value of status */
   int status;               /* Status returned by function call */
   int i;

   MI_SAVE_ROUTINE_NAME("micopy_all_atts");


   /* Only allow copying of global attributes to global attributes or
      variable attributes to variable attributes - this makes
      micopy_all_atts(inmincid, ncvarid(inmincid, varname),...) safer, since
      MI_ERROR == NC_GLOBAL */
   if (((invarid == NC_GLOBAL) || (outvarid == NC_GLOBAL)) &&
       (invarid != outvarid)) {
      MI_RETURN(MI_NOERROR);
   }

   /* Inquire about the number of input variable attributes */
   if (invarid != NC_GLOBAL) {
       status = ncvarinq(incdfid, invarid, 
			 NULL, NULL, NULL, NULL, &num_atts);
   }
   else {
       status = ncinquire(incdfid, NULL, NULL, &num_atts, NULL);
   }
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_ATTRCOUNT);
       MI_RETURN(MI_ERROR);
   }

   /* Loop through input attributes */
   for (i=0; i<num_atts; i++){
      
      /* Get the attribute name */
       status = ncattname(incdfid, invarid, i, name);
       if (status < 0) {
	   MI_LOG_ERROR(MI_MSG_ATTRNAME);
	   MI_RETURN(MI_ERROR);
       }

      /* Check to see if it is in the output file. We must set and reset
         ncopts to avoid surprises to the calling program. (This is no
	 longer needed with new MI_SAVE_ROUTINE_NAME macro). */
      oldncopts=get_ncopts(); set_ncopts(0);
      status=ncattinq(outcdfid, outvarid, name, NULL, NULL);
      set_ncopts(oldncopts);

      /* If the attribute does not exist, copy it.
       * Must always copy signtype, if present!
       */
      if (status == MI_ERROR || !strcmp(name,  MIsigntype)) {
	  status = ncattcopy(incdfid, invarid, name, outcdfid, outvarid);
	  if (status < 0) {
	      MI_LOG_ERROR(MI_MSG_COPYATTR, name);
	      MI_RETURN(MI_ERROR);
	  }
      }
   }

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micopy_var_def
@INPUT      : incdfid  - input cdf file id
              invarid  - input variable id
              outcdfid - output cdf file id
@OUTPUT     : (none)
@RETURNS    : Variable id of variable created in outcdfid or MI_ERROR (=-1)
              if an error occurs.
@DESCRIPTION: Copies a variable definition (including attributes) from one 
              cdf file to another. outcdfid must be in define mode.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : 
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int micopy_var_def(int incdfid, int invarid, int outcdfid)
{
   char varname[MAX_NC_NAME]; /* Name of variable */
   char dimname[MAX_NC_NAME]; /* Name of dimension */
   nc_type datatype;          /* Type of variable */
   int ndims;                 /* Number of dimensions of variable */
   int indim[MAX_VAR_DIMS];   /* Dimensions of input variable */
   int outdim[MAX_VAR_DIMS];  /* Dimensions of output variable */
   long insize, outsize;      /* Size of each dimension */
   int recdim;                /* Dimension that is unlimited for input file */
   int outvarid;              /* Id of newly created variable */
   int oldncopts;             /* Store ncopts */
   int i;
   int status;

   MI_SAVE_ROUTINE_NAME("micopy_var_def");

   /* Get name and dimensions of variable */
   status = ncvarinq(incdfid, invarid, varname, &datatype, &ndims, 
                     indim, NULL);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_VARINQ);
       MI_RETURN(MI_ERROR);
   }

   /* Get unlimited dimension for input file */
   status = ncinquire(incdfid, NULL, NULL, NULL, &recdim);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_UNLIMDIM);
       MI_RETURN(MI_ERROR);
   }

   /* Create any new dimensions that need creating */
   for (i=0; i<ndims; i++) {

      /* Get the dimension info */
       status = ncdiminq(incdfid, indim[i], dimname, &insize);
       if (status < 0) {
	   MI_LOG_ERROR(MI_MSG_DIMINQ);
	   MI_RETURN(MI_ERROR);
       }

      /* If it exists in the output file, check the size. */
      oldncopts=get_ncopts(); set_ncopts(0);
      outdim[i] = ncdimid(outcdfid, dimname);
      set_ncopts(oldncopts);
      if (outdim[i] != MI_ERROR) {
	if ((ncdiminq(outcdfid, outdim[i], NULL, &outsize) == MI_ERROR) ||
             ((insize!=0) && (outsize!=0) && (insize != outsize)) ) {
            if ((insize!=0) && (outsize!=0) && (insize != outsize))
		MI_LOG_ERROR(MI_MSG_VARCONFLICT);
            MI_RETURN(MI_ERROR);
         }
      }
      /* Otherwise create it */
      else {
         /* If the dimension is unlimited then try to create it unlimited
            in the output file */
         if (indim[i]==recdim) {
            oldncopts=get_ncopts(); set_ncopts(0);
            outdim[i]=ncdimdef(outcdfid, dimname, NC_UNLIMITED);
            set_ncopts(oldncopts);
         }
         /* If it's not meant to be unlimited, or if we cannot create it
            unlimited, then create it with the current size */
         if ((indim[i]!=recdim) || (outdim[i]==MI_ERROR)) {
	     outdim[i] = ncdimdef(outcdfid, dimname, MAX(1,insize));
	     if (outdim[i] < 0) {
		 MI_LOG_ERROR(MI_MSG_DIMDEF, dimname);
		 MI_RETURN(MI_ERROR);
	     }
         }
      }
   }

   /* Create a new variable */
   outvarid = ncvardef(outcdfid, varname, datatype, ndims, outdim);

   if (outvarid < 0) {
       MI_LOG_ERROR(MI_MSG_VARDEF, varname);
       MI_RETURN(MI_ERROR);
   }
   else {
       /* Copy all the attributes */
       status = micopy_all_atts(incdfid, invarid, outcdfid, outvarid);
       if (status < 0) {
	   MI_RETURN(MI_ERROR);
       }
   }

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : mivarsize
@INPUT      : fd  - file id
              varid  - variable id
@OUTPUT     : size_ptr - dimension sizes
@RETURNS    : MI_ERROR (=-1) if an error occurs.
@DESCRIPTION: Copies the lengths of the variable's dimensions to the
              size_ptr array.  size_ptr must point to a buffer large
              enough to hold the data.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : 
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int
mivarsize(int fd, int varid, long *size_ptr)
{
    int i;
    int ndims;
    int dimids[MAX_VAR_DIMS];

    if (ncvarinq(fd, varid, NULL, NULL, &ndims, dimids, NULL) == MI_ERROR) {
        return (MI_ERROR);
    }

    /* Get the dimension sizes and check for compatibility */
    for (i = 0; i < ndims; i++) {
        if (ncdiminq(fd, dimids[i], NULL, &size_ptr[i]) == MI_ERROR) {
            size_ptr[i] = 0;
        }
    }
    return (MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micopy_var_values
@INPUT      : incdfid  - input cdf file id
              invarid  - input variable id
              outcdfid - output cdf file id
              outvarid - output variable id (usually returned by 
                 micopy_var_def)
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs.
@DESCRIPTION: Copies the values of a variable from one cdf file to another.
              outcdfid must be in data mode. The two variables must have
              the same shape.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : 
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int micopy_var_values(int incdfid, int invarid, 
                             int outcdfid, int outvarid)
{
   nc_type intype, outtype;   /* Data types */
   int inndims, outndims;     /* Number of dimensions */
   long insize[MAX_VAR_DIMS]; /* Dimension sizes */
   long outsize[MAX_VAR_DIMS];
   long start[MAX_VAR_DIMS];  /* First coordinate of variable */
   int indim[MAX_VAR_DIMS];   /* Input dimensions */
   int outdim[MAX_VAR_DIMS];  /* Output dimensions */
   mi_vcopy_type stc;
   int i;
   int status;

   MI_SAVE_ROUTINE_NAME("micopy_var_values");

   /* Get the dimensions of the two variables and check for compatibility */
   if ((ncvarinq(incdfid, invarid, NULL, &intype, &inndims, indim, NULL)
	== MI_ERROR) ||
       (ncvarinq(outcdfid, outvarid, NULL, &outtype, &outndims, outdim, NULL)
	          == MI_ERROR) ||
       (intype != outtype) || (inndims != outndims)) {
       MI_LOG_ERROR(MI_MSG_VARMISMATCH);
       MI_RETURN(MI_ERROR);
   }

   /* Get the dimension sizes and check for compatibility */

   mivarsize(incdfid, invarid, insize);
   mivarsize(outcdfid, outvarid, outsize);
   for (i = 0; i < inndims; i++) {
     if (insize[i] != 0 && outsize[i] != 0 && insize[i] != outsize[i]) {
	 MI_LOG_ERROR(MI_MSG_VARDIFFSIZE);
	 MI_RETURN(MI_ERROR);
     }
   }

   /* Copy the values */
   stc.incdfid =incdfid;
   stc.outcdfid=outcdfid;
   stc.invarid =invarid;
   stc.outvarid=outvarid;
   stc.value_size=nctypelen(intype);
   status = MI_var_loop(inndims, miset_coords(MAX_VAR_DIMS, 0L, start),
			insize, stc.value_size, NULL, 
			MI_MAX_VAR_BUFFER_SIZE, &stc,
			MI_vcopy_action);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_COPYVAR);
   }
   MI_RETURN(status);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_vcopy_action
@INPUT      : ndims       - number of dimensions
              var_start   - coordinate vector of corner of hyperslab
              var_count   - vector of edge lengths of hyperslab
              nvalues     - number of values in hyperslab
              var_buffer  - pointer to variable buffer
              caller_data - pointer to data from micopy_var_values
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Buffer action routine to be called by MI_var_loop, for
              use by micopy_var_values.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : August 3, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_vcopy_action(int ndims, long start[], long count[], 
                            long nvalues, void *var_buffer, void *caller_data)
     /* ARGSUSED */
{
   mi_vcopy_type *ptr;       /* Pointer to data from micopy_var_values */
   int status;

   MI_SAVE_ROUTINE_NAME("MI_vcopy_action");

   ptr=(mi_vcopy_type *) caller_data;

   /* Get values from input variable */
   status = ncvarget(ptr->incdfid, ptr->invarid, start, count, var_buffer);
   if (status < 0) {
       MI_RETURN(MI_ERROR);
   }

   /* Put values to output variable */
   status = ncvarput(ptr->outcdfid, ptr->outvarid, start, count, var_buffer);
   if (status < 0) {
       MI_RETURN(MI_ERROR);
   }
   MI_RETURN(MI_NOERROR);

}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micopy_all_var_defs
@INPUT      : incdfid       - input cdf file id
              outcdfid      - output cdf file id
              nexclude      - number of values in array excluded_vars
              excluded_vars - array of variable id's in incdfid that should
                 not be copied
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs.
@DESCRIPTION: Copies all variable definitions in file incdfid to file outcdfid
              (including attributes), excluding the variable id's listed in
              array excluded_vars. File outcdfid must be in define mode.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : August 3, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int micopy_all_var_defs(int incdfid, int outcdfid, int nexclude,
                               int excluded_vars[])
{
   int num_vars;          /* Number of variables in input file */
   int varid;             /* Variable id counter */
   int i;
   int status;

   MI_SAVE_ROUTINE_NAME("micopy_all_var_defs");

   /* Find out how many variables there are in the file and loop through
      them */
   status = ncinquire(incdfid, NULL, &num_vars, NULL, NULL);
   if (status < 0) {
       MI_RETURN(MI_ERROR);
   }

   /* Loop through variables, copying them */
   for (varid=0; varid<num_vars; varid++) {

      /* Check list of excluded variables */
      for (i=0; i<nexclude; i++) {
         if (varid==excluded_vars[i]) 
	     break;
      }

      /* If the variable is not excluded, copy its definition */
      if (i>=nexclude) {
	  status = micopy_var_def(incdfid, varid, outcdfid);
	  if (status < 0) {
	      MI_RETURN(MI_ERROR);
	  }
      }
   }

   /* Copy global attributes */
   for (i=0; i<nexclude; i++) {
      if (NC_GLOBAL==excluded_vars[i]) break;
   }
   if (i>=nexclude) {
       status = micopy_all_atts(incdfid, NC_GLOBAL, outcdfid, NC_GLOBAL);
   }

   MI_RETURN(status);
       
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micopy_all_var_values
@INPUT      : incdfid       - input cdf file id
              outcdfid      - output cdf file id
              nexclude      - number of values in array excluded_vars
              excluded_vars - array of variable id's in incdfid that should
                 not be copied
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) if an error occurs.
@DESCRIPTION: Copies all variable values in file incdfid to file outcdfid,
              excluding the variable id's listed in array excluded_vars. 
              File outcdfid must be in data mode. Usually called after
              micopy_all_var_defs with the same arguments. If a variable
              to be copied is not defined properly in outcdfid, then an
              error occurs.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines.
@CREATED    : 
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int micopy_all_var_values(int incdfid, int outcdfid, int nexclude,
                                 int excluded_vars[])
{
   int num_vars;           /* Number of variables in input file */
   int varid;              /* Variable id counter */
   int outvarid;           /* Output variable id */
   char name[MAX_NC_NAME]; /*Variable name */
   int i;
   int status;

   MI_SAVE_ROUTINE_NAME("micopy_all_var_values");

   /* Find out how many variables there are in the file and loop through
      them */
   status = ncinquire(incdfid, NULL, &num_vars, NULL, NULL);
   if (status < 0) {
       MI_LOG_ERROR(MI_MSG_VARCOUNT);
       MI_RETURN(MI_ERROR);
   }

   /* Loop through variables, copying them */
   for (varid=0; varid<num_vars; varid++) {

      /* Check list of excluded variables */
      for (i=0; i<nexclude; i++) {
         if (varid==excluded_vars[i]) break;
      }

      /* If the variable is not excluded, copy its values */
      if (i>=nexclude) {
	  /* Get the input variable's name */
	  status = ncvarinq(incdfid, varid, name, NULL, NULL, NULL, NULL);
	  if (status < 0) {
              MI_LOG_ERROR(MI_MSG_VARINQ);
	      MI_RETURN(MI_ERROR);
	  }
	  /* Look for it in the output file */
	  outvarid = ncvarid(outcdfid, name);
	  if (outvarid < 0) {
	      MI_LOG_ERROR(MI_MSG_OUTPUTVAR, name);
	      MI_RETURN(MI_ERROR);
	  }
	  /* Copy the values */
	  status = micopy_var_values(incdfid, varid, outcdfid, outvarid);
	  if (status < 0) {
	      MI_RETURN(MI_ERROR);
	  }
      }
   }

   MI_RETURN(MI_NOERROR);
       
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : micreate_tempfile
@INPUT      : void
@OUTPUT     : (none)
@RETURNS    : Pointer to filename (which must be freed), or NULL if an error
              occurs.
@DESCRIPTION: Creates a temporary file (which is initially CLOSED).
@METHOD     : Unnecessarily convoluted, I suppose... See comments.
@GLOBALS    : 
@CALLS      : Standard POSIX/UNIX routines
@CREATED    : 07-March-2003 by bert@bic.mni.mcgill.ca
@MODIFIED   : 
---------------------------------------------------------------------------- */


MNCAPI char *
micreate_tempfile(void)
{
  int tmp_fd=0;
  char *tmpfile_ptr;

#if defined (HAVE_MKSTEMP)

  /* Best-case scenario (so far...)
   * mkstemp() creates a file immediately, minimizing the race
   * conditions that exist when using the other functions.  These race
   * conditions can lead to small security holes (and large, annoying
   * GNU linker messages).
   *
   * The only catch is that mkstemp() does not automatically put the 
   * file in the TMPDIR directory (or some other appropriate place).
   * So I more-or-less emulate that behavior here.
   */
  const char pat_str[] = "/minc-XXXXXX";
  char *tmpdir_ptr;

  if ((tmpdir_ptr = getenv("TMPDIR")) == NULL) {
    tmpdir_ptr = P_tmpdir;
  }
  tmpfile_ptr = malloc(strlen(tmpdir_ptr) + sizeof (pat_str));
  if (tmpfile_ptr == NULL) {
    return (NULL);
  }
  strcpy(tmpfile_ptr, tmpdir_ptr);
  strcat(tmpfile_ptr, pat_str);
  tmp_fd = mkstemp(tmpfile_ptr); /* Creates the file if possible. */

#elif defined (HAVE_TEMPNAM)

  /* Second-best case.  While not completely avoiding the race condition,
   * this approach should at least have the nice property of putting the
   * tempfile in the right directory (on IRIX and Linux, at least - on
   * some systems tempnam() may not consult the TMPDIR environment variable).
   */
  tmpfile_ptr = tempnam(NULL, "minc-");
  if (tmpfile_ptr == NULL) {
    return (NULL);
  }
  tmp_fd = open(tmpfile_ptr, O_CREAT | O_EXCL | O_RDWR, S_IWRITE | S_IREAD);

#elif defined (HAVE_TMPNAM)
  /* Worst case.  tmpnam() is apparently the worst of all possible worlds
   * here.  It doesn't allow any way to force a particular directory,
   * and it doesn't avoid the race condition.  But volume_io used it for
   * years, so I see no reason to disallow this case for systems that 
   * might not define the above two functions (whether any such systems
   * exist is unclear to me).
   */
  tmpfile_ptr = malloc(L_tmpnam + 1);
  if (tmpfile_ptr == NULL) {
    return (NULL);
  }
  if (tmpnam(tmpfile_ptr) == NULL) {
    free(tmpfile_ptr);
    return (NULL);
  }
  tmp_fd = open(tmpfile_ptr, O_CREAT | O_EXCL | O_RDWR, S_IWRITE | S_IREAD);

#else
#error "System defines neither mkstemp(), tempnam(), nor tmpnam()"
#endif /* Neither HAVE_MKSTEMP, HAVE_TEMPNAM, or HAVE_TMPNAM defined. */

  /* If we get here, tmp_fd should have been opened and the file
   * created.  Now go ahead and close the file.
   */
  if (tmp_fd >= 0) {
    close(tmp_fd);
  }
  else {
    free(tmpfile_ptr);
    tmpfile_ptr = NULL;
  }
  return (tmpfile_ptr);
}


#ifndef NCOPTS_STACK_LIMIT 
 #define NCOPTS_STACK_LIMIT 10
#endif

static int _ncopts_stack[NCOPTS_STACK_LIMIT];
static int _ncopts_stack_pointer=0;
/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_ncopts
@INPUT      : int 
@OUTPUT     : int
@RETURNS    : Old value of ncopts.
@DESCRIPTION: Sets new value of ncopts
@CREATED    : 30-Nov-2016 Vladimir S. FONOV
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int set_ncopts(int new_ncopts)
{
  int old_ncopts=ncopts;
  ncopts=new_ncopts;
  return old_ncopts;
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_ncopts
@INPUT      : None
@OUTPUT     : int
@RETURNS    : Current value of ncopts
@DESCRIPTION: Current value of ncopts
@CREATED    : 30-Nov-2016 Vladimir S. FONOV
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int get_ncopts(void)
{
  return ncopts;
}
/* ----------------------------- MNI Header -----------------------------------
@NAME       : push_ncopts
@INPUT      : int 
@OUTPUT     : int
@RETURNS    : Old value of ncopts.
@DESCRIPTION: Sets new value of ncopts, pushes old value into stack
@CREATED    : 30-Nov-2016 Vladimir S. FONOV
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int push_ncopts(int new_ncopts)
{
  int old_ncopts=ncopts;
  if(_ncopts_stack_pointer>=NCOPTS_STACK_LIMIT)
  {
    MI_LOG_ERROR(MI_MSG_NCOPTS_STACK_OVER);  
  } else {
    _ncopts_stack[_ncopts_stack_pointer]=old_ncopts;
    _ncopts_stack_pointer++;
  }
  ncopts=new_ncopts;
  return old_ncopts;
}
/* ----------------------------- MNI Header -----------------------------------
@NAME       : pop_ncopts
@INPUT      : None
@OUTPUT     : int
@RETURNS    : New value of ncopts.
@DESCRIPTION: Sets new value of ncopts, from stack
@CREATED    : 30-Nov-2016 Vladimir S. FONOV
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int pop_ncopts(void)
{
  if(_ncopts_stack_pointer>0)
  {
    _ncopts_stack_pointer--;
    ncopts=_ncopts_stack[_ncopts_stack_pointer];
  } else {
    MI_LOG_ERROR(MI_MSG_NCOPTS_STACK_UNDER);  
  }
  return ncopts;
}

