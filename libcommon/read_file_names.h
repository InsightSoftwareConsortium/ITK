

/* ----------------------------- MNI Header -----------------------------------
@NAME       : read_file_names
@INPUT      : filelist - name of file from which to read names
@OUTPUT     : num_files - number of files read in
@RETURNS    : Pointer to a NULL-terminated array of file names
@DESCRIPTION: Reads in a list of file names from file filelist or stdin if 
              "-" is specified. Returns NULL if an error occurs. If
              no error occurs, then a pointer to an empty array is 
              returned and num_files is zero.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : March 8, 1995 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
char **read_file_names(char *filelist, int *num_files);
