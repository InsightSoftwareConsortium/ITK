/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_format_convert.c

@COPYRIGHT  : Copyright 2013 Vladimir S. FONOV , McConnell Brain Imaging Centre,
              Copyright 2003 Robert Vincent, McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
---------------------------------------------------------------------------- */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <minc.h>


static int micopy(int old_fd, int new_fd)
{
    /* Copy all variable definitions (and global attributes).
     */
    micopy_all_var_defs(old_fd, new_fd, 0, NULL);
    ncendef(new_fd);
    micopy_all_var_values(old_fd, new_fd, 0, NULL);
    return MI_NOERROR;
}

MNCAPI int minc_format_convert(const char *input,const char *output)
{
    int old_fd;
    int new_fd;
    int flags;
    struct mi2opts opts;
    
    old_fd = miopen(input, NC_NOWRITE);
    if (old_fd < 0) {
        perror(input);
        return MI_ERROR;
    }

    flags = NC_CLOBBER|MI2_CREATE_V2;

    memset(&opts,0,sizeof(struct mi2opts));
    opts.struct_version = MI2_OPTS_V1;

    new_fd = micreatex(output, flags, &opts);
    if (new_fd < 0) {
        perror(output);
        exit MI_ERROR;
    }

    micopy(old_fd, new_fd);

    miclose(old_fd);
    miclose(new_fd);
    
    return MI_NOERROR;
}
