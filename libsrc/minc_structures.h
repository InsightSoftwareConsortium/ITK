#ifndef MINC_STRUCTURES_H
#define MINC_STRUCTURES_H

/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_structures.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Defines structures for use by MINC routines
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : August 28, 1992 (Peter Neelin)
@MODIFIED   : 
 * $Log: minc_structures.h,v $
 * Revision 6.1  1999-10-19 14:45:10  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:24:54  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:25:53  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:07:52  neelin
 * Release of minc version 0.4
 *
 * Revision 3.1  1997/04/10  18:14:50  neelin
 * Fixed handling of invalid data when icv scale is zero.
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:38:09  neelin
 * Release of minc version 0.2
 *
 * Revision 1.10  94/09/28  10:37:30  neelin
 * Pre-release
 * 
 * Revision 1.9  93/08/11  12:06:42  neelin
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
@RCSID      : $Header: /private-cvsroot/minc/libsrc/minc_structures.h,v 6.1 1999-10-19 14:45:10 neelin Exp $ MINC (MNI)
---------------------------------------------------------------------------- */

/* Image conversion variable structure type */

typedef struct mi_icv_struct mi_icv_type;

struct mi_icv_struct {

   /* semiprivate : fields available to the package */

   int     do_scale;       /* Indicates to MI_convert_type that scaling should
                              be done */
   double  scale;          /* For scaling in MI_convert_type */
   double  offset;
   int     do_dimconvert;  /* Indicates that dimensional conversion function
                              should be given */
   int   (*dimconvert_func) (int operation, mi_icv_type *icvp, 
                             long start[], long count[], void *values,
                             long bufstart[], long bufcount[], void *buffer);
   int     do_fillvalue;   /* Indicates to MI_convert_type that fillvalue
                              checking should be done */
   double  fill_valid_min; /* Range limits for fillvalue checking */
   double  fill_valid_max;

   /* private : fields available only to icv routines */

   /* Fields that hold values passed by user */
   nc_type user_type;      /* Type to that user wants */
   int     user_typelen;   /* Length of type in bytes */
   int     user_sign;      /* Sign that user wants */
   int     user_do_range;  /* Does the user want range scaling? */
   double  user_vmax;      /* Range of values that user wants */
   double  user_vmin;
   int     user_do_norm;   /* Indicates that user wants value normalization */
   int     user_user_norm; /* If TRUE, user specifies range for norm, otherwise
                                 norm is taken from variable range */
   char    *user_maxvar;   /* Name of MIimagemax variable */
   char    *user_minvar;   /* Name of MIimagemin variable */
   double  user_imgmax;    /* Range for normalization */
   double  user_imgmin;
   int     user_do_dimconv; /* Indicates that user wants to do dimension 
                               conversion stuff */
   int     user_do_scalar; /* Indicates that user wants scalar fields */
   int     user_xdim_dir;  /* Direction for x, y and z dimensions */
   int     user_ydim_dir;
   int     user_zdim_dir;
   int     user_num_imgdims; /* Number of image (fastest varying) dimensions */
   long    user_dim_size[MI_MAX_IMGDIMS]; /* Size of fastest varying 
                                              dimensions for user */
   int     user_keep_aspect; /* Indicates that user wants to preserve the
                                aspect ratio when resizing images */
   int     user_do_fillvalue; /* Indicates that user wants fillvalue checking
                                 to be done */
   double  user_fillvalue;    /* Fillvalue that user wants */

   /* Fields that hold values from real variable */
   int     cdfid;          /* Id of cdf */
   int     varid;          /* Id of variable */
   int     imgmaxid;       /* Id of MIimagemax */
   int     imgminid;       /* Id of Miimagemin */
   int     var_ndims;      /* Number of dimensions of variable */
   int     var_dim[MAX_VAR_DIMS]; /* Dimensions of variable */
   nc_type var_type;       /* Variable type */
   int     var_typelen;    /* Length of type in bytes */
   int     var_sign;       /* Variable sign */
   double  var_vmax;       /* Range of values in variable */
   double  var_vmin;
   int     var_is_vector;  /* Is this variable a vector field */
   long    var_vector_size; /* Size of vector dimension */
   long    var_dim_size[MI_MAX_IMGDIMS]; /* Size of image dimensions in 
                                             variable */

   /* Fields derived from user values and variable values */
   int     derv_usr_float; /* Are user or variable values floating point? */
   int     derv_var_float;
   double  derv_imgmax;    /* Range for normalization */
   double  derv_imgmin;
   int     derv_firstdim;  /* First dimension (counting from fastest, ie.
                                 backwards) over which MIimagemax or 
                                 MIimagemin vary */
   int     derv_do_zero;   /* Indicates if we should zero user's buffer
                              on GETs */
   int     derv_do_bufsize_step; /* Indicates if we need to worry about 
                                    bufsize_step */
   int     derv_bufsize_step[MAX_VAR_DIMS];  /* Array of convenient multiples
                                                for buffer allocation */
   int     derv_var_compress; /* Indicate need for compressing variable or */
   int     derv_usr_compress;    /* user buffer */
   int     derv_dimconv_fastdim;  /* Fastest varying dimensions for dimension
                                     conversion */
   long    derv_var_pix_num; /* Number of pixels to compress/expand for */
   long   *derv_var_pix_off;    /* variable and user buffers, as well as */
   long    derv_usr_pix_num;    /* pointers to arrays of offsets */
   long   *derv_usr_pix_off;
   long    derv_icv_start[MAX_VAR_DIMS]; /* Space for storing parameters to */
   long    derv_icv_count[MAX_VAR_DIMS]; /* MI_icv_access */

                           /* Stuff that affects first user_num_imgdims
                              (excluding any vector dimension) as image
                              dimensions */
   int     derv_dim_flip[MI_MAX_IMGDIMS];   /* Flip dimension? */
   int     derv_dim_grow[MI_MAX_IMGDIMS];   /* Expand variable to fit user's 
                                                array? */
   int     derv_dim_scale[MI_MAX_IMGDIMS];  /* Grow/shrink scale factor */
   int     derv_dim_off[MI_MAX_IMGDIMS];    /* Pixels to skip in user's 
                                                image */
   double  derv_dim_step[MI_MAX_IMGDIMS];   /* Step, start for user's image 
                                                (analogous to MIstep, 
                                                MIstart) for first 
                                                user_num_imgdims dims */
   double  derv_dim_start[MI_MAX_IMGDIMS];
};

/* Structure for passing values for MI_varaccess */
typedef struct {
   int operation;
   int cdfid;
   int varid;
   nc_type var_type, call_type;
   int var_sign, call_sign;
   int var_value_size, call_value_size;
   mi_icv_type *icvp;
   int do_scale;
   int do_dimconvert;
   int do_fillvalue;
   long *start, *count;
   void *values;
} mi_varaccess_type;

/* Structure for passing values for micopy_var_values */
typedef struct {
   int value_size;            /* Size of each value */
   int incdfid, outcdfid;     /* Input and output cdf files */
   int invarid, outvarid;     /* Input and output variables */
} mi_vcopy_type;

/* Structure for passing values for MI_icv_dimconvert */
typedef struct {
   int do_compress, do_expand;
   long end[MAX_VAR_DIMS];
   long in_pix_num,     out_pix_num; /* Variables for compressing/expanding */
   long *in_pix_off,   *out_pix_off;
   void *in_pix_first, *out_pix_first;
   void *in_pix_last,  *out_pix_last;
   nc_type intype, outtype;     /* Variable types and signs */
   int insign, outsign;
   long buf_step[MAX_VAR_DIMS];    /* Step sizes for pointers */
   long usr_step[MAX_VAR_DIMS];
   long *istep, *ostep;
   void *istart, *ostart;       /* Beginning of buffers */
} mi_icv_dimconv_type;

#endif
