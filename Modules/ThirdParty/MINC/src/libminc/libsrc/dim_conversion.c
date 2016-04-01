/* ----------------------------- MNI Header -----------------------------------
@NAME       : dim_conversion.c
@DESCRIPTION: File of functions to do dimension conversion for image 
              conversion variables (icv). These variables allow conversion 
              of netcdf variables (the MINC image variable, in particular) 
              to a form more convenient for a program.
@METHOD     : Routines included in this file :
              public :
                 miicv_attach
              private :
                 MI_icv_get_dim
                 MI_get_dim_flip
                 MI_get_dim_scale
                 MI_get_dim_bufsize_step
                 MI_icv_get_dim_conversion
                 MI_icv_dimconvert
                 MI_icv_dimconv_init
@CREATED    : September 9, 1992. (Peter Neelin)
@MODIFIED   : 
 * $Log: dim_conversion.c,v $
 * Revision 6.6  2008-01-17 02:33:02  rotor
 *  * removed all rcsids
 *  * removed a bunch of ^L's that somehow crept in
 *  * removed old (and outdated) BUGS file
 *
 * Revision 6.5  2008/01/12 19:08:14  stever
 * Add __attribute__ ((unused)) to all rcsid variables.
 *
 * Revision 6.4  2004/10/15 13:44:52  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.3  2003/11/14 16:52:24  stever
 * More last-minute fixes.
 *
 * Revision 6.2  2003/09/18 16:16:15  bert
 * Use standard labs() and fabs() instead of our private macros
 *
 * Revision 6.1  1999/10/19 14:45:07  neelin
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
 * Revision 2.3  1995/02/08  19:14:44  neelin
 * More changes for irix 5 lint.
 *
 * Revision 2.2  1995/02/08  19:01:06  neelin
 * Moved private function declarations from minc_routines.h to appropriate file.
 *
 * Revision 2.1  1994/11/02  09:42:37  neelin
 * Fixed conversion of vector to scalar (old code simply returned the first
 * component of the vector - now averaging is done properly).
 *
 * Revision 2.0  94/09/28  10:37:52  neelin
 * Release of minc version 0.2
 * 
 * Revision 1.11  94/09/28  10:37:35  neelin
 * Pre-release
 * 
 * Revision 1.10  94/07/05  15:31:07  neelin
 * Assume that MIstep is positive if it is not found (for flipping dimensions).
 * 
 * Revision 1.9  94/03/16  10:30:00  neelin
 * Changed default MI_ICV_STEP: if not found use 1.0 (and 0.0 for start).
 * 
 * Revision 1.8  93/08/11  12:06:03  neelin
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
#include <math.h>
#include <type_limits.h>

/* Private functions */
PRIVATE int MI_icv_get_dim(mi_icv_type *icvp, int cdfid, int varid);
PRIVATE int MI_get_dim_flip(mi_icv_type *icvp, int cdfid, int dimvid[], 
                           int subsc[]);
PRIVATE int MI_get_dim_scale(mi_icv_type *icvp, int cdfid, int dimvid[]);
PRIVATE int MI_get_dim_bufsize_step(mi_icv_type *icvp, int subsc[]);
PRIVATE int MI_icv_get_dim_conversion(mi_icv_type *icvp, int subsc[]);
PRIVATE int MI_icv_dimconvert(int operation, mi_icv_type *icvp,
                              long start[], long count[], void *values,
                              long bufstart[], long bufcount[], void *buffer);
PRIVATE int MI_icv_dimconv_init(int operation, mi_icv_type *icvp,
                              mi_icv_dimconv_type *dcp,
                              long start[], long count[], void *values,
                              long bufstart[], long bufcount[], void *buffer);


/* ----------------------------- MNI Header -----------------------------------
@NAME       : miicv_attach
@INPUT      : icvid - icv id
              cdfid - cdf file id
              varid - cdf variable id
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Attaches an open cdf file and variable to an image conversion
              variable for subsequent access through miicvget and miicvput.
              File must be in data mode.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : September 9, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
MNCAPI int miicv_attach(int icvid, int cdfid, int varid)
{
   mi_icv_type *icvp;         /* Pointer to icv structure */
   long size_diff, user_dim_size;
   int idim;

   MI_SAVE_ROUTINE_NAME("miicv_attach");

   /* Check icv id */
   if ((icvp=MI_icv_chkid(icvid)) == NULL) MI_RETURN_ERROR(MI_ERROR);

   /* Call routine to set variables for everything except dimension 
      conversion */
   {MI_CHK_ERR(miicv_ndattach(icvid, cdfid, varid))}

   /* Check to see if we need to worry about dimension conversion */
   if (!icvp->user_do_dimconv) {
      MI_RETURN(MI_NOERROR);
   }

   /* Reset cdfid and varid in icv structure in case something goes wrong 
      in dimension conversion calculations */
   icvp->cdfid = MI_ERROR;
   icvp->varid = MI_ERROR;

   /* Get dimensions info */
   {MI_CHK_ERR(MI_icv_get_dim(icvp, cdfid, varid))}

   /* Set the do_dimconvert field of icv structure
      We do dimension conversion if any dimension needs flipping, scaling
      or offset, or if we have to convert from vector to scalar. */

   icvp->do_dimconvert = (icvp->user_do_scalar && icvp->var_is_vector);
   for (idim=0; idim<icvp->user_num_imgdims; idim++) {
      if (icvp->derv_dim_flip[idim] || (icvp->derv_dim_scale[idim] != 1) ||
          (icvp->derv_dim_off[idim] != 0))
         icvp->do_dimconvert = TRUE;
   }
   icvp->dimconvert_func = MI_icv_dimconvert;

   /* Check if we have to zero user's buffer on GETs */
   icvp->derv_do_zero = FALSE;
   for (idim=0; idim<icvp->user_num_imgdims; idim++) {
      user_dim_size = ((icvp->user_dim_size[idim]<=0) ? 
                       icvp->var_dim_size[idim] :
                       icvp->user_dim_size[idim]);
      if (icvp->derv_dim_grow[idim])
         size_diff = user_dim_size -
                     icvp->var_dim_size[idim] * icvp->derv_dim_scale[idim];
      else
         size_diff = user_dim_size - 1 -
                     (icvp->var_dim_size[idim] - 1)
                        / icvp->derv_dim_scale[idim];
      if ((icvp->derv_dim_off[idim]!=0) || (size_diff!=0))
         icvp->derv_do_zero = TRUE;
   }

   /* Set the cdfid and varid fields */
   icvp->cdfid = cdfid;
   icvp->varid = varid;

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_get_dim
@INPUT      : icvp  - pointer to icv structure
              cdfid - cdf file id
              varid - variable id
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Gets dimension info for the icv
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : August 10, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_icv_get_dim(mi_icv_type *icvp, int cdfid, int varid)
     /* ARGSUSED */
{
   int oldncopts;             /* For saving value of ncopts */
   char dimname[MAX_NC_NAME]; /* Dimensions name */
   int idim;                  /* Looping counter for fastest image dims */
   int subsc[MI_MAX_IMGDIMS]; /* Subscripts for fastest image dims */
   int dimvid[MI_MAX_IMGDIMS]; /* Variable ids for dimensions */

   MI_SAVE_ROUTINE_NAME("MI_icv_get_dim");

   /* Check that the variable has at least icvp->user_num_imgdims dimensions */
   if (icvp->var_ndims < icvp->user_num_imgdims) {
      MI_LOG_PKG_ERROR2(MI_ERR_TOOFEWDIMS, 
                       "Variable has too few dimensions to be an image");
      MI_RETURN_ERROR(MI_ERROR);
   }

   /* Check the first dimensions of the variable */
   MI_CHK_ERR(ncdiminq(cdfid, icvp->var_dim[icvp->var_ndims-1], dimname, 
                       &(icvp->var_vector_size)))
   icvp->var_is_vector = STRINGS_EQUAL(dimname, MIvector_dimension);

   /* Check that the variable has at least icvp->user_num_imgdims+1 
      dimensions if it is a vector field */
   if (icvp->var_is_vector && (icvp->var_ndims < icvp->user_num_imgdims+1)) {
      MI_LOG_PKG_ERROR2(MI_ERR_TOOFEWDIMS, 
                        "Variable has too few dimensions to be an image");
      MI_RETURN_ERROR(MI_ERROR);
   }

   /* Check for dimension flipping and get dimension sizes */

   /* Get subscripts for first icvp->user_num_imgdims dimensions */
   subsc[0] = (icvp->var_is_vector) ? icvp->var_ndims-2 : icvp->var_ndims-1;
   for (idim=1; idim < icvp->user_num_imgdims; idim++)
      subsc[idim]=subsc[idim-1]-1;

   /* Get dimension variable ids */
   for (idim=0; idim < icvp->user_num_imgdims; idim++) {
      {MI_CHK_ERR(ncdiminq(cdfid, icvp->var_dim[subsc[idim]], dimname, 
                       &(icvp->var_dim_size[idim])))};
      oldncopts =get_ncopts(); set_ncopts(0);
      dimvid[idim] = ncvarid(cdfid, dimname);
      set_ncopts(oldncopts);
   }

   /* Check for flipping */
   {MI_CHK_ERR(MI_get_dim_flip(icvp, cdfid, dimvid, subsc))}

   /* Check for scaling of dimension */
   {MI_CHK_ERR(MI_get_dim_scale(icvp, cdfid, dimvid))}

   /* Check for variable buffer size increments */
   {MI_CHK_ERR(MI_get_dim_bufsize_step(icvp, subsc))}

   /* Get information for dimension conversion */
   {MI_CHK_ERR(MI_icv_get_dim_conversion(icvp, subsc))}

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_get_dim_flip
@INPUT      : icvp  - icv pointer
              cdfid - cdf file id
              dimvid - variable id
              subsc - array of dimension subscripts for fastest varying 
                 dimensions
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Checks for flipping of icv.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : September 1, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_get_dim_flip(mi_icv_type *icvp, int cdfid, int dimvid[], 
                           int subsc[])
{
   int oldncopts;             /* For saving value of ncopts */
   char dimname[MAX_NC_NAME]; /* Dimensions name */
   int dim_dir;               /* Desired direction for current dimension */
   double dimstep;            /* Dimension step size (and direction) */
   int idim;

   MI_SAVE_ROUTINE_NAME("MI_get_dim_flip");

   /* Loop through fast dimensions */

   for (idim=0; idim < icvp->user_num_imgdims; idim++) {

      /* Get name of the dimension */
      {MI_CHK_ERR(ncdiminq(cdfid, icvp->var_dim[subsc[idim]], dimname, 
                           NULL))}

      /* Should we look for dimension flipping? */
      icvp->derv_dim_flip[idim]=FALSE;
      if (STRINGS_EQUAL(dimname, MIxspace) || 
          STRINGS_EQUAL(dimname, MIxfrequency))
         dim_dir = icvp->user_xdim_dir;
      else if (STRINGS_EQUAL(dimname, MIyspace) || 
               STRINGS_EQUAL(dimname, MIyfrequency))
         dim_dir = icvp->user_ydim_dir;
      else if (STRINGS_EQUAL(dimname, MIzspace) || 
               STRINGS_EQUAL(dimname, MIzfrequency))
         dim_dir = icvp->user_zdim_dir;
      else
         dim_dir = MI_ICV_ANYDIR;

      /* Look for variable corresponding to dimension */
      if (dim_dir != MI_ICV_ANYDIR) {   /* Look for flipping? */

         /* Get the value of the MIstep attribute to determine whether flipping
            is needed. Assume that direction is positive if no step is
            provided. */
         dimstep = 1.0;
         if (dimvid[idim] != MI_ERROR) {   /* if dimension exists */
            oldncopts =get_ncopts(); set_ncopts(0);
            (void) miattget1(cdfid, dimvid[idim], MIstep, NC_DOUBLE, &dimstep);
            set_ncopts(oldncopts);
         }                           /* if dimension exists */
         if (dim_dir == MI_ICV_POSITIVE)
            icvp->derv_dim_flip[idim] = (dimstep<0.0);
         else if (dim_dir == MI_ICV_NEGATIVE)
            icvp->derv_dim_flip[idim] = (dimstep>0.0);
      }                          /* if look for flipping */

   }                           /* for each dimension */

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_get_dim_scale
@INPUT      : icvp  - icv pointer
              cdfid - cdf file id
              dimvid - dimension variable id
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Checks for scaling of images
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : September 1, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_get_dim_scale(mi_icv_type *icvp, int cdfid, int dimvid[])
{
   int oldncopts;             /* For saving value of ncopts */
   int min_grow, dim_grow;
   int min_scale, dim_scale;
   double dimstep, dimstart;
   int idim;
   long user_dim_size;

   MI_SAVE_ROUTINE_NAME("MI_get_dim_scale");

   /* Loop through dimensions, calculating scale and looking for smallest 
      one. For each dimension, check to see if we need to shrink or grow the 
      image. (This is get-oriented: grow is TRUE if the variable dimension 
      has to be expanded to fit the user's dimensions). */

   for (idim=0; idim < icvp->user_num_imgdims; idim++) {

      /* Check to see if we user wants resize */
      if (icvp->user_dim_size[idim] <= 0) {
         icvp->derv_dim_grow[idim]=TRUE;
         icvp->derv_dim_scale[idim]=1;
      }
      else {

         /* Check for growing or shrinking */
         icvp->derv_dim_grow[idim] =
            (icvp->var_dim_size[idim] <= icvp->user_dim_size[idim]);

         /* If growing */
         if (icvp->derv_dim_grow[idim]) {
            /* Get scale so that whole image fits in user array */
            icvp->derv_dim_scale[idim] = 
               icvp->user_dim_size[idim] / icvp->var_dim_size[idim];
         }

         /* Otherwise, shrinking. Things are complicated by the fact that
            the external variable must fit completely in the user's array */
         else {

            icvp->derv_dim_scale[idim] = 1 +
               (icvp->var_dim_size[idim] - 1) / icvp->user_dim_size[idim];
         }
      }           /* if user wants resizing */

      /* Check for smallest scale */
      if (idim==0) {
         min_grow = icvp->derv_dim_grow[idim];
         min_scale = icvp->derv_dim_scale[idim];
      }
      else {
         dim_grow  = icvp->derv_dim_grow[idim];
         dim_scale = icvp->derv_dim_scale[idim];
         /* Check for one of three conditions :
               (1) smallest so far is growing, but this dim is shrinking
               (2) both are growing and this dim has smaller scale
               (3) both are shrinking and this dim has larger scale */
         if ((min_grow && !dim_grow) ||
             ((min_grow && dim_grow) &&
              (min_scale > dim_scale)) ||
             ((!min_grow && !dim_grow) &&
              (min_scale < dim_scale))) {
            min_grow = dim_grow;
            min_scale = dim_scale;
         }
      }           /* if not first dim */

   }           /* for each dimension, get scale */

   /* Loop through dimensions, resetting scale if needed, setting offset 
      and pixel step and start */

   for (idim=0; idim < icvp->user_num_imgdims; idim++) {

      /* Check for aspect ratio */
      if (icvp->user_keep_aspect) {
         icvp->derv_dim_grow[idim]  = min_grow;
         icvp->derv_dim_scale[idim] = min_scale;
      }

      /* Get user's buffer size */
      user_dim_size = ((icvp->user_dim_size[idim]<=0) ? 
                       icvp->var_dim_size[idim] :
                       icvp->user_dim_size[idim]);

      /* Set offset of variable into user's image */

      /* If growing */
      if (icvp->derv_dim_grow[idim]) {
         /* Calculate remainder and split it in half */
         icvp->derv_dim_off[idim] = 
            ( user_dim_size -
             icvp->var_dim_size[idim] * icvp->derv_dim_scale[idim] )
                                      / 2;
      }
      /* Otherwise, shrinking. Things are complicated by the fact that
         the external variable must fit completely in the user's array */
      else {
         /* Calculate remainder and split it in half */
         icvp->derv_dim_off[idim] = 
            ( user_dim_size - 1 -
             (icvp->var_dim_size[idim] - 1) 
                              / icvp->derv_dim_scale[idim] ) / 2 ;
      }

      /* Get pixel step and start for variable and calculate for user.
         Look for them in the dimension variable (if MIstep is not
         there, then use defaults step = 1.0, start = 0.0 */
      oldncopts =get_ncopts(); set_ncopts(0);
      dimstep = 1.0;
      (void) miattget1(cdfid, dimvid[idim], MIstep, NC_DOUBLE, &dimstep);
      /* Flip dimstep if needed */
      if (icvp->derv_dim_flip[idim]) 
         dimstep *= (-1);
      /* Get step size for user's image */
      icvp->derv_dim_step[idim] = icvp->derv_dim_grow[idim] ?
         dimstep / icvp->derv_dim_scale[idim] :
            dimstep * icvp->derv_dim_scale[idim];
      /* Get start position for user's image - if no MIstart for
         dimension, then assume 0.0 */
      dimstart = 0.0;
      (void) miattget1(cdfid, dimvid[idim], MIstart, NC_DOUBLE, &dimstart);
      /* Flip dimstart if needed */
      if (icvp->derv_dim_flip[idim])
         dimstart -= dimstep * (icvp->var_dim_size[idim]-1);
      /* Calculate start position */
      icvp->derv_dim_start[idim] = dimstart + 
         (icvp->derv_dim_step[idim] - dimstep) / 2.0 -
            icvp->derv_dim_off[idim] * icvp->derv_dim_step[idim];
      set_ncopts(oldncopts);

   }                 /* for each dimension */

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_get_dim_bufsize_step
@INPUT      : icvp  - icv pointer
              subsc - array of dimension subscripts for fastest varying 
                 dimensions
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Sets the variables giving variable buffer size
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : September 3, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_get_dim_bufsize_step(mi_icv_type *icvp, int subsc[])
{
   int idim;

   MI_SAVE_ROUTINE_NAME("MI_get_dim_bufsize_step");

   /* Set default buffer size step */
   for (idim=0; idim < MAX_VAR_DIMS; idim++)
      icvp->derv_bufsize_step[idim]=1;

   /* Check for converting vector to scalar */
   icvp->derv_do_bufsize_step = (icvp->var_is_vector && icvp->user_do_scalar);
   if (icvp->derv_do_bufsize_step)
      icvp->derv_bufsize_step[icvp->var_ndims-1] = icvp->var_vector_size;
      
   /* Check each dimension to see if we need to worry about the variable
      buffer size. This only occurs if we are shrinking the dimension from 
      the variable buffer to the user buffer. */
   for (idim=0; idim < icvp->user_num_imgdims; idim++) {
      if (!icvp->derv_dim_grow[idim])
         icvp->derv_bufsize_step[subsc[idim]]=icvp->derv_dim_scale[idim];
      if (icvp->derv_bufsize_step[subsc[idim]] != 1)
         icvp->derv_do_bufsize_step = TRUE;
   }

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_get_dim_conversion
@INPUT      : icvp  - icv pointer
              subsc - array of dimension subscripts for fastest varying 
                 dimensions
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Sets the variables for dimensions converions
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : September 8, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_icv_get_dim_conversion(mi_icv_type *icvp, int subsc[])
     /* ARGSUSED */
{
   int idim;

   MI_SAVE_ROUTINE_NAME("MI_icv_get_dim_conversion");

   /* Find out whether we need to compress variable or user buffer */
   icvp->derv_var_compress = (icvp->var_is_vector && icvp->user_do_scalar);
   icvp->derv_usr_compress = FALSE;
   for (idim=0; idim<icvp->user_num_imgdims; idim++) {
      if (icvp->derv_dim_scale[idim]!=1) {
         if (icvp->derv_dim_grow[idim])
            icvp->derv_usr_compress = TRUE;
         else
            icvp->derv_var_compress = TRUE;
      }
   }

   /* Get the fastest varying dimension in user's buffer */
   icvp->derv_dimconv_fastdim = icvp->var_ndims-1;
   if (icvp->var_is_vector && icvp->user_do_scalar)
      icvp->derv_dimconv_fastdim--;

   /* Find out how many pixels to compress/expand for variable and user
      buffers and allocate arrays */
   if (icvp->var_is_vector && icvp->user_do_scalar)
      icvp->derv_var_pix_num=icvp->var_vector_size;
   else
      icvp->derv_var_pix_num=1;
   icvp->derv_usr_pix_num=1;
   for (idim=0; idim<icvp->user_num_imgdims; idim++) {
      if (icvp->derv_dim_grow[idim])
         icvp->derv_usr_pix_num *= icvp->derv_dim_scale[idim];
      else
         icvp->derv_var_pix_num *= MIN(icvp->var_dim_size[idim], 
                                       icvp->derv_dim_scale[idim]);
   }
   icvp->derv_var_pix_off = MALLOC(icvp->derv_var_pix_num, long);
   icvp->derv_usr_pix_off = MALLOC(icvp->derv_usr_pix_num, long);
   if ((icvp->derv_var_pix_off == NULL) || (icvp->derv_usr_pix_off == NULL)) {
      MI_LOG_SYS_ERROR1("MI_icv_get_dim_conversion");
      MI_RETURN_ERROR(MI_ERROR);
   }

   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_dimconvert
@INPUT      : operation  - MI_PRIV_GET or MI_PRIV_PUT
              icvp       - icv structure pointer
              start      - start passed by user
              count      - count passed by user
              values     - pointer to user's data area (for put)
              bufstart   - start of variable buffer
              bufcount   - count of variable buffer
              buffer     - pointer to variable buffer (for get)
@OUTPUT     : values     - pointer to user's data area (for get)
              buffer     - pointer to variable buffer (for put)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Converts values and dimensions from an input buffer to the 
              user's buffer. Called by MI_var_action.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : August 27, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_icv_dimconvert(int operation, mi_icv_type *icvp,
                              long start[], long count[], void *values,
                              long bufstart[], long bufcount[], void *buffer)
{
   mi_icv_dimconv_type dim_conv_struct;
   mi_icv_dimconv_type *dcp;
   double sum0, sum1;           /* Counters for averaging values */
   double dvalue;               /* Pixel value */
   long counter[MAX_VAR_DIMS];  /* Dimension loop counter */
   void *ptr, *iptr, *optr;     /* Pointers for looping through fastest dim */
   void *ivecptr[MAX_VAR_DIMS]; /* Pointers to start of each dimension */
   void *ovecptr[MAX_VAR_DIMS];
   long *end;                   /* Pointer to array of dimension ends */
   int fastdim;                 /* Dimension that varies fastest */
   long ipix;                   /* Buffer subscript */
   int idim;                    /* Dimension subscript */
   int notmodified;             /* First dimension not reset */
   int out_of_range;            /* Flag indicating one pixel of sum out of 
                                   range */
   double dmin, dmax, epsilon;  /* Range limits */

   MI_SAVE_ROUTINE_NAME("MI_icv_dimconvert");

   /* Initialize variables */
   dcp = &dim_conv_struct;
   {MI_CHK_ERR(MI_icv_dimconv_init(operation, icvp, dcp, start, count, values,
                                   bufstart, bufcount, buffer))}

   /* Initialize local variables */
   iptr    = dcp->istart;
   optr    = dcp->ostart;
   end     = dcp->end;
   fastdim = icvp->derv_dimconv_fastdim;
   dmax = icvp->fill_valid_max;
   dmin = icvp->fill_valid_min;
   epsilon = (dmax - dmin) * FILLVALUE_EPSILON;
   epsilon = fabs(epsilon);
   dmax += epsilon;
   dmin -= epsilon;

   /* Initialize counters */
   for (idim=0; idim<=fastdim; idim++) {
      counter[idim] = 0;
      ivecptr[idim] = iptr;
      ovecptr[idim] = optr;
   }

   /* Loop through data */

   while (counter[0] < end[0]) {

      /* Compress data by averaging if needed */
      if (!dcp->do_compress) {
         {MI_TO_DOUBLE(dvalue, dcp->intype, dcp->insign, iptr)}
         out_of_range = (icvp->do_fillvalue && 
                         ((dvalue < dmin) || (dvalue > dmax)));
      }
      else {
         sum1 = 0.0;
         sum0 = 0.0;
         out_of_range=FALSE;
         for (ipix=0; ipix<dcp->in_pix_num; ipix++) {
            ptr=(void *) ((char *)iptr + dcp->in_pix_off[ipix]);

            /* Check if we are outside the buffer.
               If we are looking before the buffer, then we need to
               add in the previous result to do averaging properly. If
               we are looking after the buffer, then break. */
            if (ptr<dcp->in_pix_first) {
               /* Get the output value and re-scale it */
               {MI_TO_DOUBLE(dvalue, dcp->outtype, dcp->outsign, optr)}
               if (icvp->do_scale) {
                  dvalue = ((icvp->scale==0.0) ?
                            0.0 : (dvalue - icvp->offset) / icvp->scale);
               }
            }
            else if (ptr>dcp->in_pix_last) {
               continue;
            }
            else {
               {MI_TO_DOUBLE(dvalue, dcp->intype, dcp->insign, ptr)}
            }

            /* Add in the value, checking for range if needed */
            if (icvp->do_fillvalue && ((dvalue < dmin) || (dvalue > dmax))) {
               out_of_range = TRUE;
            }
            else {
               sum1 += dvalue;
               sum0++;
            }
         }         /* Foreach pixel to compress */

         /* Average values */
         if (sum0!=0.0)
            dvalue = sum1/sum0;
         else
            dvalue = 0.0;
      }           /* If compress */

      /* Check for out of range values and scale result */
      if (out_of_range) {
         dvalue = icvp->user_fillvalue;
      }
      else if (icvp->do_scale) {
         dvalue = icvp->scale * dvalue + icvp->offset;
      }

      /* Expand data if needed */
      if (!dcp->do_expand) {
         {MI_FROM_DOUBLE(dvalue, dcp->outtype, dcp->outsign, optr)}
      }
      else {
         for (ipix=0; ipix<dcp->out_pix_num; ipix++) {
            ptr=(void *) ((char *)optr + dcp->out_pix_off[ipix]);

            /* Check if we are outside the buffer. */
            if ((ptr>=dcp->out_pix_first) && (ptr<=dcp->out_pix_last)) {
               {MI_FROM_DOUBLE(dvalue, dcp->outtype, dcp->outsign, ptr)}
            }

         }         /* Foreach pixel to expand */
      }         /* if expand */

      /* Increment the counter and the pointers */
      if ((++counter[fastdim]) < end[fastdim]) {
         optr = (void *) ((char *) optr + dcp->ostep[fastdim]);
         iptr = (void *) ((char *) iptr + dcp->istep[fastdim]);
      }
      else {
         /* If we reach the end of fastdim, then reset the counter and
            increment the next dimension down - keep going as needed.
            The vectors ovecptr and ivecptr give the starting values of optr 
            and iptr for that dimension. */
         idim = fastdim;
         while ((idim>0) && (counter[idim] >= end[idim])) {
            counter[idim] = 0;
            idim--;
            counter[idim]++;
            ovecptr[idim] = (void *)((char *)ovecptr[idim]+dcp->ostep[idim]);
            ivecptr[idim] = (void *)((char *)ivecptr[idim]+dcp->istep[idim]);
         }
         notmodified = idim;

         /* Copy the starting index up the vector */
         for (idim=notmodified+1; idim<=fastdim; idim++) {
            ovecptr[idim]=ovecptr[notmodified];
            ivecptr[idim]=ivecptr[notmodified];
         }

         optr = ovecptr[fastdim];
         iptr = ivecptr[fastdim];
      }      /* if at end of row */

   }      /* while more pixels to process */


   MI_RETURN(MI_NOERROR);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_icv_dimconv_init
@INPUT      : operation  - MI_PRIV_GET or MI_PRIV_PUT
              icvp       - icv structure pointer
              dcp        - dimconvert structure pointer
              start      - start passed by user
              count      - count passed by user
              values     - pointer to user's data area (for put)
              bufstart   - start of variable buffer
              bufcount   - count of variable buffer
              buffer     - pointer to variable buffer (for get)
@OUTPUT     : 
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Sets up stuff for MI_icv_dimconvert.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF routines
@CREATED    : September 4, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_icv_dimconv_init(int operation, mi_icv_type *icvp,
                              mi_icv_dimconv_type *dcp,
                              long start[], long count[], void *values,
                              long bufstart[], long bufcount[], void *buffer)
     /* ARGSUSED */
{
   long buffer_len, values_len; /* Buffer lengths, offsets and indices */
   long buffer_off, values_off;
   long buffer_index, values_index;
   int imgdim_high, imgdim_low; /* Range of subscripts of image dimensions */
   int scale, offset;            /* Dimension scale and offset */
   int idim, jdim;
   int fastdim;
   /* Variables for calculating pixel offsets for compress/expand */
   long var_dcount[MI_MAX_IMGDIMS+1], var_dend[MI_MAX_IMGDIMS+1];
   long usr_dcount[MI_MAX_IMGDIMS+1], usr_dend[MI_MAX_IMGDIMS+1];
   long pixcount;
   int var_fd, usr_fd, dshift;
   long ipix;

   MI_SAVE_ROUTINE_NAME("MI_icv_dimconv_init");

   /* Check to see if any compression or expansion needs to be done.
      Work it out for a GET and then swap if a PUT. */
   if (operation==MI_PRIV_GET) {
      dcp->do_compress = icvp->derv_var_compress;
      dcp->do_expand   = icvp->derv_usr_compress;
   }
   else {
      dcp->do_expand   = icvp->derv_var_compress;
      dcp->do_compress = icvp->derv_usr_compress;
   }

   fastdim = icvp->derv_dimconv_fastdim;

   /* Get the indices of high and low image dimensions */
   imgdim_high=icvp->var_ndims-1;
   if (icvp->var_is_vector) imgdim_high--;
   imgdim_low = imgdim_high - icvp->user_num_imgdims + 1;

   /* Get the buffer sizes */
   buffer_len = icvp->var_typelen;
   values_len = icvp->user_typelen;
   for (idim=0; idim < icvp->var_ndims; idim++) {
      buffer_len *= bufcount[idim];
      if (idim<=fastdim)
         values_len *= icvp->derv_icv_count[idim];
   }

   /* Calculate step size for variable and user buffers. This does not
      allow for growing or shrinking pixels. That correction is done below. */
   if (icvp->var_is_vector && icvp->user_do_scalar) {
      dcp->buf_step[fastdim+1] = icvp->var_typelen;
      dcp->buf_step[fastdim] = dcp->buf_step[fastdim+1] * bufcount[fastdim+1];
   }
   else {
      dcp->buf_step[fastdim] = icvp->var_typelen;
   }
   dcp->usr_step[fastdim] = icvp->user_typelen;
   for (idim=fastdim-1; idim>=0; idim--) {
      dcp->buf_step[idim] = dcp->buf_step[idim+1] * bufcount[idim+1];
      dcp->usr_step[idim] = dcp->usr_step[idim+1]
         * icvp->derv_icv_count[idim+1];
   }

   /* Set sign of user steps for flipping, if needed */
   for (idim=imgdim_low; idim <= imgdim_high; idim++) {
      if (icvp->derv_dim_flip[imgdim_high-idim])
         dcp->usr_step[idim] *= (-1);
   }

   /* Get the pointers to the start of buffers and the number of pixels
      in each dimension (count a pixel as one expansion/compression -
      one time through the loop below) */
   buffer_off = 0;
   values_off = 0;
   for (idim=0; idim <= fastdim; idim++) {
      if ((idim < imgdim_low) || (idim > imgdim_high)) {
         dcp->end[idim] = bufcount[idim];
         buffer_index = 0;
         values_index = bufstart[idim] - icvp->derv_icv_start[idim];
      }
      else {
         jdim = imgdim_high - idim;
         scale = icvp->derv_dim_scale[jdim];
         offset = icvp->derv_dim_off[jdim];
         if (icvp->derv_dim_grow[jdim]) {
            dcp->end[idim] = bufcount[idim];
            buffer_index = 0;
            if (!icvp->derv_dim_flip[jdim])
               values_index = bufstart[idim]*scale
                  - icvp->derv_icv_start[idim] + offset;
            else
               values_index = 
                  (icvp->var_dim_size[jdim] - bufstart[idim])*scale
                  - 1 - icvp->derv_icv_start[idim] + offset;
         }
         else {
            dcp->end[idim] = (bufcount[idim] - 1 + bufstart[idim]%scale) 
                                     / scale + 1; 
            buffer_index = -(bufstart[idim] % scale);
            if (!icvp->derv_dim_flip[jdim])
               values_index = bufstart[idim]/scale
                  - icvp->derv_icv_start[idim] + offset;
            else
               values_index = 
                  (icvp->var_dim_size[jdim] - bufstart[idim] - 1)/scale
                  - icvp->derv_icv_start[idim] + offset;
         }
      }

      /* Force these offsets to stay within the presumed limits of the
       * allocated memory. Before implementing this change it was
       * possible for miicv_get() or miicv_put() to write outside the
       * "values" buffer, leading to heap corruption.  I overload the
       * "pixcount" variable since it is never really used elsewhere
       * (bert).
       */
      pixcount = buffer_index * labs(dcp->buf_step[idim]);
      if (buffer_off + pixcount < buffer_len) {
        buffer_off += pixcount;
      }
      pixcount = values_index * labs(dcp->usr_step[idim]);
      if (values_off + pixcount < values_len) {
        values_off += pixcount;
      }
   }

   /* Disallow negative offsets to avoid illegal accesses (bert).
    */
   if (buffer_off < 0) {
     buffer_off = 0;
   }
   if (values_off < 0) {
     values_off = 0;
   }

   /* Calculate arrays of offsets for compress/expand. */
   if (dcp->do_compress || dcp->do_expand) {
      /* Initialize counters */
      var_fd = icvp->user_num_imgdims-1;
      usr_fd = icvp->user_num_imgdims-1;
      if (icvp->var_is_vector && icvp->user_do_scalar) {
         var_fd++;
         var_dcount[var_fd]=0;
         var_dend[var_fd]=icvp->var_vector_size;
      }
      for (jdim=0; jdim<icvp->user_num_imgdims; jdim++) {
         idim=icvp->user_num_imgdims - jdim - 1;
         var_dcount[idim] = 0;
         usr_dcount[idim] = 0;
         var_dend[idim] = (icvp->derv_dim_grow[jdim] ?
                           1 : MIN(icvp->var_dim_size[jdim],
                                   icvp->derv_dim_scale[jdim]));
         usr_dend[idim] = (icvp->derv_dim_grow[jdim] ?
                           icvp->derv_dim_scale[jdim] : 1);
      }

      /* Loop through variable buffer pixels */
      pixcount=0;
      dshift = imgdim_low;
      for (ipix=0; ipix<icvp->derv_var_pix_num; ipix++) {
         icvp->derv_var_pix_off[ipix] = pixcount;
         pixcount += dcp->buf_step[var_fd+dshift];
         if ((++var_dcount[var_fd]) >= var_dend[var_fd]) {
            idim=var_fd;
            while ((idim>0) && (var_dcount[idim]>=var_dend[idim])) {
               var_dcount[idim]=0;
               idim--;
               var_dcount[idim]++;
            }
            for (idim=0, pixcount=0; idim<=var_fd; idim++) {
               pixcount += var_dcount[idim] * dcp->buf_step[idim+dshift];
            }
         }
      }

      /* Loop through user buffer pixels */
      pixcount=0;
      dshift = imgdim_low;
      for (ipix=0; ipix<icvp->derv_usr_pix_num; ipix++) {
         icvp->derv_usr_pix_off[ipix] = pixcount;
         pixcount += dcp->usr_step[usr_fd+dshift];
         if ((++usr_dcount[usr_fd]) >= usr_dend[usr_fd]) {
            idim=usr_fd;
            while ((idim>0) && (usr_dcount[idim]>=usr_dend[idim])) {
               usr_dcount[idim]=0;
               idim--;
               usr_dcount[idim]++;
            }
            for (idim=0, pixcount=0; idim<=var_fd; idim++) {
               pixcount += usr_dcount[idim] * dcp->usr_step[idim+dshift];
            }
         }
      }

      /* Correct buffer steps for compress/expand */
      for (idim=imgdim_low; idim <= imgdim_high; idim++) {
         jdim = imgdim_high-idim;
         if (icvp->derv_dim_grow[jdim])
            dcp->usr_step[idim] *= icvp->derv_dim_scale[jdim];
         else
            dcp->buf_step[idim] *= icvp->derv_dim_scale[jdim];
      }

   }           /* if compress/expand */

   /* Set input and output variables */
   if (operation==MI_PRIV_GET) {          /* For a GET */
      dcp->in_pix_num = icvp->derv_var_pix_num;
      dcp->in_pix_off = icvp->derv_var_pix_off;
      dcp->in_pix_first = buffer;
      dcp->in_pix_last = (void *) ((char *)buffer + buffer_len - 1);
      dcp->out_pix_num = icvp->derv_usr_pix_num;
      dcp->out_pix_off = icvp->derv_usr_pix_off;
      dcp->out_pix_first = values;
      dcp->out_pix_last = (void *) ((char *)values + values_len - 1);
      dcp->intype = icvp->var_type;
      dcp->insign = icvp->var_sign;
      dcp->outtype = icvp->user_type;
      dcp->outsign = icvp->user_sign;
      dcp->istep = dcp->buf_step;
      dcp->ostep = dcp->usr_step;
      dcp->istart = (void *) ((char *) buffer + buffer_off);
      dcp->ostart = (void *) ((char *) values + values_off);
   }                   /* if GET */
   else {                                 /* For a PUT */
      dcp->out_pix_num = icvp->derv_var_pix_num;
      dcp->out_pix_off = icvp->derv_var_pix_off;
      dcp->out_pix_first = buffer;
      dcp->out_pix_last = (void *) ((char *)buffer + buffer_len - 1);
      dcp->in_pix_num = icvp->derv_usr_pix_num;
      dcp->in_pix_off = icvp->derv_usr_pix_off;
      dcp->in_pix_first = values;
      dcp->in_pix_last = (void *) ((char *)values + values_len - 1);
      dcp->outtype = icvp->var_type;
      dcp->outsign = icvp->var_sign;
      dcp->intype = icvp->user_type;
      dcp->insign = icvp->user_sign;
      dcp->ostep = dcp->buf_step;
      dcp->istep = dcp->usr_step;
      dcp->ostart = (void *) ((char *) buffer + buffer_off);
      dcp->istart = (void *) ((char *) values + values_off);
   }                   /* if PUT */

   MI_RETURN(MI_NOERROR);
}
