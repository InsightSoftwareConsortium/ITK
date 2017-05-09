/* ----------------------------- MNI Header -----------------------------------
@NAME       : value_conversion.c
@DESCRIPTION: File of functions for converting values. These routines
              are for use by other MINC routines only.
@METHOD     : Routines included in this file :
              semiprivate : (public but destined only for this package)
                 MI_varaccess
                 MI_var_loop
                 MI_get_sign_from_string
                 MI_convert_type
              private :
                 MI_get_sign
                 MI_var_action
@CREATED    : July 27, 1992. (Peter Neelin, Montreal Neurological Institute)
@MODIFIED   : 
 * $Log: value_conversion.c,v $
 * Revision 6.8  2008-01-17 02:33:02  rotor
 *  * removed all rcsids
 *  * removed a bunch of ^L's that somehow crept in
 *  * removed old (and outdated) BUGS file
 *
 * Revision 6.7  2008/01/12 19:08:14  stever
 * Add __attribute__ ((unused)) to all rcsid variables.
 *
 * Revision 6.6  2004/10/15 13:45:28  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.5  2004/04/27 15:49:51  bert
 * Use new logging, gettext preparation
 *
 * Revision 6.4  2003/11/14 16:52:24  stever
 * More last-minute fixes.
 *
 * Revision 6.3  2003/09/18 16:17:23  bert
 * Use fabs instead of ABS
 *
 * Revision 6.2  2001/04/17 18:40:14  neelin
 * Modifications to work with NetCDF 3.x
 * In particular, changed NC_LONG to NC_INT (and corresponding longs to ints).
 * Changed NC_UNSPECIFIED to NC_NAT.
 * A few fixes to the configure script.
 *
 * Revision 6.1  1999/10/19 14:45:12  neelin
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
 * Revision 2.2  1995/02/08  19:14:44  neelin
 * More changes for irix 5 lint.
 *
 * Revision 2.1  1995/02/08  19:01:06  neelin
 * Moved private function declarations from minc_routines.h to appropriate file.
 *
 * Revision 2.0  1994/09/28  10:38:21  neelin
 * Release of minc version 0.2
 *
 * Revision 1.9  94/09/28  10:37:22  neelin
 * Pre-release
 * 
 * Revision 1.8  93/11/05  09:18:08  neelin
 * Improved epsilon calculation for valid range checking.
 * 
 * Revision 1.7  93/10/28  15:12:06  neelin
 * Fixed fillvalue checking stuff in MI_convert_type.
 * 
 * Revision 1.6  93/10/28  10:19:16  neelin
 * Added an epsilon for fillvalue checking in routine MI_convert_type (for
 * reading through an icv).
 * 
 * Revision 1.5  93/08/11  12:06:32  neelin
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
#include "type_limits.h"

/* Private functions */
PRIVATE int MI_var_action(int ndims, long var_start[], long var_count[], 
                          long nvalues, void *var_buffer, void *caller_data);
PRIVATE int MI_get_sign(nc_type datatype, int sign);




/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_varaccess
@INPUT      : operation - either MI_PRIV_GET or MI_PRIV_PUT, indicating
                 whether the routine should get or put data from/to a
                 cdf file
              cdfid     - cdf file id
              varid     - variable id
              start     - vector of coordinates of corner of hyperslab
              count     - vector of edge lengths of hyperslab
              datatype  - type that calling routine wants (one of the valid
                 netcdf data types, excluding NC_CHAR)
              sign      - sign that the calling routine wants (one of
                 MI_PRIV_SIGNED, MI_PRIV_UNSIGNED, MI_PRIV_DEFAULT).
              bufsize_step - vector of buffer size steps wanted by 
                 caller (MI_var_loop will try, but no guarantees); if
                 NULL, then 1 is assumed. For the first index that cannot be 
                 read in one piece, the allocated buffer will tend to have 
                 the count of as a multiple of the corresponding value in 
                 this vector.
              icvp      - pointer to icv structure (image conversion variable)
                 If NULL, then icvp->do_scale and icvp->do_dimconvert are
                 assumed to be FALSE.
                 icvp->do_scale        - boolean indicating whether scaling
                    should be done. If so, then 
                       outvalue = icvp->scale * (double) invalue + icvp->offset
                 icvp->scale           - (see do_scale)
                 icvp->offset          - (see do_scale)
                 icvp->do_dimconvert   - boolean indicating whether the
                    dimension conversion routine should be called
                 icvp->dimconvert_func - dimension conversion routine
              values    - values to store in variable (for put)
@OUTPUT     : values    - values to get from variable (for get)
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Routine to do work for getting/putting and converting 
              the type of variable values. Similar to routine ncvarget/
              ncvarput but the calling routine specifies the form in 
              which data should be returned/passed (datatype), as well as 
              the sign. The datatype can only be a numeric type. If the 
              variable in the file is of type NC_CHAR, then an error is 
              returned. Values can optionally be scaled (for image
              conversion routines) by setting icvp->do_scale to TRUE and 
              using icvp->scale and icvp->offset. Dimensional conversion
              can be done be setting icvp->do_dimconvert to TRUE and
              passing a function to be called (icvp->dimconvert_func).
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
SEMIPRIVATE int MI_varaccess(int operation, int cdfid, int varid, 
                             long start[], long count[],
                             nc_type datatype, int sign, void *values,
                             int *bufsize_step, mi_icv_type *icvp)
{
   mi_varaccess_type strc;    /* Structure of values for functions */
   int ndims;                 /* Number of variable dimensions */
   char stringa[MI_MAX_ATTSTR_LEN];  /* String for attribute value */
   char *string = stringa;
   int oldncopts;             /* Save old value of ncopts */

   MI_SAVE_ROUTINE_NAME("MI_varaccess");

   /* Check to see if ivc structure was passed and set variables
      needed by this routine */
   if (icvp == NULL) {
      strc.do_scale      = FALSE;
      strc.do_dimconvert = FALSE;
      strc.do_fillvalue  = FALSE;
   }
   else {
      strc.do_scale      = icvp->do_scale;
      strc.do_dimconvert = icvp->do_dimconvert;
      strc.do_fillvalue  = icvp->do_fillvalue;
   }

   /* Inquire about the variable */
   MI_CHK_ERR(ncvarinq(cdfid, varid, NULL, &(strc.var_type), 
                       &ndims, NULL, NULL))

   /* Check that the variable type is numeric */
   if ((datatype==NC_CHAR) || (strc.var_type==NC_CHAR)) {
      MI_LOG_ERROR(MI_MSG_VARNOTNUM);
      MI_RETURN(MI_ERROR);
   }

   /* Try to find out the sign of the variable using MIsigntype.
      To avoid programs dying unexpectedly, we must change ncopts,
      then restore it */
   oldncopts =get_ncopts();
   set_ncopts(0);
   string=miattgetstr(cdfid, varid, MIsigntype, MI_MAX_ATTSTR_LEN, string);
   set_ncopts(oldncopts);

   /* Get the signs */
   strc.var_sign  = MI_get_sign_from_string(strc.var_type, string);
   strc.call_sign = MI_get_sign(datatype, sign);

   /* Check to see if the type requested is the same as the variable type,
      the signs are the same and no dimension conversion is needed. If so, 
      just get/put the values */
   if ((datatype == strc.var_type) && (strc.call_sign == strc.var_sign) && 
                !strc.do_scale && !strc.do_dimconvert && !strc.do_fillvalue) {
      switch (operation) {
      case MI_PRIV_GET:
         MI_CHK_ERR(ncvarget(cdfid, varid, start, count, values))
         break;
      case MI_PRIV_PUT:
         MI_CHK_ERR(ncvarput(cdfid, varid, start, count, values))
         break;
      default:
         MI_LOG_ERROR(MI_MSG_BADOP);
         MI_RETURN(MI_ERROR);
      }
      MI_RETURN(MI_NOERROR);
   }

   /* Otherwise, we have to loop through data. Set up structure
      and call MI_var_loop */
   strc.operation=operation;
   strc.cdfid=cdfid;
   strc.varid=varid;
   strc.call_type=datatype;
   strc.var_value_size=nctypelen(strc.var_type);
   strc.call_value_size=nctypelen(strc.call_type);
   strc.icvp=icvp;
   strc.start=start;
   strc.count=count;
   strc.values=values;
   MI_CHK_ERR( MI_var_loop(ndims, start, count, 
                           strc.var_value_size, bufsize_step,
                           MI_MAX_VAR_BUFFER_SIZE, 
                           (void *) &strc, MI_var_action) )
   MI_RETURN(MI_NOERROR);
   
}



/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_var_action
@INPUT      : ndims       - number of dimensions
              var_start   - coordinate vector of corner of hyperslab
              var_count   - vector of edge lengths of hyperslab
              nvalues     - number of values in hyperslab
              var_buffer  - pointer to variable buffer
              caller_data - pointer to data from MI_varaccess
@OUTPUT     : (none)
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Buffer action routine to be called by MI_var_loop, for
              use by MI_varaccess.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 30, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_var_action(int ndims, long var_start[], long var_count[], 
                          long nvalues, void *var_buffer, void *caller_data)
     /* ARGSUSED */
{
   mi_varaccess_type *ptr;   /* Pointer to data from MI_varaccess */
   int status;               /* Status returned by function call */

   MI_SAVE_ROUTINE_NAME("MI_var_action");

   ptr=(mi_varaccess_type *) caller_data;

   /* Get/put values and do conversions, etc. */
   switch (ptr->operation) {
   case MI_PRIV_GET:
      status=ncvarget(ptr->cdfid, ptr->varid, var_start, var_count, 
                      var_buffer);
      if (status != MI_ERROR) {
         /* If doing dimension conversion, let dimconvert function do all the 
            work, including type conversion */
         if (!ptr->do_dimconvert) {
            status=MI_convert_type(nvalues,
                      ptr->var_type, ptr->var_sign, var_buffer,
                      ptr->call_type, ptr->call_sign, ptr->values,
                      ptr->icvp);
         }
         else {
            status=(*(ptr->icvp->dimconvert_func))(ptr->operation, ptr->icvp, 
                         ptr->start, ptr->count, ptr->values,
                         var_start, var_count, var_buffer);
         }
      }
      break;
   case MI_PRIV_PUT:
      /* If doing dimension conversion, let dimconvert function do all the 
         work, including type conversion */
      if (!ptr->do_dimconvert) {
         status=MI_convert_type(nvalues,
                   ptr->call_type, ptr->call_sign, ptr->values,
                   ptr->var_type, ptr->var_sign, var_buffer,
                   ptr->icvp);
      }
      else {
         status=(*(ptr->icvp->dimconvert_func))(ptr->operation, ptr->icvp, 
                      ptr->start, ptr->count, ptr->values,
                      var_start, var_count, var_buffer);
      }
      if (status != MI_ERROR) {
         status=ncvarput(ptr->cdfid, ptr->varid, var_start, var_count, 
                         var_buffer);
      }
      break;
   default:
      MI_LOG_ERROR(MI_MSG_BADOP);
      status=MI_ERROR;
   }

   /* Check for an error */
   MI_CHK_ERR(status)

   /* Increment the values pointer */
   if (!ptr->do_dimconvert) {
      ptr->values = (void *) ((char *) ptr->values + 
                                   nvalues*ptr->call_value_size);
   }

   MI_RETURN(MI_NOERROR);

}



/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_var_loop
@INPUT      : ndims       - number of dimensions in variable
              start       - vector of coordinates of corner of hyperslab
              count       - vector of edge lengths of hyperslab
              value_size  - size (in bytes) of each value to be buffered
              bufsize_step - vector of buffer size steps wanted by 
                 caller (MI_var_loop will try, but no guarantees); if
                 NULL, then 1 is assumed. For the first index that cannot be 
                 read in one piece, the allocated buffer will tend to have 
                 the count of as a multiple of the corresponding value in 
                 this vector.
              max_buffer_size - maximum size (in bytes) of buffer
              caller_data - pointer to a structure of data to pass to
                 functions
              action_func - function to do something with each buffer
@OUTPUT     : (none)
@RETURNS    : MI_ERROR (=-1) when an error occurs
@DESCRIPTION: Routine to loop through a variable's indices, getting data
              into a buffer and doing something to it. A function pointer
              is passed that will perform these functions on each buffer.
@METHOD     : 
@GLOBALS    : 
@CALLS      : NetCDF and MINC routines
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
SEMIPRIVATE int MI_var_loop(int ndims, long start[], long count[],
                            int value_size, int *bufsize_step,
                            long max_buffer_size,
                            void *caller_data,
                            int (*action_func) (int, long [], long [], 
                                                long, void *, void *))
{
   long nvalues, newnvalues;  /* Number of values in fastest varying dims.
                                 Note that any dimensional subscript variables
                                 should be long */
   int firstdim;              /* First dimension that doesn't fit in buffer */
   long ntimes;               /* Number of firstdim elements that fit in buf */
   void *var_buffer;          /* Pointer to buffer for variable data */
   long var_count[MAX_VAR_DIMS];   /* Count, start and end coordinate */
   long var_start[MAX_VAR_DIMS];   /* vectors for getting buffers */
   long var_end[MAX_VAR_DIMS];
   int i;                     /* Looping variable - only used for dimension
                                 number, not dimension subscript */

   MI_SAVE_ROUTINE_NAME("MI_var_loop");

   /* Find out how much space we need and then allocate a buffer.
      To do this we find out how many dimensions will fit in our
      maximum buffer size. firstdim is the index of the first dimension
      that won't fit. nvalues is the number of values in the first dimensions
      that do fit in the buffer. ntimes is the number of times that the first
      dimensions fit in the buffer. To make things simpler, dimension 0 is
      always considered to not fit, even if it does. */
   nvalues=newnvalues=1;
   for (firstdim=ndims-1; firstdim>=1; firstdim--) {
      newnvalues *= count[firstdim];
      if (newnvalues*value_size > max_buffer_size) break;
      nvalues = newnvalues;
   }
   if (firstdim<0) {               /* Check for 0-dim variable */
      firstdim=0;
      ntimes=1;
   }
   else {
      ntimes = MIN(MI_MAX_VAR_BUFFER_SIZE/(nvalues*value_size),
                   count[firstdim]);
      /* Try to make ntimes an convenient multiple for the caller */
      if ((ntimes != count[firstdim]) && (bufsize_step != NULL)) {
         ntimes = MAX(1, ntimes - (ntimes % bufsize_step[firstdim]));
      }
   }

   /* Allocate space for variable values */
   if ((var_buffer = MALLOC(ntimes*nvalues*value_size, char)) 
                                     == NULL) {
      MI_LOG_ERROR(MI_MSG_OUTOFMEM);
      MI_RETURN(MI_ERROR);
   }

   /* Create a count variable for the var buffer, with 1s for dimensions
      that vary slower than firstdim and count[i] for dimensions that
      vary faster. Set a start variable for the var buffer, equal to start.
      Set an end variable for the var buffer. */
   if (ndims <= 0) {             /* Handle zero-dimension variable */
      var_start[0]=0; var_end[0]=1; var_count[0]=1;
   }
   for (i=0; i<ndims; i++) {
      var_count[i] = (i>firstdim)  ? count[i] : 
                     (i==firstdim) ? ntimes : 1;
      var_start[i] = start[i];
      var_end[i] = start[i] + count[i];
   }
      
   /* Loop through the dimensions, copying buffers, etc. 
      Exit when the slowest varying dimension reaches its limit. */

   while (var_start[0] < var_end[0]) {
      var_count[firstdim] = 
         MIN(ntimes, var_end[firstdim] - var_start[firstdim]);
      
      /* Do the stuff on the buffer */
      if ((*action_func)(ndims, var_start, var_count, 
                         var_count[firstdim]*nvalues, var_buffer,
                         caller_data) == MI_ERROR) {
         FREE(var_buffer);
         MI_RETURN_ERROR(MI_ERROR);
      }

      /* Increment the start counters */
      var_start[firstdim] += var_count[firstdim];
      i=firstdim;
      while ( (i>0) && (var_start[i] >= var_end[i])) {
         var_start[i] = start[i];
         i--;
         var_start[i]++;
      }
      
   }

   /* Free the buffer and return */
   FREE(var_buffer);
   MI_RETURN(MI_NOERROR);
   
}



/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_get_sign_from_string
@INPUT      : type - type of value
              sign - sign of value (one of
                 MI_EMPTY_STRING, MI_SIGNED or MI_UNSIGNED)
@OUTPUT     : (none)
@RETURNS    : either MI_PRIV_SIGNED or MI_PRIV_UNSIGNED
@DESCRIPTION: Converts sign string to either MI_PRIV_SIGNED or 
              MI_PRIV_UNSIGNED, as appropriate, by calling MI_get_sign.
@METHOD     : 
@GLOBALS    : (none)
@CALLS      : MI_get_sign
@CREATED    : July 30, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
SEMIPRIVATE int MI_get_sign_from_string(nc_type datatype, const char *sign)
{
   MI_SAVE_ROUTINE_NAME("MI_get_sign_from_string");

   MI_RETURN(MI_get_sign(datatype,
             (sign == NULL) || (*sign == '\0')      ? MI_PRIV_DEFSIGN :
             (STRINGS_EQUAL(sign, MI_SIGNED))       ? MI_PRIV_SIGNED :
             (STRINGS_EQUAL(sign, MI_UNSIGNED))     ? MI_PRIV_UNSIGNED :
                                                      MI_PRIV_DEFSIGN));
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_get_sign
@INPUT      : type - type of value
              sign - sign of value (one of
                 MI_PRIV_DEFSIGN, MI_PRIV_SIGNED or MI_PRIV_UNSIGNED)
@OUTPUT     : (none)
@RETURNS    : either MI_PRIV_SIGNED or MI_PRIV_UNSIGNED
@DESCRIPTION: Converts sign variable to either MI_PRIV_SIGNED or 
              MI_PRIV_UNSIGNED, as appropriate, if its value is
              MI_PRIV_DEFSIGN, otherwise the value of sign is returned
              as is. The default signs are
                 byte   : unsigned
                 short  : signed
                 int    : signed
                 float  : signed
                 double : signed
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 27, 1992 (Peter Neelin)
@MODIFIED   : 
---------------------------------------------------------------------------- */
PRIVATE int MI_get_sign(nc_type datatype, int sign)
{
   MI_SAVE_ROUTINE_NAME("MI_get_sign");

   MI_RETURN(  ((datatype==NC_FLOAT) || 
                  (datatype==NC_DOUBLE))        ? MI_PRIV_SIGNED :
               ((sign==MI_PRIV_SIGNED) ||
                  (sign==MI_PRIV_UNSIGNED))     ? sign :
               (datatype==NC_BYTE)              ? MI_PRIV_UNSIGNED :
               (datatype==NC_SHORT)             ? MI_PRIV_SIGNED :
               (datatype==NC_INT)               ? MI_PRIV_SIGNED : 
                                                  MI_PRIV_SIGNED );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : MI_convert_type
@INPUT      : number_of_values  - number of values to copy
              intype            - type of input values
              insign            - sign of input values (one of
                 MI_PRIV_DEFSIGN, MI_PRIV_SIGNED or MI_PRIV_UNSIGNED)
              invalues          - vector of values
              outtype           - type of output values
              outsign           - sign of output values
              icvp              - pointer to icv structure (if NULL,
                 then icvp->do_scale is assumed to be FALSE)
                 icvp->do_scale - boolean indicating whether scaling
                    should be done. If so, then 
                       outvalue = icvp->scale * (double) invalue + icvp->offset
                 icvp->scale    - (see do_scale)
                 icvp->offset   - (see do_scale)
@OUTPUT     : outvalues         - output values
@RETURNS    : MI_ERROR if an error occurs
@DESCRIPTION: Converts the invalues to outvalues according to their type.
              Types must be numeric. Values out of range are truncated
              to the nearest value in range. The sign of integer values
              is given by insign and outsign, which must have values
              MI_PRIV_DEFSIGN, MI_PRIV_SIGNED or MI_PRIV_UNSIGNED. 
              If it is MI_PRIV_DEFSIGN then the default signs are
              used (from MI_get_sign) :
                 byte  : unsigned
                 short : signed
                 int   : signed
              Note that if a conversion must take place, then all input 
              values are converted to double. Values can be scaled through
              icvp->scale and icvp->offset by setting icvp->do_scale to TRUE.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 27, 1992 (Peter Neelin)
@MODIFIED   : August 28, 1992 (P.N.)
                 - replaced type conversions with macros
---------------------------------------------------------------------------- */
SEMIPRIVATE int MI_convert_type(long number_of_values,
                                nc_type intype,  int insign,  void *invalues,
                                nc_type outtype, int outsign, void *outvalues,
                                mi_icv_type *icvp)
{
   int inincr, outincr;    /* Pointer increments for arrays */
   int insgn, outsgn;      /* Signs for input and output */
   long i;
   double dvalue=0.0;      /* Temporary double for conversion */
   void *inptr, *outptr;   /* Pointers to input and output values */
   int do_scale;           /* Should scaling be done? */
   int do_fillvalue;       /* Should fillvalue checking be done? */
   double fillvalue;       /* Value to fill with */
   double dmax, dmin;      /* Range of legal values */
   double epsilon;         /* Epsilon for legal values comparisons */

   MI_SAVE_ROUTINE_NAME("MI_convert_type");

   /* Check to see if icv structure was passed and set variables needed */
   if (icvp == NULL) {
      do_scale=FALSE;
      do_fillvalue = FALSE;
      dmax = dmin = 0.0;
      fillvalue = 0.0;
   }
   else {
      do_scale=icvp->do_scale;
      do_fillvalue=icvp->do_fillvalue;
      fillvalue = icvp->user_fillvalue;
      dmax = icvp->fill_valid_max;
      dmin = icvp->fill_valid_min;
      epsilon = (dmax - dmin) * FILLVALUE_EPSILON;
      epsilon = fabs(epsilon);
      dmax += epsilon;
      dmin -= epsilon;
   }

   /* Check the types and get their size */
   if ((intype==NC_CHAR) || (outtype==NC_CHAR)) {
      MI_LOG_ERROR(MI_MSG_VARNOTNUM);
      MI_RETURN(MI_ERROR);
   }
   if (((inincr =nctypelen(intype ))==MI_ERROR) ||
       ((outincr=nctypelen(outtype))==MI_ERROR)) {
      MI_RETURN(MI_ERROR);
   }

   /* Get the sign of input and output values */
   insgn  = MI_get_sign(intype,  insign);
   outsgn = MI_get_sign(outtype, outsign);

   /* Check to see if a conversion needs to be made.
      If not, just copy the memory */
   if ((intype==outtype) && (insgn==outsgn) && !do_scale && !do_fillvalue) {
         (void) memcpy(outvalues, invalues, 
                       (size_t) number_of_values*inincr);
   }
   
   /* Otherwise, loop through */
   else {

      /* Step through values  */
      inptr=invalues; 
      outptr=outvalues;
      for (i=0 ; i<number_of_values; i++) { 

         /* Convert the input value */
         {MI_TO_DOUBLE(dvalue, intype, insgn, inptr)}

         /* Check the value for range and scale the value if necessary */
         if (do_fillvalue && ((dvalue < dmin) || (dvalue > dmax))) {
            dvalue = fillvalue;
         }
         else if (do_scale) {
            dvalue = icvp->scale * dvalue + icvp->offset;
         }

         /* Truncate if necessary and assign the value */
         {MI_FROM_DOUBLE(dvalue, outtype, outsgn, outptr)}

         inptr  = (void *) ((char *)inptr  + inincr);
         outptr = (void *) ((char *)outptr + outincr);

      }           /* End of for loop */

   }              /* End of else */

   MI_RETURN(MI_NOERROR);
   
}
