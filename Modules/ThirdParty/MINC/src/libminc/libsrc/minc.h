#ifndef MINC_H
#define MINC_H

/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc.h
@DESCRIPTION: Header file for minc (Medical Image NetCDF) file format 
              standard.
@METHOD     : Includes name definitions for NetCDF dimensions, attributes
              and variables for the following :
                 NetCDF standard attributes
                 MI general variable attributes
                 MI dimensions and associated variables
                 MI dimension variable attributes
                 MI root variable
                 MI image variable
                 MI patient variable
                 MI study variable
                 MI acquisition variable
@CREATED    : July 24, 1992. (Peter Neelin, Montreal Neurological Institute)
@MODIFIED   : 
 * $Log: minc.h,v $
 * Revision 6.21  2010-05-19 03:44:25  stever
 * Move definition of MNCAPI ahead of #include "minc_compat.h"; fix-up for
 * previous check-in.
 *
 * Revision 6.20  2010-05-19 03:18:42  stever
 * Ensure hdf5.h included before netcdf.h.
 *
 * Revision 6.19  2010-03-02 23:24:40  rotor
 *  * libsrc/hdf_convenience.c: removed spurious debug output
 *  * libsrc/minc.h: replaced MAX_NC_OPEN with 32
 *  * libsrc/voxel_loop.c: replaced MAX_NC_OPEN with MI_MAX_NUM_ICV
 *
 * Revision 6.18  2007/08/09 17:05:25  rotor
 *  * added some fixes of Claudes for chunking and internal compression
 *
 * Revision 6.17  2005/08/26 21:04:57  bert
 * Use #if rather than #ifdef with MINC2 symbol
 *
 * Revision 6.16  2004/12/03 21:52:35  bert
 * Minor changes for Windows build
 *
 * Revision 6.15  2004/10/15 13:48:13  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.14  2004/08/11 20:50:54  bert
 * Fix incompatibility with netCDF 3.5.1 by fixing MI_MAX_IMGDIMS at 100
 *
 * Revision 6.13  2004/06/04 18:14:52  bert
 * Add micreate_ident()
 *
 * Revision 6.12  2004/04/27 15:44:04  bert
 * Add MINC 2.0 specific stuff
 *
 * Revision 6.11  2004/04/15 21:13:21  bert
 * Add C++ linkage specifier
 *
 * Revision 6.10  2004/02/02 18:22:34  bert
 * Added miget_version() and miappend_history()
 *
 * Revision 6.9  2003/03/17 16:03:28  bert
 * Added declaration of micreate_tempfile()
 *
 * Revision 6.8  2001/11/13 14:15:17  neelin
 * Added functions miget_image_range and mivar_exists
 *
 * Revision 6.7  2001/08/20 13:19:14  neelin
 * Added function miattget_with_sign to allow the caller to specify the sign
 * of the input attribute since this information is ambiguous. This is
 * necessary for the valid_range attribute which should have the same sign
 * as the image data. Modified miget_valid_range to make use of this function.
 *
 * Revision 6.6  2001/08/16 16:41:31  neelin
 * Added library functions to handle reading of datatype, sign and valid range,
 * plus writing of valid range and setting of default ranges. These functions
 * properly handle differences between valid_range type and image type. Such
 * difference can cause valid data to appear as invalid when double to float
 * conversion causes rounding in the wrong direction (out of range).
 * Modified voxel_loop, volume_io and programs to use these functions.
 *
 * Revision 6.5  2001/08/16 13:32:18  neelin
 * Partial fix for valid_range of different type from image (problems
 * arising from double to float conversion/rounding). NOT COMPLETE.
 *
 * Revision 6.4  2001/04/24 13:38:40  neelin
 * Replaced NC_NAT with MI_ORIGINAL_TYPE.
 *
 * Revision 6.3  2001/04/17 18:40:13  neelin
 * Modifications to work with NetCDF 3.x
 * In particular, changed NC_LONG to NC_INT (and corresponding longs to ints).
 * Changed NC_UNSPECIFIED to NC_NAT.
 * A few fixes to the configure script.
 *
 * Revision 6.2  2001/04/10 22:05:30  neelin
 * Start of modifications to get minc working with netcdf 3.5.
 *
 * Revision 6.1  1999/10/19 14:45:08  neelin
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
 * Revision 3.1  1995/06/12  17:37:05  neelin
 * Added MI_LABEL modality.
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.4  1995/01/24  08:34:30  neelin
 * Added optional tempfile argument to miexpand_file.
 *
 * Revision 2.3  95/01/23  08:28:31  neelin
 * Changed name of midecompress_file to miexpand_file.
 * 
 * Revision 2.2  95/01/20  15:21:16  neelin
 * Added midecompress_file with ability to decompress only the header of a file.
 * 
 * Revision 2.1  94/11/25  15:32:32  neelin
 * Added #undef for public if it wasn't previously defined so that C++
 * code won't get upset.
 * 
 * Revision 2.0  94/09/28  10:38:00  neelin
 * Release of minc version 0.2
 * 
 * Revision 1.29  94/09/28  10:37:24  neelin
 * Pre-release
 * 
 * Revision 1.28  93/11/03  13:08:37  neelin
 * Added prototypes for miopen, miclose, micreate.
 * 
 * Revision 1.27  93/11/03  12:29:11  neelin
 * Added error code for failure to uncompress a file.
 * 
 * Revision 1.26  93/08/11  12:06:34  neelin
 * Added RCS logging in source.
 * 
              July 15, 1993 (P.N.)
                 - added MI_ICV_DO_FILLVALUE and MI_FILLVALUE
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
@RCSID      : $Header: /private-cvsroot/minc/libsrc/minc.h,v 6.21 2010-05-19 03:44:25 stever Exp $ MINC (MNI)
---------------------------------------------------------------------------- */

#ifndef MNCAPI
#if defined(_MSC_VER)
/* If we are building on the Microsoft C compiler, we want to
 * explicitly import all public functions from the DLL
 */
#define MNCAPI __declspec(dllimport)
#else
#define MNCAPI
#endif /* _MSC_VER not defined */
#endif /* MNCAPI not defined */

#if MINC2
#include <hdf5.h>
#include <netcdf.h>
#include "minc_compat.h"
#else
#include <netcdf.h>
#endif

#include <minc_common_defs.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* For compatibility with NetCDF 2.x which uses NC_LONG rather than NC_INT.
   Since NC_INT is defined in an enum, and since there are no version macros,
   we use NC_FILL_INT as an indicator of whether NC_INT is defined. */
#ifndef NC_FILL_INT
#  define NC_INT NC_LONG
#endif

/* Constant used with nc_type variables to indicate that the data type 
   should be taken from the source object or file. This replaces earlier
   use of NC_UNSPECIFIED and is equivalent to NetCDF NC_NAT in version 3.5
   and later. */
#define MI_ORIGINAL_TYPE ((nc_type) 0) 

/* NC_UNSPECIFIED is defined here for backwards compatibility. With 
   NetCDF 2.x, NC_UNSPECIFIED may already be defined either through a macro
   or an enum. In the latter case, this macro will override the enum. */
#ifndef NC_UNSPECIFIED
#  define NC_UNSPECIFIED MI_ORIGINAL_TYPE
#endif

/* Some useful constants */
#define MI_EMPTY_STRING ""
/* Error flags */
#define MI_ERROR (-1)
#define MI_NOERROR 0
/* Maximum length of standard attributes */
#define MI_MAX_ATTSTR_LEN  64
/* Number of spatial dimensions */
#define MI_NUM_SPACE_DIMS 3
/* Maximum number of image dimensions for image conversion */

/* Bert 10-Aug-2004 - MI_MAX_IMGDIMS used to be defined to be MAX_VAR_DIMS,
 * a constant defined in netcdf.h. For many years MAX_VAR_DIMS was 100,
 * but in netCDF 3.5.1 the value was changed to 512.
 * Unfortunately, the definitions of MI_ICV_DIM_SIZE, MI_ICV_DIM_STEP,
 * and MI_ICV_DIM_START assume that MI_MAX_IMGDIMS is less than or
 * equal to 100.  To avoid changing the MINC API, we have to define
 * MI_MAX_IMGDIMS to 100 here.  Otherwise the miicv_inqdbl() function
 * will return bogus values for these ICV properties.
 */
#define MI_MAX_IMGDIMS 100

/* NetCDF standard attributes */
#define MIunits       "units"
#define MIlong_name   "long_name"
#define MIvalid_range "valid_range"
#define MIvalid_max   "valid_max"
#define MIvalid_min   "valid_min"
#define MI_FillValue  "_FillValue"
#define MItitle       "title"
#define MIhistory     "history"

/* General variable attributes */
#define MIvartype  "vartype"
#define MIvarid    "varid"
#define MIsigntype "signtype"
#define MIparent   "parent"
#define MIchildren "children"
#define MIcomments "comments"
#define MIversion  "version"

/* General attribute constants */
/*    Prefix for identifying a variable attribute pointer */
#define MI_VARATT_POINTER_PREFIX "--->"
/*    Separator for elements of MIchildren */
#define MI_CHILD_SEPARATOR "\n"
/*    MIvartype values */
#define MI_GROUP     "group________"
#define MI_DIMENSION "dimension____"
#define MI_DIM_WIDTH "dim-width____"
#define MI_VARATT    "var_attribute"
/*    MIvarid value */
#define MI_STDVAR "MINC standard variable"
/*    MIsigntype values */
#define MI_SIGNED   "signed__"
#define MI_UNSIGNED "unsigned"
/*    MIversion value */
#define MI_VERSION_1_0 "MINC Version    1.0"
#define MI_CURRENT_VERSION MI_VERSION_1_0
/* Generally useful values for boolean attributes */
#define MI_TRUE  "true_"
#define MI_FALSE "false"

/* Dimension names and names of associated variables */
#define MIxspace           "xspace"
#define MIyspace           "yspace"
#define MIzspace           "zspace"
#define MItime             "time"
#define MItfrequency       "tfrequency"
#define MIxfrequency       "xfrequency"
#define MIyfrequency       "yfrequency"
#define MIzfrequency       "zfrequency"
#define MIvector_dimension "vector_dimension"
#define MIxspace_width     "xspace-width"
#define MIyspace_width     "yspace-width"
#define MIzspace_width     "zspace-width"
#define MItime_width       "time-width"
#define MItfrequency_width "tfrequency-width"
#define MIxfrequency_width "xfrequency-width"
#define MIyfrequency_width "yfrequency-width"
#define MIzfrequency_width "zfrequency-width"

/* Dimension variable attribute names */
/* For dimension variables (MIspacing is also for dimension width vars) */
#define MIspacing           "spacing"
#define MIstep              "step"
#define MIstart             "start"
#define MIspacetype         "spacetype"
#define MIalignment         "alignment"
#define MIdirection_cosines "direction_cosines"
/* For dimension width variables */
#define MIwidth             "width"
#define MIfiltertype        "filtertype"

/* Dimension attribute constants */
/*    MIgridtype values */
#define MI_REGULAR   "regular__"
#define MI_IRREGULAR "irregular"
/*    MIspacetype values */
#define MI_NATIVE    "native____"
#define MI_TALAIRACH "talairach_"
#define MI_CALLOSAL  "callosal__"
/*    MIalignment values */
#define MI_START  "start_"
#define MI_CENTRE "centre"
#define MI_END    "end___"
#define MI_CENTER MI_CENTRE
/*    MIfiltertype values */
#define MI_SQUARE     "square____"
#define MI_GAUSSIAN   "gaussian__"
#define MI_TRIANGULAR "triangular"

/* The root variable */
#define MIrootvariable "rootvariable"

/* The image variable and its attributes */
#define MIimage    "image"
#define MIimagemax "image-max"
#define MIimagemin "image-min"
#define MIcomplete "complete"

/* The patient variable and its attributes */
#define MIpatient        "patient"
#define MIfull_name      "full_name"
#define MIother_names    "other_names"
#define MIidentification "identification"
#define MIother_ids      "other_ids"
#define MIbirthdate      "birthdate"
#define MIsex            "sex"
#define MIage            "age"
#define MIweight         "weight"
#define MIsize           "size"
#define MIaddress        "address"
#define MIinsurance_id   "insurance_id"

/* Patient attribute constants */
#define MI_MALE   "male__"
#define MI_FEMALE "female"
#define MI_OTHER  "other_"

/* The study variable and its attributes */
#define MIstudy               "study"
#define MIstart_time          "start_time"
#define MIstart_year          "start_year"
#define MIstart_month         "start_month"
#define MIstart_day           "start_day"
#define MIstart_hour          "start_hour"
#define MIstart_minute        "start_minute"
#define MIstart_seconds       "start_seconds"
#define MImodality            "modality"
#define MImanufacturer        "manufacturer"
#define MIdevice_model        "device_model"
#define MIinstitution         "institution"
#define MIdepartment          "department"
#define MIstation_id          "station_id"
#define MIreferring_physician "referring_physician"
#define MIattending_physician "attending_physician"
#define MIradiologist         "radiologist"
#define MIoperator            "operator"
#define MIadmitting_diagnosis "admitting_diagnosis"
#define MIprocedure           "procedure"
#define MIstudy_id            "study_id"

/* Study attribute constants */
#define MI_PET   "PET__"
#define MI_SPECT "SPECT"
#define MI_GAMMA "GAMMA"
#define MI_MRI   "MRI__"
#define MI_MRS   "MRS__"
#define MI_MRA   "MRA__"
#define MI_CT    "CT___"
#define MI_DSA   "DSA__"
#define MI_DR    "DR___"
#define MI_LABEL "label"

/* The acquisition variable and its attributes */
#define MIacquisition           "acquisition"
#define MIprotocol              "protocol"
#define MIscanning_sequence     "scanning_sequence"
#define MIrepetition_time       "repetition_time"
#define MIecho_time             "echo_time"
#define MIinversion_time        "inversion_time"
#define MInum_averages          "num_averages"
#define MIimaging_frequency     "imaging_frequency"
#define MIimaged_nucleus        "imaged_nucleus"
#define MIradionuclide          "radionuclide"
#define MIcontrast_agent        "contrast_agent"
#define MIradionuclide_halflife "radionuclide_halflife"
#define MItracer                "tracer"
#define MIinjection_time        "injection_time"
#define MIinjection_year        "injection_year"
#define MIinjection_month       "injection_month"
#define MIinjection_day         "injection_day"
#define MIinjection_hour        "injection_hour"
#define MIinjection_minute      "injection_minute"
#define MIinjection_seconds     "injection_seconds"
#define MIinjection_length      "injection_length"
#define MIinjection_dose        "injection_dose"
#define MIdose_units            "dose_units"
#define MIinjection_volume      "injection_volume"
#define MIinjection_route       "injection_route"

/* Constants for image conversion variable (icv) properties */

/* This value is not really enforced in the code (see miicv_create() in
 * image_conversion.c), but this value is used in voxel_loop.c to set the
 * maximum number of allowable open files.
 */
#define MI_MAX_NUM_ICV 1000 /**< Maximum number of icv's allowed */

/* Default max and min for normalization */
#define MI_DEFAULT_MAX 1.0
#define MI_DEFAULT_MIN 0.0
/* For converting data type */
#define MI_ICV_TYPE             1
#define MI_ICV_SIGN             2
#define MI_ICV_DO_RANGE         3
#define MI_ICV_VALID_MAX        4
#define MI_ICV_VALID_MIN        5
/* For doing normalization */
#define MI_ICV_DO_NORM          6
#define MI_ICV_USER_NORM        7
#define MI_ICV_IMAGE_MAX        8
#define MI_ICV_IMAGE_MIN        9
/* Values actually used in normalization - read-only */
#define MI_ICV_NORM_MAX        10
#define MI_ICV_NORM_MIN        11
/* For doing dimension conversions */
#define MI_ICV_DO_DIM_CONV     12
/* For converting vector fields to scalar */
#define MI_ICV_DO_SCALAR       13
/* For flipping axis direction */
#define MI_ICV_XDIM_DIR        14
#define MI_ICV_YDIM_DIR        15
#define MI_ICV_ZDIM_DIR        16
/* For changing size of first two dimensions (excluding MIvector_dimension) */
#define MI_ICV_ADIM_SIZE       17
#define MI_ICV_BDIM_SIZE       18
#define MI_ICV_KEEP_ASPECT     19
/* The pixel size and location of first two dimensions (these are readonly) */
#define MI_ICV_ADIM_STEP       20
#define MI_ICV_BDIM_STEP       21
#define MI_ICV_ADIM_START      22
#define MI_ICV_BDIM_START      23
/* Number of image dimensions for dimension conversion */
#define MI_ICV_NUM_IMGDIMS     24
/* Number of dimensions of image variable taking into account vector/scalar
   data (read-only property) */
#define MI_ICV_NUM_DIMS        25
/* Id of file and image variable (read-only properties) */
#define MI_ICV_CDFID           26
#define MI_ICV_VARID           27
/* Names of MIimagemax and MIimagemin variables */
#define MI_ICV_MAXVAR          28
#define MI_ICV_MINVAR          29
/* For setting input values to a specified fillvalue */
#define MI_ICV_DO_FILLVALUE    30
#define MI_ICV_FILLVALUE       31
/* Image dimension properties. For each dimension, add the dimension 
   number (counting from fastest to slowest). */
#define MI_ICV_DIM_SIZE        1000
#define MI_ICV_DIM_STEP        1100
#define MI_ICV_DIM_START       1200

/* Constants that can be used as values for the above properties. */
/* Possible values for MI_ICV_?DIM_DIR */
#define MI_ICV_POSITIVE         1
#define MI_ICV_NEGATIVE       (-1)
#define MI_ICV_ANYDIR           0
/* Possible value for MI_ICV_?DIM_SIZE */
#define MI_ICV_ANYSIZE        (-1)

/* Error codes.
   Note that they must not conflict with NetCDF error codes since
   they are stored in the same global variable. */
#define MI_ERR_NONNUMERIC       1331  /* Non-numeric type */
#define MI_ERR_NONCHAR          1332  /* Non-character type */
#define MI_ERR_NONSCALAR        1333  /* Non-scalar attribute */
#define MI_ERR_BADOP            1334  /* Bad operation for MI_varaccess */
#define MI_ERR_NOTPOINTER       1335  /* Attribute is not a pointer */
#define MI_ERR_BAD_STDVAR       1336  /* Not a standard variable */
#define MI_ERR_BADSUFFIX        1337  /* Bad dimension width suffix */
#define MI_ERR_NOICV            1338  /* Out of icv slots */
#define MI_ERR_BADICV           1339  /* Illegal icv identifier */
#define MI_ERR_BADPROP          1340  /* Unknown icv property */
#define MI_ERR_ICVATTACHED      1341  /* Tried to modify attached icv */
#define MI_ERR_TOOFEWDIMS       1342  /* Too few dimensions to be an image */
#define MI_ERR_ICVNOTATTACHED   1343  /* Tried to access an unattached icv */
#define MI_ERR_DIMSIZE          1344  /* Dimensions differ in size */
#define MI_ERR_ICV_INVCOORDS    1345  /* Invalid icv coordinates */
#define MI_ERR_WRONGNDIMS       1346  /* Too many dimensions for a dim var */
#define MI_ERR_BADMATCH         1347  /* Variables do not match for copy */
#define MI_ERR_MAXMIN_DIMS      1348  /* Imagemax/min variables vary over
                                         image dimensions */
#define MI_ERR_UNCOMPRESS       1349  /* Not able to uncompress file */

/* MINC public functions */

/* From netcdf_convenience.c */
MNCAPI char *miexpand_file(const char *path, char *tempfile, 
                           int header_only, int *created_tempfile);
MNCAPI int miopen(const char *path, int mode);
MNCAPI int micreate(const char *path, int cmode);
MNCAPI int miclose(int cdfid);
MNCAPI int miattget_with_sign(int cdfid, int varid, const char *name, 
                              char *insign, nc_type datatype, char *outsign,
                              int max_length, void *value, int *att_length);
MNCAPI int miattget(int cdfid, int varid, const char *name, nc_type datatype,
                    int max_length, void *value, int *att_length);
MNCAPI int miattget1(int cdfid, int varid, const char *name, nc_type datatype,
                     void *value);
MNCAPI char *miattgetstr(int cdfid, int varid, const char *name,
                         int maxlen, char *value);
MNCAPI int miattputint(int cdfid, int varid, const char *name, int value);
MNCAPI int miattputdbl(int cdfid, int varid, const char *name, double value);
MNCAPI int miattputstr(int cdfid, int varid, const char *name, const char *value);
MNCAPI int mivarget(int cdfid, int varid, long start[], long count[],
                    nc_type datatype, const char *sign, void *values);
MNCAPI int mivarget1(int cdfid, int varid, long mindex[],
                     nc_type datatype, const char *sign, void *value);
MNCAPI int mivarput(int cdfid, int varid, long start[], long count[],
                    nc_type datatype, const char *sign, void *values);
MNCAPI int mivarput1(int cdfid, int varid, long mindex[],
                     nc_type datatype, const char *sign, void *value);
MNCAPI long *miset_coords(int nvals, long value, long coords[]);
MNCAPI long *mitranslate_coords(int cdfid, 
                                int invar,  long incoords[],
                                int outvar, long outcoords[]);
MNCAPI int micopy_all_atts(int incdfid, int invarid, 
                           int outcdfid, int outvarid);
MNCAPI int micopy_var_def(int incdfid, int invarid, int outcdfid);
MNCAPI int micopy_var_values(int incdfid, int invarid, 
                             int outcdfid, int outvarid);
MNCAPI int micopy_all_var_defs(int incdfid, int outcdfid, int nexclude,
                               int excluded_vars[]);
MNCAPI int micopy_all_var_values(int incdfid, int outcdfid, int nexclude,
                                 int excluded_vars[]);
MNCAPI char *micreate_tempfile(void);

MNCAPI int set_ncopts(int new_ncopts);
MNCAPI int get_ncopts(void);

MNCAPI int push_ncopts(int new_ncopts);
MNCAPI int pop_ncopts(void);


/* From minc_convenience.c */
MNCAPI int miget_datatype(int cdfid, int imgid, 
                          nc_type *datatype, int *is_signed);
MNCAPI int miget_default_range(nc_type datatype, int is_signed, 
                               double default_range[]);
MNCAPI int miget_valid_range(int cdfid, int imgid, double valid_range[]);
MNCAPI int miset_valid_range(int cdfid, int imgid, const double valid_range[]);
MNCAPI int miget_image_range(int cdfid, double image_range[]);
MNCAPI int mivar_exists(int cdfid, const char *varname);
MNCAPI int miattput_pointer(int cdfid, int varid, const char *name, int ptrvarid);
MNCAPI int miattget_pointer(int cdfid, int varid, const char *name);
MNCAPI int miadd_child(int cdfid, int parent_varid, int child_varid);
MNCAPI int micreate_std_variable(int cdfid, const char *name, nc_type datatype, 
                                 int ndims, int dim[]);
MNCAPI int micreate_group_variable(int cdfid, const char *name);
MNCAPI const char *miget_version(void);
MNCAPI int miappend_history(int fd, const char *tm_stamp);
MNCAPI int micreate_ident(char * id_str, size_t length);

/* From image_conversion.c */
MNCAPI int miicv_create(void);
MNCAPI int miicv_free(int icvid);
MNCAPI int miicv_setdbl(int icvid, int icv_property, double value);
MNCAPI int miicv_setint(int icvid, int icv_property, int value);
MNCAPI int miicv_setlong(int icvid, int icv_property, long value);
MNCAPI int miicv_setstr(int icvid, int icv_property, const char *value);
MNCAPI int miicv_inqdbl(int icvid, int icv_property, double *value);
MNCAPI int miicv_inqint(int icvid, int icv_property, int *value);
MNCAPI int miicv_inqlong(int icvid, int icv_property, long *value);
MNCAPI int miicv_inqstr(int icvid, int icv_property, char *value);
MNCAPI int miicv_ndattach(int icvid, int cdfid, int varid);
MNCAPI int miicv_detach(int icvid);
MNCAPI int miicv_get(int icvid, long start[], long count[], void *values);
MNCAPI int miicv_put(int icvid, long start[], long count[], void *values);

/* From dim_conversion.c */
MNCAPI int miicv_attach(int icvid, int cdfid, int varid);

/* From minc_error.c */
MNCAPI void milog_init(const char *);
MNCAPI int milog_set_verbosity(int);

/* from minc_format_convert.h*/
MNCAPI int minc_format_convert(const char *input,const char *output);
/* default voxel loop buffer size */
#define MI2_DEF_BUFF_SIZE 4096
#define MI2_DEF_MAX_MEM 104857

#if MINC2

/* New functions, not directly part of compatibility layer. */
extern int MI2varsize(int fd, int varid, long *size_ptr);

extern int miget_file_type(const char *filename);

#define MI2_GRPNAME "/minc-2.0"
/* These must not interfere with any NC_ flags we might have to support. */
#define MI2_CREATE_V2 0x1000    /* Force V2 format */
#define MI2_CREATE_V1 0x2000    /* Force V1 format */

/* Possible compression type values. */
#define MI2_COMP_UNKNOWN (-1)
#define MI2_COMP_NONE 0
#define MI2_COMP_ZLIB 1

#define MI2_CHUNK_UNKNOWN (-1)
#define MI2_CHUNK_OFF 0
#define MI2_CHUNK_ON 1
#define MI2_CHUNK_MIN_SIZE 4

#define MI2_CHECKSUM_OFF 0
#define MI2_CHECKSUM_ON  1

#define MI2_OPTS_V1 1

struct mi2opts {
    int struct_version;
    int comp_type;
    int comp_param;
    int chunk_type;
    int chunk_param;
    int checksum;
};

/* This is hackish in that it assumes that all NetCDF handles returned
 * by ncopen/nccreate will be small integers. Historically this is
 * true, but it could break someday. We used to use an HDF5 call, but
 * that broke when HDF5 switched from 32 to 64 bits for hid_t.
 */
#define HDF5_ID_MIN 0x30000000
#define MI2_ISH5OBJ(x) (x >= HDF5_ID_MIN)

MNCAPI int micreatex(const char *path, int cmode, struct mi2opts *opts_ptr);

#else
#define MI2_ISH5OBJ(x) (0)
#endif /* MINC2 */

#ifdef __cplusplus
}
#endif /* __cplusplus */

/* End ifndef MINC_H */
#endif
