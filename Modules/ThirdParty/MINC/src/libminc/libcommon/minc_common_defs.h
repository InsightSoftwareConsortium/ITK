/** minc2 definitions*/
#ifndef MINC_COMMON_DEFS_H
#define MINC_COMMON_DEFS_H

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

/************************************************************************
 * CONSTANTS
 ************************************************************************/

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
 * Unfortunately, the definitions of MI2_ICV_DIM_SIZE, MI2_ICV_DIM_STEP,
 * and MI2_ICV_DIM_START assume that MI_MAX_IMGDIMS is less than or
 * equal to 100.  To avoid changing the MINC API, we have to define
 * MI_MAX_IMGDIMS to 100 here.  Otherwise the miicv_inqdbl() function
 * will return bogus values for these ICV properties.
 */
#define MI_MAX_IMGDIMS 100
#define MI2_MAX_IMGDIMS 100

/* Epsilon for detecting fillvalues */
#define MI2_FILLVALUE_EPSILON (10.0 * FLT_EPSILON)


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


/* Error codes.
 *   Note that they must not conflict with NetCDF error codes since
 *   they are stored in the same global variable. */
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


#ifndef MI_NOERROR
/** Generic return code for successful operations. */
#define MI_NOERROR 0
#endif /* MI_NOERROR not defined */

#ifndef MI2_NOERROR
/** Generic return code for successful operations. */
#define MI2_NOERROR MI_NOERROR
#endif /* MI2_NOERROR not defined */


#ifndef MI_ERROR
/** Generic return code for operations which fail for any reason. */
#define MI_ERROR (-1)
#endif /* MI_ERROR not defined */

#ifndef MI2_ERROR
/** Generic return code for operations which fail for any reason. */
#define MI2_ERROR MI_ERROR
#endif /* MI2_ERROR not defined */


#define MI_NATIVE    "native____"
#define MI_TALAIRACH "talairach_"
#define MI_CALLOSAL  "callosal__"

#ifndef TRUE
#define TRUE  1
#endif /* TRUE */

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

/** World spatial coordinates should always have this structure.
 */
#define MI2_3D 3
#define MI2_X 0
#define MI2_Y 1
#define MI2_Z 2

/** Dimension attribute values.
 */
#define MI_DIMATTR_ALL 0
#define MI_DIMATTR_REGULARLY_SAMPLED 0x1
#define MI_DIMATTR_NOT_REGULARLY_SAMPLED 0x2

/** Maximum length of a standard string.
 */
#define MI2_CHAR_LENGTH 128

/** Maximum number of dimensions a variable can have.
 */
#define MI2_MAX_VAR_DIMS 100

/**
 * Maximum length of dimension name TODO: check in HDF5 documentation
 * */
#define MI2_MAX_DIM_NAME 256

#define MI2_CHUNK_SIZE 32 /* Length of chunk, per dimension */
#define MI2_DEFAULT_ZLIB_LEVEL 4
#define MI2_MAX_ZLIB_LEVEL 9

#define MI2_MAX_PATH 128
#define MI2_MAX_RESOLUTION_GROUP 16

#define MI2_OPEN_READ 0x0001
#define MI2_OPEN_RDWR 0x0002

#define MI_VERSION_2_0 "MINC Version    2.0"


/**
 * MINC2 ICV
 * 
 * */
#define MI2_PRIV_DEFSIGN   0
#define MI2_PRIV_SIGNED    1
#define MI2_PRIV_UNSIGNED  2

/* Operations for MI_varaccess */
#define MI2_PRIV_GET 10
#define MI2_PRIV_PUT 11


/* Constants for image conversion variable (icv) properties */
/* Maximum number of icv's allowed */
#define MI2_MAX_NUM_ICV 1000    /**< Currently, this is never used. */

/* Default max and min for normalization */
#define MI2_DEFAULT_MAX 1.0
#define MI2_DEFAULT_MIN 0.0
/* For converting data type */
#define MI2_ICV_TYPE             1
#define MI2_ICV_SIGN             2
#define MI2_ICV_DO_RANGE         3
#define MI2_ICV_VALID_MAX        4
#define MI2_ICV_VALID_MIN        5
/* For doing normalization */
#define MI2_ICV_DO_NORM          6
#define MI2_ICV_USER_NORM        7
#define MI2_ICV_IMAGE_MAX        8
#define MI2_ICV_IMAGE_MIN        9
/* Values actually used in normalization - read-only */
#define MI2_ICV_NORM_MAX        10
#define MI2_ICV_NORM_MIN        11
/* For doing dimension conversions */
#define MI2_ICV_DO_DIM_CONV     12
/* For converting vector fields to scalar */
#define MI2_ICV_DO_SCALAR       13
/* For flipping axis direction */
#define MI2_ICV_XDIM_DIR        14
#define MI2_ICV_YDIM_DIR        15
#define MI2_ICV_ZDIM_DIR        16
/* For changing size of first two dimensions (excluding MIvector_dimension) */
#define MI2_ICV_ADIM_SIZE       17
#define MI2_ICV_BDIM_SIZE       18
#define MI2_ICV_KEEP_ASPECT     19
/* The pixel size and location of first two dimensions (these are readonly) */
#define MI2_ICV_ADIM_STEP       20
#define MI2_ICV_BDIM_STEP       21
#define MI2_ICV_ADIM_START      22
#define MI2_ICV_BDIM_START      23
/* Number of image dimensions for dimension conversion */
#define MI2_ICV_NUM_IMGDIMS     24
/* Number of dimensions of image variable taking into account vector/scalar
   data (read-only property) */
#define MI2_ICV_NUM_DIMS        25
/* Id of file and image variable (read-only properties) */
#define MI2_ICV_CDFID           26
#define MI2_ICV_VARID           27
/* Names of MIimagemax and MIimagemin variables */
#define MI2_ICV_MAXVAR          28
#define MI2_ICV_MINVAR          29
/* For setting input values to a specified fillvalue */
#define MI2_ICV_DO_FILLVALUE    30
#define MI2_ICV_FILLVALUE       31
/* Image dimension properties. For each dimension, add the dimension 
   number (counting from fastest to slowest). */
#define MI2_ICV_DIM_SIZE        1000
#define MI2_ICV_DIM_STEP        1100
#define MI2_ICV_DIM_START       1200

/* Constants that can be used as values for the above properties. */
/* Possible values for MI2_ICV_?DIM_DIR */
#define MI2_ICV_POSITIVE         1
#define MI2_ICV_NEGATIVE       (-1)
#define MI2_ICV_ANYDIR           0
/* Possible value for MI2_ICV_?DIM_SIZE */
#define MI2_ICV_ANYSIZE        (-1)



/**
 * Error handling macros
 * take from minc
 * */
#define MI2_SAVE_ROUTINE_NAME(name) MI2_save_routine_name(name)
#define MI2_RETURN(value) \
   return( MI2_return() ? (value) : (value) )
#define MI2_RETURN_ERROR(error) \
   return( MI2_return_error() ? (error) : (error) )
#define MI2_LOG_PKG_ERROR2(p1,p2) MI2_log_pkg_error2(p1, p2)
#define MI2_LOG_PKG_ERROR3(p1,p2,p3) MI2_log_pkg_error3(p1, p2, p3)
#define MI2_LOG_SYS_ERROR1(p1) MI2_log_sys_error1(p1)
#define MI2_CHK_ERR(expr) {if ((expr)<0) MI2_RETURN_ERROR(MI_ERROR);}

/*#define MICFG_FORCE_V2 "MINC_FORCE_V2"
#define MICFG_COMPRESS "MINC_COMPRESS"
#define MICFG_CHUNKING "MINC_CHUNKING"
#define MICFG_LOGFILE  "MINC_LOGFILE"
#define MICFG_LOGLEVEL "MINC_LOGLEVEL"
#define MICFG_MAXBUF   "MINC_MAX_FILE_BUFFER_KB"
#define MICFG_MAXMEM   "MINC_MAX_MEMORY_KB"*/


/* DUMMY rootvariable ID */
#define MI2_ROOTVARIABLE_ID (NC_MAX_VARS + 1) /* Impossible value */


/* stupid macro which was never put to use*/
#ifndef _
#define _(x) x      /* For future gettext */
#endif 

/* Force P_tmpdir to be something reasonable. */
#if !defined(P_tmpdir)
#define P_tmpdir "/var/tmp"
#endif /* P_tmpdir not defined */


#endif /*MINC_COMMON_DEFS_H*/

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
