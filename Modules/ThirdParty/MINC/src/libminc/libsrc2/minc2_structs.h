/** internal minc2 data structures*/
#ifndef MINC2_STRUCTS_H
#define MINC2_STRUCTS_H 

#include "H5public.h"

/************************************************************************
 * ENUMS, STRUCTS, and TYPEDEFS
 ************************************************************************/
/* These structure declarations exist to allow the following typedefs to
 * work.  Since the details of these structures are not meant to be public,
 * the actual structure definitions are in minc2_private.h
 */
struct mivolprops;
struct midimension;
struct mivolume;

/** \typedef mivolumeprops_t 
 * Opaque pointer to volume properties.
 */
typedef struct mivolprops *mivolumeprops_t;


/** \typedef midimhandle_t
 * Opaque pointer to a MINC dimension object.
 */
typedef struct midimension *midimhandle_t;


/** \typedef mihandle_t 
 * The mihandle_t is an opaque type that represents a MINC file object.
 */
typedef struct mivolume *mihandle_t;


/** \typedef milisthandle_t 
 * The milisthandle_t is an opaque type that represents a handle 
 * to iterate through various properties of MINC file object.
 */
typedef void *milisthandle_t;

/**
 * This typedef used to represent the type of an individual voxel <b>as
 * stored</b> by MINC 2.0. 
 */
typedef enum {
  MI_TYPE_ORIGINAL = 0,     /**< MI_ORIGINAL_TYPE */
  MI_TYPE_BYTE = 1,         /**< 8-bit signed integer */
  MI_TYPE_SHORT = 3,        /**< 16-bit signed integer */
  MI_TYPE_INT = 4,          /**< 32-bit signed integer */
  MI_TYPE_FLOAT = 5,        /**< 32-bit floating point */
  MI_TYPE_DOUBLE = 6,       /**< 64-bit floating point */
  MI_TYPE_STRING = 7,       /**< ASCII string */
  MI_TYPE_UBYTE = 100,      /**< 8-bit unsigned integer */
  MI_TYPE_USHORT = 101,     /**< 16-bit unsigned integer */
  MI_TYPE_UINT = 102,       /**< 32-bit unsigned integer */
  MI_TYPE_SCOMPLEX = 1000,  /**< 16-bit signed integer complex */
  MI_TYPE_ICOMPLEX = 1001,  /**< 32-bit signed integer complex */
  MI_TYPE_FCOMPLEX = 1002,  /**< 32-bit floating point complex */
  MI_TYPE_DCOMPLEX = 1003,  /**< 64-bit floating point complex */
  MI_TYPE_UNKNOWN  = -1     /**< when the type is a record */
} mitype_t;

/** \typedef miclass_t
 * This typedef is used to represent the class of the MINC file.  
 *
 * The class specifies the data's interpretation rather than its 
 * storage format. For example, a floating point class implies
 * that the data may be stored as integers but must nonetheless be 
 * scaled into a "real" range before any mathematical operations
 * are performed.  A label class implies that the values of a voxel
 * should be considered to represent a symbol, and therefore many 
 * operations on the voxels would be considered meaningless.
 */
typedef enum {
  MI_CLASS_REAL = 0,            /**< Floating point (default) */
  MI_CLASS_INT = 1,             /**< Integer */
  MI_CLASS_LABEL = 2,           /**< Enumerated (named data values) */
  MI_CLASS_COMPLEX = 3,         /**< Complex (real/imaginary) values */
  MI_CLASS_UNIFORM_RECORD = 4,  /**< Aggregate datatypes consisting of multiple values of the same underlying type. */
  MI_CLASS_NON_UNIFORM_RECORD = 5 /**< Aggregate datatypes consisting of multiple values of potentially differing types (not yet implemented). */
} miclass_t;

/** \typedef midimclass_t
 * Dimensions be members of one of several classes.  The "MI_DIMCLASS_ANY"
 * value is never actually assigned to a dimension.  It is used in the 
 * programming interface to specify that an operation should apply to
 * all dimensions regardless of class.
 */
typedef enum {
  MI_DIMCLASS_ANY = 0,        /**< Don't care (or unknown) */
  MI_DIMCLASS_SPATIAL = 1,    /**< Spatial dimensions (x, y, z) */
  MI_DIMCLASS_TIME = 2,       /**< Time dimension */
  MI_DIMCLASS_SFREQUENCY = 3, /**< Spatial frequency dimensions */
  MI_DIMCLASS_TFREQUENCY = 4, /**< Temporal frequency dimensions */
  MI_DIMCLASS_USER = 5,       /**< Arbitrary user-defined dimension */
  MI_DIMCLASS_RECORD = 6      /**< Record as dimension */
} midimclass_t;

/** \typedef miorder_t
 * Dimension order refers to the idea that data can be structured in 
 * a variety of ways with respect to the dimensions.  For example, a typical
 * 3D scan could be structured as a transverse (ZYX) or sagittal (XZY) image.
 * Since it may be convenient to write code which expects a particular 
 * dimension order, a user can specify an alternative ordering by using 
 * miset_apparent_dimension_order().  This will cause most functions
 * to return data as if the file was in the apparent, rather than the
 * file (native) order.
 */
typedef enum {
  MI_DIMORDER_FILE      = 0,
  MI_DIMORDER_APPARENT  = 1
} miorder_t;

/** \typedef midimalign_t
 * Dimension alignment - one of CENTRE, START, END
 */
typedef enum {
  MI_DIMALIGN_CENTRE = 0,
  MI_DIMALIGN_START = 1,
  MI_DIMALIGN_END = 2
} midimalign_t;

/** \typedef mivoxel_order_t
 * Voxel order can be either file (native), or apparent, as set by
 * the function miset_dimension_apparent_voxel_order().
 */
typedef enum {
  MI_ORDER_FILE      = 0,       /**< File order */
  MI_ORDER_APPARENT  = 1        /**< Apparent (user) order  */
} mivoxel_order_t;

/** \typedef miflipping_t
 * Voxel flipping can be specified to either follow the file's native
 * order, the opposite of the file's order, or it can be tied to the
 * value of the dimension's step attribute.  A value of MI_NEGATIVE
 * implies that the voxel order should be rearranged such that the step
 * attribute is negative, a value of MI_POSITIVE implies the opposite.
 */
typedef enum {
  MI_FILE_ORDER         = 0,    /**< no flip */
  MI_COUNTER_FILE_ORDER = 1,    /**< flip */
  MI_POSITIVE           = 2,    /**< force step value to be positive */
  MI_NEGATIVE           = 3     /**< force step value to be negative */
} miflipping_t;

/** \typedef micompression_t
 * Compression type
 */
typedef enum {
  MI_COMPRESS_NONE = 0,         /**< No compression */
  MI_COMPRESS_ZLIB = 1          /**< GZIP compression */
} micompression_t;

/** \typedef miboolean_t
 * Boolean value
 */
typedef int miboolean_t;

/** \typedef midimattr_t
 * Something about dimension attributes
 */
typedef unsigned int midimattr_t;

/** \typedef misize_t
 * size of things
 */
typedef hsize_t misize_t; /*based on HDF5 size*/

/**  \typedef miscomplex_t
 * 16-bit integer complex voxel.
 */
typedef struct {
  short real;                   /**< Real part */
  short imag;                   /**< Imaginary part */
} miscomplex_t;

/** \typedef miicomplex_t
 * 32-bit integer complex voxel.
 */
typedef struct {
  int real;                     /**< Real part */
  int imag;                     /**< Imaginary part */
} miicomplex_t;

/** \typedef mifcomplex_t
 * 32-bit floating point complex voxel.
 */
typedef struct {
  float real;                   /**< Real part */
  float imag;                   /**< Imaginary part */
} mifcomplex_t;

/** \typedef midcomplex_t
 * 64-bit floating point complex voxel.
 */
typedef struct {
  double real;                  /**< Real part */
  double imag;                  /**< Imaginary part */
} midcomplex_t;

#endif //MINC2_STRUCTS_H
