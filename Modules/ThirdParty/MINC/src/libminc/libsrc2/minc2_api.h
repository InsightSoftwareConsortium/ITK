/**
 * \file minc2_api.h
 * MINC2 API FUNCTION DECLARATIONS
 **/

#ifndef MINC2_API_H
#define MINC2_API_H

#ifdef __cplusplus
extern "C" {               /* Hey, Mr. Compiler - this is "C" code! */
#endif /* __cplusplus defined */


/** \defgroup mi2Group ATTRIBUTE/GROUP FUNCTIONS */

/** Start listing the objects in a group.
 * \ingroup mi2Group
 */
int milist_start(mihandle_t vol, const char *path, int flags,
                        milisthandle_t *handle);

/** Iterate through attributes
 * \ingroup mi2Group
 */
int milist_attr_next(mihandle_t vol, milisthandle_t handle, 
                            char *path, int maxpath,
                            char *name, int maxname);

/** Finish listing attributes or groups
 * \ingroup mi2Group
 */
int milist_finish(milisthandle_t handle);

/** Get the group at given path
 * \ingroup mi2Group
 */
int milist_grp_next(milisthandle_t handle, char *path, int maxpath);

/** Create a group at "path" using "name".
 * \ingroup mi2Group
 */
int micreate_group(mihandle_t vol, const char *path, const char *name);

/** Delete the named attribute.
 * \ingroup mi2Group
 */
int midelete_attr(mihandle_t vol, const char *path, const char *name);

/** Delete the subgroup \a name from the group \a path
 * \ingroup mi2Group
 */
int midelete_group(mihandle_t vol, const char *path, const char *name);

/**
 * Returns the length in bytes of the attribute \a name in the
 * group or dataset at \a path. For strings, this length will reflect
 * the number of characters actually stored, which may not include the
 * terminating null character.
 * \ingroup mi2Group
 */
int miget_attr_length(mihandle_t vol, const char *path, 
                             const char *name, size_t *length);

/** Get the type of an attribute.
 * \ingroup mi2Group
 */
int miget_attr_type(mihandle_t vol, const char *path, const char *name,
                           mitype_t *data_type);

/** Copy all attribute given a path
 * \ingroup mi2Group
 */
int micopy_attr(mihandle_t vol, const char *path, mihandle_t new_vol);

/** Get the values of an attribute.
 * Note: for MI_TYPE_STRING data_type, the length and values buffer
 * should include space for the null termination.
 * \ingroup mi2Group
 */
int miget_attr_values(mihandle_t vol, mitype_t data_type,
                             const char *path, const char *name, 
                             size_t length, void *values);

/** Set the values of an attribute.
 * \ingroup mi2Group
 */
int miset_attr_values(mihandle_t vol, mitype_t data_type,
                             const char *path, const char *name, size_t length,
                             const void *values);

/** Add global history attribute
 * \ingroup mi2Group
 */
int miadd_history_attr(mihandle_t vol, size_t length, const void *values);

/** \defgroup mi2Memory FREE FUNCTIONS */

/**
 * Free space allocated for string storage by a MINC function.
 * \param name_ptr A pointer to the space to be freed.
 * \ingroup mi2Memory
 */
int mifree_name(char *name_ptr);

/**
 * Free list of names
 * not certain we really need this...
 * \ingroup mi2Memory
*/
int mifree_names(char **name_pptr);

/** \defgroup mi2DataType DATA TYPE/SPACE FUNCTIONS */

/** Return the data class of a volume (See miclass_t).
 * \ingroup mi2DataType
 */
int miget_data_class(mihandle_t vol, miclass_t *volume_class);

/** Return the data type of a volume (See mitype_t).
 * \ingroup mi2DataType
 */
int miget_data_type(mihandle_t vol, mitype_t *volume_data_type);

/** Return the byte size of the voxel datatytpe
 * \ingroup mi2DataType
 */
int miget_data_type_size(mihandle_t vol, misize_t *voxel_size);

/** Return the minc space type, name should be freed after use
 * \ingroup mi2DataType
 */
int miget_space_name(mihandle_t vol, char **name);

/**
 * Set minc space type
 * \ingroup mi2DataType
 */
int miset_space_name(mihandle_t vol, const char *name);

/** \ingroup mi2Dim DIMENSION FUNCTIONS */

/**
  * Figure out whether a dimension is associated with a volume.
  * \param dimension The dimension handle.
  * \param volume    A pointer to the volume handle.
  *
  * This method returns the volume handle associated with a given dimension
  * or an error if the specified handle is not associated with the volume.
  * \ingroup mi2Dim
  */
int miget_volume_from_dimension(midimhandle_t dimension, mihandle_t *volume);

/**
  * Create a copy of a given dimension.
  * \param dim_ptr The dimension handle of the dimension to copy.
  * \param new_dim_ptr A pointer to the dimension handle of the copied dimension.
  *
  * This method creates a copy of the specified dimension and returns the handle
  * to the copied dimension or error on failure.
  * \ingroup mi2Dim
 */
int micopy_dimension(midimhandle_t dim_ptr, midimhandle_t *new_dim_ptr);

/**
  * Define a new dimension in a MINC volume.
  * \param name A pointer to the string specifying the dimension name.
  * \param dimclass The class of the dimension.
  * \param attr  The attribute of the dimension.
  * \param length The size of the dimension.
  * \param new_dim_ptr A pointer to the dimension handle.
  *
  * This function defines a dimension that can be used in the definition
  * of a new MINC volume (see the create_volume function).  The name may
  * be an arbitrary string of up to 128 alphanumeric characters. Any of
  * the "standard" names retained from MINC 1.0 retain their default
  * behaviors: MIxspace, MIyspace, and MIzspace default to spatial
  * dimensions, and MItime default to be a time dimension.  MItfrequency
  * is a temporal frequency axis, and MIxfrequency, MIyfrequency, and
  * MIzfrequency are spatial frequency axes.  Any other name may be used.
  *
  * When initially defined, a regularly-sampled dimension will have a
  * "start" value of zero, and a "separation" or "step" value of 1.0.  An
  * irregular dimension will be initialized with all offsets equal to
  * zero.
  *
  * The size of the dimension may range from 0 to 2^32, which should provide
  * enough range to represent detail on the order of 10 Angstroms in
  * typical medical imaging applications.
  *
  * For the detailed defintion of \a class and \a type refer to the MINC 2.0 API
  * definition.
  * \ingroup mi2Dim
  */
int micreate_dimension(const char *name, midimclass_t dimclass, midimattr_t attr, 
                              misize_t length, midimhandle_t *new_dim_ptr);

/**
  * Delete the dimension definition.
  * \param dim_ptr The dimension handle.
  *
  * Note: The original document stated that a dimension has to be
  * associated with a given volume before it can be deleted. This
  * feature was erased from the document and not considered here.
  * \ingroup mi2Dim
  */
int mifree_dimension_handle(midimhandle_t dim_ptr);

/** Retrieve the list of dimensions defined in a MINC volume,
 * with the same class \a class and attribute \a attr.
 * \param volume The volume handle.
 * \param dimclass  The class of the dimensions.
 * \param attr   The attribute of the dimensions.
 * \param order  The order of the dimension (file or apparent).
 * \param array_length The number of dimension to be retrieved.
 * \param dimensions An array of dimension handles to be retrieved.
 *
 * This function is used to retrieve an array of dimension handles for a
 * MINC volume.  It will place the handles of the first "array_length"
 * dimensions into the "dimensions[]" array, returning only those dimension
 * whose characteristics match the "class" and "attr" parameters.
 * The miorder_t is an enumerated type flag which determines whether the
 * dimension order is determined by the file or by the apparent order set by
 * the user. This function will fail if the user has not set the apparent
 * dimension order by calling either of
 * (miset_apparent_dimension_order(_by_name))
 * before calling this function with MI_DIMORDER_APPARENT flag.
 * \ingroup mi2Dim
 */
int miget_volume_dimensions(mihandle_t volume, midimclass_t dimclass, midimattr_t attr,
                                   miorder_t order, int array_length, 
                                   midimhandle_t dimensions[]);


/**
 * Set apparent dimension order, based on an array of dimensions.  You
 * may also set the dimension order by the name of the dimension, see
 * miset_apparent_dimension_order_by_name().
 * \param volume The volume handle.
 * \param array_length The number of dimensions to be sorted.
 * \param dimensions An "ordered" array of dimension handles.
 *
 * This method sets an apparent dimension order. The user can sort the
 * dimensions in any desired order. If the user specifies fewer dimensions
 * than the existing ones, then they are assumed to be added to the last.
 * \ingroup mi2Dim
 */
int miset_apparent_dimension_order(mihandle_t volume, int array_length, midimhandle_t dimensions[]);


/**
  * Set apparent dimension order by name.
  * \param volume The volume handle.
  * \param array_length The number of dimensions to be sorted.
  * \param names An "ordered" array of dimension names.
  *
  * This method sets an apparent dimension order by dimension name. Note that
  * all dimension names must be different or an error occurs.
  * \ingroup mi2Dim
  */
int miset_apparent_dimension_order_by_name(mihandle_t volume, int array_length, char **names);


/**
  *Set the record flag and add a record dimension to the volume
  * dimensions so the volume would appear to have an extra dimension.
  * \param volume The volume handle.
  * \param record_flag The flag to determine whether there exist a record dimension
  * in the volume.
  *
  * This method causes a volume to appear to have a record dimension. The record
  * dimension will be set to uniform and flat (i.e., the volume will appear to have
  * an extra dimension)
 * \ingroup mi2Dim
 */
int miset_apparent_record_dimension_flag(mihandle_t volume, int record_flag);


/**
  * Get the apparent order of voxels (i.e., the order that voxel indices increase/decrease)
  * \param dimension The dimension handle
  * \param file_order The order of voxels.
  * \param sign The sign of the step value.
  *
  * This method gets the apparent order of voxels for the specified dimension
  * and the sign of the step values.
  * \ingroup mi2Dim
  */
int miget_dimension_apparent_voxel_order(midimhandle_t dimension, miflipping_t *file_order,
                                                miflipping_t *sign);

/**
 * Set the apparent order of voxels.
 * \param dimension The dimension handle.
 * \param flipping_order The order of voxels.
 *
 * This method sets the apparent order of voxels for the specified dimension.
 * For the detailed description of voxel order refer to the MINC 2.0 API definition.
 * \ingroup mi2Dim
 */
int miset_dimension_apparent_voxel_order(midimhandle_t dimension, miflipping_t flipping_order);


/**
 * Get the class of a MINC dimension.
 * \param dimension The dimension handle.
 * \param dimclass A pointer to the dimension class.
 *
 * The "class" of a MINC dimension defines the general type of a dimension,
 * whether it is a spatial dimension, a time dimension, or a frequency dimension
 * as transformed from either space or time.  User-defined dimension are also
 * permitted, with no default handling assumed. Finally, a record can be specified
 * as a dimension.
 * \ingroup mi2Dim
 */
int miget_dimension_class(midimhandle_t dimension, midimclass_t *dimclass);


/**
  * Set the class of a MINC dimension.
  * \param dimension The dimension handle.
  * \param dimclass The dimension class.
  *
  * Refer to miget_dimension_class().
  * \ingroup mi2Dim
  */
int miset_dimension_class(midimhandle_t dimension, midimclass_t dimclass);


/**
  * Get the direction cosine vector of a given SPATIAL dimension.
  * \param dimension The dimension handle.
  * \param direction_cosines An array of direction_cosines(i.e., vector determining the direction cosine).
  *
  * Spatial dimension in MINC volumes may be associated with a vector of direction
  * cosines which define the precise orientation of the axis relative to "true"
  * x, y, or z coordinates.
  * \ingroup mi2Dim
  */
int miget_dimension_cosines(midimhandle_t dimension, 
                                   double direction_cosines[3]);

/**
  * Set the direction cosine vector for a given SPATIAL dimension.
  * \param dimension The dimension handle.
  * \param direction_cosines An array of direction_cosines(i.e., vector determining the direction cosine).
  *
  * Refer to miget_dimension_cosines().
  * \ingroup mi2Dim
  */
int miset_dimension_cosines(midimhandle_t dimension, 
                                   const double direction_cosines[3]);

/**
  * Set the comments attribute for a given dimension.
  * \param dimension The dimension handle.
  * \param comments A pointer for the comments.
  *
  * Refer to miget_dimension_description().
  * \ingroup mi2Dim
  */
int miset_dimension_description(midimhandle_t dimension, const char *comments);


/**
 * Get the comments attribute for a given dimension.
 * \param dimension The dimension handle.
 * \param comments_ptr A string pointer for the comments.
 *
 * Get and Set the dimension description. Note that the spatial dimensions
 * (xspace, yspace, zspace) are initialized according to minc1 description.
 * All other dimensions will have an empty description unless set by the user.
 * The string pointer returned in \a *comments_ptr must be freed by the caller.
 * \ingroup mi2Dim
 */
int miget_dimension_description(midimhandle_t dimension, char **comments_ptr);


/**
  * Get the identifier (name) of a MINC dimension.
  * \param dimension The dimension handle.
  * \param name_ptr A string pointer for returning the dimension name.
  *
  * Retrieves the name of the given dimension.
  * \ingroup mi2Dim
  */
int miget_dimension_name(midimhandle_t dimension, char **name_ptr);


/**
 * Set the identifier (name) of a given MINC dimension.
 * \param dimension The dimension handle.
 * \param name A pointer for the dimension name.
 *
 * Rename the given dimension.
 * \ingroup mi2Dim
 */
int miset_dimension_name(midimhandle_t dimension, const char *name);

/**
  * Get the untransformed world coordinates of points along a MINC dimension.
  * \param dimension The dimension handle.
  * \param array_length The number of dimensions.
  * \param start_position The position in which to retrieve the offsets.
  * \param offsets The array of offsets to be returned.
  *
  * Get or Set the dimension offsets, that is, the
  * absolute world coordinates of each sampled point along the dimension.
  *
  * The caller may retrieve up to "array_length" values, starting at the
  * integer index "start_position".  Thus an arbitrary contiguous subset
  * of the dimension's offsets may be retrieved or stored.  An error is
  * returned if the "start_position" exceeds the total size of the
  * dimension.  If the value of "start_position" is legal, but the sum of
  * "start_position" and "array_length" exceeds the size of the dimension,
  * the function will get or set offsets up to the size of the dimension.
  * Any extra positions in the offsets[] array will be ignored.
  * \ingroup mi2Dim
  */
int miget_dimension_offsets(midimhandle_t dimension, misize_t array_length, 
                                   misize_t start_position, double offsets[]);

/**
  * Set the absolute world coordinates of points along a MINC dimension.
  * \param dimension The dimension handle.
  * \param array_length The number of dimensions.
  * \param start_position The position in which to retrieve the offsets.
  * \param offsets The array of offsets to be set.
  *
  * Refer to miget_dimension_offsets().
  * \ingroup mi2Dim
  */
int miset_dimension_offsets(midimhandle_t dimension, misize_t array_length, 
                                   misize_t start_position, const double offsets[]);

/**
  * Get the sampling flag for a MINC dimension.
  * \param dimension The dimension handle.
  * \param sampling_flag The flag to determine regular/irregular sampling dimensions.
  *
  * This flag is true (non-zero) if the dimension is sampled at regular
  * intervals, and false if the dimension is sampled irregularly.
  * If a dimension has regular sampling, the miget_dimension_separation()
  * may be used to retrieve the sampling interval, and the
  * miget_dimension_start() may be used to retrieve the origin
  * value along the axis.
  *
  * If a dimension has irregular sampling, the miget_dimension_offsets()
  * may be used to retrieve the positions of each sample along that axis.
  * \ingroup mi2Dim
  */
int miget_dimension_sampling_flag(midimhandle_t dimension, miboolean_t *sampling_flag);


/**
  * Set the sampling flag for a MINC dimension.
  * \param dimension The dimension handle.
  * \param sampling_flag The flag to determine regular/irregular sampling dimensions.
  *
  * Refer to miget_dimension_sampling_flag().
  * \ingroup mi2Dim
  */
int miset_dimension_sampling_flag(midimhandle_t dimension, miboolean_t sampling_flag);


/**
 * Get the constant sampling interval (step) for a single dimension.
 * \param dimension The dimension handle.
 * \param voxel_order The order in which the voxel indices increase/decrease.
 * \param separation_ptr The Pointer to the dimension sampling interval (step).
 *
 * Gets or sets the constant sampling interval defined on a regularly-sampled
 * dimension. While it is legal to call these functions for an irregularly-
 * sampled dimension, the values will be ignored.
 * \ingroup mi2Dim
 */
int miget_dimension_separation(midimhandle_t dimension, 
                                      mivoxel_order_t voxel_order, 
                                      double *separation_ptr);


/**
  * Set the sampling interval (step) for a single dimension.
  * \param dimension The dimension handle.
  * \param separation The dimension sampling interval (step).
  *
  * Refer to miget_dimension_separation().
  * \ingroup mi2Dim
  */
int miset_dimension_separation(midimhandle_t dimension, 
                                      double separation);

/**
  * Get the sampling interval (STEP) for a list of dimensions.
  * \param dimensions An array of dimension handles.
  * \param voxel_order The order in which the voxel indices increase/decrease.
  * \param array_length The number of dimensions in the dimesions array.
  * \param separations An array of dimensions sampling intervals (step) values.
  *
  * Get or Set the scalar separation (sampling interval)
  * associated with each of the dimensions in the input "dimensions[]"
  * array.  The "array_length" parameter specifies the size of both the
  * input and output arrays. While it is legal to call these functions for
  * an irregularly-sampled dimension, the values will be ignored.
  * \ingroup mi2Dim
  */
int miget_dimension_separations(const midimhandle_t dimensions[], 
                                       mivoxel_order_t voxel_order, 
                                       misize_t array_length, 
                                       double separations[]);


/**
  * Set the sampling interval (STEP) for a list of dimensions.
  * \param dimensions An array of dimension handles.
  * \param array_length The number of dimensions in the dimesions array.
  * \param separations An array of dimensions sampling intervals (step) values.
  *
  * Refer to miget_dimension_separations().
  * \ingroup mi2Dim
  */
int miset_dimension_separations(const midimhandle_t dimensions[], misize_t array_length,
                                       const double separations[]);

/**
  * Get the length of a MINC dimension.
  * \param dimension The dimension handle.
  * \param size_ptr A pointer to the dimension size.
  *
  * Get or Set the size (or length) of a MINC 2 dimension
  * object used in creating a new volume.  The size of a dimension
  * associated with an existing volume cannot be changed.
  * \ingroup mi2Dim
  */
int miget_dimension_size(midimhandle_t dimension, misize_t *size_ptr);

/**
  * Set the length of a MINC dimension if not associated with a volume.
  * \param dimension The dimension handle.
  * \param size  The size of the dimension.
  *
  * Refer to miget_dimension_size().
  * \ingroup mi2Dim
  */
int miset_dimension_size(midimhandle_t dimension, misize_t size);


/**
  * Retrieve the length of all dimensions in dimensions array.
  * \param dimensions An array of dimension handles.
  * \param array_length The number of dimensions in the dimensions array
  * \param sizes An array of dimension sizes.
  *
  * This function will copy the lengths of each of the dimensions listed in the
  * "dimensions[]" array into the "sizes[]" array.  The parameter "array_length"
  * specifies the length of both of the arrays.
  * \ingroup mi2Dim
  */
int miget_dimension_sizes(const midimhandle_t dimensions[], misize_t array_length,
                                misize_t sizes[]);

/**
  * Get the start value of a MINC dimension.
  * \param dimension The dimension handle.
  * \param voxel_order The order in which the voxel indices increase/decrease.
  * \param start_ptr A pointer to the start value.
  *
  * Get or set the origin of the dimension in world
  * coordinates. While a "start" value may be legally associated with any
  * dimension, it is considered meaningless when associated with an
  * irregularly sampled dimension.
  * \ingroup mi2Dim
  */
int miget_dimension_start(midimhandle_t dimension, 
                          mivoxel_order_t voxel_order,
                          double *start_ptr);

/** 
  * Set the start of a MINC dimension.
  * \param dimension The dimension handle.
  * \param start_ptr The start of the dimension.
  *
  * Refer to miget_dimension_start().
  * \ingroup mi2Dim
  */
int miset_dimension_start(midimhandle_t dimension, double start_ptr);

/**
  * Get the start values for MINC dimensions in dimensions array.
  * \param dimensions The array of dimension handles.
  * \param voxel_order The order in which the voxel indices increase/decrease.
  * \param array_length The number of dimensions in the dimensions array.
  * \param starts The array of dimension starts.
  *
  * Get or Set the start value for an array of
  * regularly-sampled dimensions.  The start value defines the origin of
  * that dimension.  While it is legal to call these functions for an
  * irregularly-sampled dimension, the values will be ignored.
  * \ingroup mi2Dim
  */
int miget_dimension_starts(const midimhandle_t dimensions[], mivoxel_order_t voxel_order,
                                  misize_t array_length, double starts[]);

/**
  * Set the start values for MINC dimensions in dimensions array.
  * \param dimensions The array of dimension handles.
  * \param array_length The number of dimensions in the dimensions array.
  * \param starts The array of dimension starts.
  *
  * Refer to miget_dimension_starts().
  * \ingroup mi2Dim
  */
int miset_dimension_starts(const midimhandle_t dimensions[], misize_t array_length, 
                                  const double starts[]);

/**
  * Get the unit string for a MINC dimension.
  * \param dimension The dimension handle.
  * \param units_ptr A string pointer to the dimension units.
  *
  * Retrieves the units of the given dimension,
  * The caller must free the string returned by this function.
  * \ingroup mi2Dim
  */
int miget_dimension_units(midimhandle_t dimension, char **units_ptr);

/**
 * Set the unit string for a MINC dimension.
 * \param dimension The dimension handle.
 * \param units A pointer to the dimension units.
 *
 * Set the units for an existing dimension.
 * The new string must be no greater than 128 characters in length,
 * including the trailing zero byte.
 * \ingroup mi2Dim
 */
int miset_dimension_units(midimhandle_t dimension, const char *units);

/**
  * Get A single full-width half-maximum value from a
  * regularly sampled dimension.
  * \param dimension The dimension handle.
  * \param width_ptr A pointer to the FWHM value.
  *
  * Get or Set the dimension width, that is, the
  * full-width half-maximum values of each sampled point along the dimension.
  * These functions are used to set a constant width for regularly-sampled
  * dimensions.
  * \ingroup mi2Dim
  */
int miget_dimension_width(midimhandle_t dimension, double *width_ptr);

/**
  * Set the A single full-width half-maximum value for a
  * regularly sampled dimension.
  * \param dimension The dimension handle.
  * \param width_ptr The FWHM value.
  *
  * Refer to miget_dimension_width().
  * \ingroup mi2Dim
  */
int miset_dimension_width(midimhandle_t dimension, double width_ptr);

/**
  * Get the full-width half-maximum value for points along an
  * irregularly sampled dimension.
  * \param dimension The dimension handle.
  * \param voxel_order The order in which the voxel indices increase/decrease.
  * \param array_length The number of width in the widths array.
  * \param start_position Index for starting position.
  * \param widths An array of width values to be retrieved.
  *
  * Get or Set the dimension widths, that is, the
  * full-width half-maximum values of each sampled point along the
  * dimension.
  * The caller may retrieve up to "array_length" values, starting at the
  * integer index "start_position".  Thus an arbitrary contiguous subset
  * of the dimension's widths may be retrieved or stored.  An error is
  * returned if the "start_position" exceeds the total size of the
  * dimension.  If the value of "start_position" is legal, but the sum of
  * "start_position" and "array_length" exceeds the size of the dimension,
  * the function will get or set widths up to the size of the dimension.
  * Any extra positions in the widths[] array will be ignored.
  * \ingroup mi2Dim
  */
int miget_dimension_widths(midimhandle_t dimension, mivoxel_order_t voxel_order,
                                  misize_t array_length, misize_t start_position,
                                  double widths[]);


/**
  * Set the full-width half-maximum value for points along an
  * irregularly sampled dimension.
  * \param dimension The dimension handle.
  * \param array_length The number of width in the widths array.
  * \param start_position Index for starting position.
  * \param widths An array of width values to be set.
  *
  * Refer to miget_dimension_widths().
  * \ingroup mi2Dim
  */
int miset_dimension_widths(midimhandle_t dimension, misize_t array_length,
                                  misize_t start_position, const double widths[]);


/* VOLUME FUNCTIONS */
/** Create a volume with the specified name, dimensions,
    type, class, volume properties and retrieve the volume handle.
    \ingroup mi2Vol
*/
int micreate_volume(const char *filename, 
                           int number_of_dimensions,
                           midimhandle_t dimensions[],
                           mitype_t volume_type,
                           miclass_t volume_class,
                           mivolumeprops_t create_props,
                           mihandle_t *volume);

/** Create the actual image for the volume.
  * Note that the image dataset muct be created in the hierarchy
  * before the image data can be added.
  * \ingroup mi2Vol
*/
int micreate_volume_image(mihandle_t volume);


/** Return the number of dimensions associated with this volume.
  * \ingroup mi2Vol
*/
int miget_volume_dimension_count(mihandle_t volume, midimclass_t dimclass,
                                        midimattr_t attr, int *number_of_dimensions);

/** Return the number of slice dimensions associated with this volume.
 *  it's only usefull for slice normalization
 *  will return the total number of volume dimensions if slice normalization is not used
 * \ingroup mi2Vol
*/
int miget_slice_dimension_count(mihandle_t volume, midimclass_t dimclass,
                                        midimattr_t attr, int *number_of_dimensions);

/** Returns the number of voxels in the volume.
  * \ingroup mi2Vol
*/
int miget_volume_voxel_count(mihandle_t volume, misize_t *number_of_voxels);

/** Opens an existing MINC volume for read-only access if mode argument is
  * MI2_OPEN_READ, or read-write access if mode argument is MI2_OPEN_RDWR.
  * \ingroup mi2Vol
*/
int miopen_volume(const char *filename, int mode, mihandle_t *volume);


/** Close an existing MINC volume. If the volume was newly created,
  *  all changes will be written to disk. In all cases this function closes
  *  the open volume and frees memory associated with the volume handle.
  *  \ingroup mi2Vol
*/
int miclose_volume(mihandle_t volume);

/** Function to get the volume's slice-scaling flag.
 */
int miget_slice_scaling_flag(mihandle_t volume, 
                                    miboolean_t *slice_scaling_flag);

/** Function to set the volume's slice-scaling flag.
 */
int miset_slice_scaling_flag(mihandle_t volume, 
                                    miboolean_t slice_scaling_flag);

/** \defgroup mi2VPrp VOLUME PROPERTIES FUNCTIONS */

/** Create a volume property list.  The new list will be returned in the
 * \a props parameter.    When the program is finished 
 * using the property list it should call  mifree_volume_props() to free the
 * memory associated with the list.
 * \param props A pointer to the returned volume properties handle.
 * \ingroup mi2VPrp
 */
int minew_volume_props(mivolumeprops_t *props);

/** Destroy a volume property list.
 * \param props The volume property list to delete.
 * \ingroup mi2VPrp
 */
int mifree_volume_props(mivolumeprops_t props);

/** Get a copy of the volume property list.  When the program is finished 
 * using the property list it should call  mifree_volume_props() to free the
 * memory associated with the list.
 * \param vol A volume handle
 * \param props A pointer to the returned volume properties handle.
 * \ingroup mi2VPrp
 */
int miget_volume_props(mihandle_t vol, mivolumeprops_t *props);


/** Set multi-resolution properties.  The \a enable_flag determines
 * whether or not thumbnail images will be calculated at all.  The \a
 * depth parameter determines the lowest-resolution image that will be
 * available.  The full resolution image is considered to be image #0,
 * the half resolution image is image #1, the quarter-resolution image
 * is #2, etc. Therefore a \a depth value of 2 implies both the half
 * and quarter resolution thumbnails will be calculated and stored in
 * the file.
 * \param props A volume property list handle 
 * \param enable_flag TRUE if multiresolution support should be enabled in
 * this file.  
 * \param depth The maximum depth of multiresolution data
 * to support.
 * \ingroup mi2VPrp
 */
int miset_props_multi_resolution(mivolumeprops_t props, miboolean_t enable_flag, 
                                        int depth);

/** Get multi-resolution properties.  Returns the value of the \a enable_flag
 * and \a depth parameters.
 * \param props A volume property list handle
 * \param enable_flag Pointer to a boolean which will be set to TRUE if 
 * multiresolution has been enabled.
 * \param depth Pointer to a integer which will contain the maximum resolution
 * depth enabled if multiresolution is enabled.
 * \ingroup mi2VPrp
 */
int miget_props_multi_resolution(mivolumeprops_t props, miboolean_t *enable_flag,
                                        int *depth);


/** Select a different resolution from a multi-resolution image.
 * \ingroup mi2VPrp
 */
int miselect_resolution(mihandle_t volume, int depth);


/** Compute or recompute all resolution groups.
 * 
 * \ingroup mi2VPrp
 */
int miflush_from_resolution(mihandle_t volume, int depth);


/** Set compression type for a volume property list
 * Note that enabling compression will automatically 
 * enable blocking with default parameters. 
 * \param props A volume properties list
 * \param compression_type The type of compression to use (MI_COMPRESS_NONE
 * or MI_COMPRESS_ZLIB)
 * \ingroup mi2VPrp
 */
int miset_props_compression_type(mivolumeprops_t props, micompression_t compression_type);


/** Get compression type for a volume property list
 * \param props A volume property list handle
 * \param compression_type A pointer to a variable to which the current
 * compression type will be assigned.
 * \ingroup mi2VPrp
 */
int miget_props_compression_type(mivolumeprops_t props, micompression_t *compression_type);


/** Set zlib compression properties for a volume list.  The \a zlib_level
 * parameter may range from 1 to 9, where higher numbers request that the
 * library attempt to use more memory (and possibly processing power) to
 * achieve the highest possible compression ratio.
 *
 * \param props A volume property list handle
 * \param zlib_level An integer specifying the desired compression level.
 * \ingroup mi2VPrp
 */
int miset_props_zlib_compression(mivolumeprops_t props, int zlib_level);


/** Get zlib compression properties from a volume property list.
 * \param props A volume property list handle
 * \param zlib_level Pointer to an integer variable that will receive the
 * current compression level.
 * \ingroup mi2VPrp
 */
int miget_props_zlib_compression(mivolumeprops_t props, int *zlib_level);


/** Set blocking structure properties for the volume
 * \param props A volume property list handle
 * \param edge_count The number of edges (dimensions) in a block
 * \param edge_lengths The lengths of the edges
 * \ingroup mi2VPrp
 */
int miset_props_blocking(mivolumeprops_t props, int edge_count, const int *edge_lengths);


/** Get blocking structure properties for the volume
 * \param props The properties structure from which to get the information
 * \param edge_count Returns the number of edges (dimensions) in a block
 * \param edge_lengths The lengths of the edges
 * \param max_lengths The number of elements of the edge_lengths array
 * \ingroup mi2VPrp
 */
int miget_props_blocking(mivolumeprops_t props, int *edge_count, int *edge_lengths,
                                int max_lengths);




/** Set checksumming for volume
 * \param on A volume property list handle
 * \ingroup mi2VPrp
 */
int miset_props_checksum(mivolumeprops_t props, int on);


/** Get checksumming for volume
 * \ingroup mi2VPrp
 */
int miget_props_checksum(mivolumeprops_t props, int *on);



/** Set properties for uniform/nonuniform record dimension
 * \ingroup mi2VPrp
 */
int miset_props_record(mivolumeprops_t props, misize_t record_length, char *record_name); 


/** Set the template volume flag
 * \ingroup mi2VPrp
 */ 
int miset_props_template(mivolumeprops_t props, int template_flag);

/** \defgroup mi2Slice SLICE/VOLUME SCALE FUNCTIONS */
/**
 * This function sets \a slice_max to the maximum real value of
 * voxels in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miget_slice_max(mihandle_t volume, 
                           const misize_t start_positions[],
                           size_t array_length, double *slice_max);

/**
 * This function sets minimum real value of
 * values in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miset_slice_max(mihandle_t volume, 
                           const misize_t start_positions[],
                           size_t array_length, double slice_max);


/**
 * This function sets \a slice_min to the minimum real value of
 * voxels in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miget_slice_min(mihandle_t volume, 
                           const misize_t start_positions[],
                           size_t array_length, double *slice_min);


/**
 * This function sets minimum real value of
 * values in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miset_slice_min(mihandle_t volume, 
                           const misize_t start_positions[],
                           size_t array_length, double slice_min);


/**
 * This function gets both the minimum and
 * maximum real value of voxels in the slice containing the coordinates
 * \a start_positions.  The \a array_length may be less than or equal to
 * the number of dimensions in the volume, extra coordinates will be
 * ignored.  Specifying too few coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miget_slice_range(mihandle_t volume,
                             const misize_t start_positions[],
                             size_t array_length, double *slice_max,
                             double *slice_min);


/**
 * This function the minimum and maximum real value of voxels in the
 * slice containing the coordinates \a start_positions.  The \a
 * array_length may be less than or equal to the number of dimensions in
 * the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.  Coordinates must always be
 * specified in raw file order.
 * \ingroup mi2Slice
 */
int miset_slice_range(mihandle_t volume, 
                             const misize_t start_positions[],
                             size_t array_length, double slice_max,
                             double slice_min);

/**
 * This function returns the maximum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miget_volume_max(mihandle_t volume, double *slice_max);


/**
 * This function sets the maximum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miset_volume_max(mihandle_t volume, double slice_max);


/**
 * This function returns the minimum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miget_volume_min(mihandle_t volume, double *slice_min);


/**
 * This function sets the minimum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miset_volume_min(mihandle_t volume, double slice_min);


/**
 * This function retrieves the maximum and minimum real values of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miget_volume_range(mihandle_t volume, double *volume_max, 
                              double *volume_min);

/**
 * This function sets the maximum and minimum real values of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miset_volume_range(mihandle_t volume, double volume_max, 
                              double volume_min);


/** \defgroup mi2Hyper HYPERSLAB FUNCTIONS */

/** Calculates and returns the number of bytes required to store the
 * hyperslab specified by the \a n_dimensions and the
 * \a count parameters.
 * \ingroup mi2Hyper
 */
int miget_hyperslab_size(mitype_t volume_data_type, int n_dimensions, 
                                const hsize_t count[], 
                                misize_t *size_ptr);


/** Calculates and returns the number of bytes required to store the
 * hyperslab specified by the \a n_dimensions and the
 * \a count parameters, using hdf type id
 * \ingroup mi2Hyper
 */
void miget_hyperslab_size_hdf(hid_t hdf_type_id, int n_dimensions, 
                                const hsize_t count[], 
                                misize_t *size_ptr);


/** Reads the real values in the volume from the interval min through
 *  max, mapped to the maximum representable range for the requested
 *  data type. Float types is mapped to 0.0 1.0
 * \ingroup mi2Hyper
 */
int miget_hyperslab_normalized(mihandle_t volume, 
                                      mitype_t buffer_data_type,
                                      const misize_t start[], 
                                      const misize_t count[],
                                      double min, 
                                      double max, 
                                      void *buffer);

/** Writes the real values in the volume from the interval min through
 *  max, mapped to the maximum representable range for the requested
 *  data type. Float types is mapped to 0.0 1.0
 * \ingroup mi2Hyper
 */
int miset_hyperslab_normalized(mihandle_t volume, 
                                      mitype_t buffer_data_type,
                                      const misize_t start[], 
                                      const misize_t count[],
                                      double min, 
                                      double max, 
                                      void *buffer);

/** Get a hyperslab from the file, 
 * converting voxel values into real values
 * \ingroup mi2Hyper
 */
int miget_hyperslab_with_icv(mihandle_t volume, 
                                    mitype_t buffer_data_type, 
                                    const misize_t start[], 
                                    const misize_t count[], 
                                    void *buffer);

/** Write a hyperslab to the file, converting real values into voxel values
 * \ingroup mi2Hyper
 */
int miset_hyperslab_with_icv(mihandle_t volume,
                                    mitype_t buffer_data_type, 
                                    const misize_t start[],
                                    const misize_t count[],
                                    void *buffer);

/** Read a hyperslab from the file into the preallocated buffer,
 *  converting from the stored "voxel" data range to the desired
 * "real" (float or double) data range, same as miget_hyperslab_with_icv
 * \ingroup mi2Hyper
 */
int miget_real_value_hyperslab(mihandle_t volume,
                                      mitype_t buffer_data_type,
                                      const misize_t start[],
                                      const misize_t count[],
                                      void *buffer);

/** Write a hyperslab to the file from the preallocated buffer,
 *  converting from the stored "voxel" data range to the desired
 * "real" (float or double) data range, same as miset_hyperslab_with_icv
 * \ingroup mi2Hyper
 */
int miset_real_value_hyperslab(mihandle_t volume,
                                      mitype_t buffer_data_type,
                                      const misize_t start[],
                                      const misize_t count[],
                                      void *buffer);

/** Read a hyperslab from the file into the preallocated buffer,
 * with no range conversions or normalization.  Type conversions will
 * be performed if necessary.
 * \ingroup mi2Hyper
 */
int miget_voxel_value_hyperslab(mihandle_t volume,
                                       mitype_t buffer_data_type,
                                       const misize_t start[],
                                       const misize_t count[],
                                       void *buffer);

/** Write a hyperslab to the file from the preallocated buffer,
 * with no range conversions or normalization.  Type conversions will
 * be performed if necessary.
 * \ingroup mi2Hyper
 */
int miset_voxel_value_hyperslab(mihandle_t volume,
                                       mitype_t buffer_data_type,
                                       const misize_t start[],
                                       const misize_t count[],
                                       void *buffer);


/** \defgroup mi2Cvt CONVERT FUNCTIONS */

/** Convert values between real (scaled) values and voxel (unscaled)
 * values.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file, whereas the "real" value is the
 * value at the given location after scaling has been applied.
 * 
 * The \a coords parameter specifies the location at which the
 * conversion is performed.  This is needed because MINC supports
 * per-slice scaling, therefore a conversion performed at one location
 * may differ from that performed at another location.
 *
 * \param volume A volume handle
 * \param coords The position for which to perform the conversion.
 * \param ncoords The length of the \a coords array.
 * \param real_value The original real value, to be converted to voxel.
 * \param voxel_value_ptr A pointer to the converted voxel value.
 * \ingroup mi2Cvt
 */
int miconvert_real_to_voxel(mihandle_t volume,
                                   const misize_t coords[],
                                   size_t ncoords,
                                   double real_value,
                                   double *voxel_value_ptr);

/** Convert values between real (scaled) values and voxel (unscaled)
 * values.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file, whereas the "real" value is the
 * value at the given location after scaling has been applied.
 *
 * The \a coords parameter specifies the location at which the
 * conversion is performed.  This is needed because MINC supports
 * per-slice scaling, therefore a conversion performed at one location
 * may differ from that performed at another location. 
 *
 * \param volume A volume handle
 * \param coords The position for which to perform the conversion.
 * \param ncoords The length of the \a coords array.
 * \param voxel_value The original voxel value, to be converted to real.
 * \param real_value_ptr A pointer to the converted real value.
 * \ingroup mi2Cvt
 */
int miconvert_voxel_to_real(mihandle_t volume,
                                   const misize_t coords[],
                                   int ncoords,
                                   double voxel_value,
                                   double *real_value_ptr);

/** Converts an N-dimensional spatial position in voxel coordinates into a 
 * 3-dimensional spatial position in world coordinates.
 *
 * The returned world coordinate vector is in a standardized order, with
 * the X position first (at index 0), followed by the Y and Z coordinates.
 * The voxel coordinate vector is in the native order appropriate to the
 * file.
 *
 * \ingroup mi2Cvt
 */
int miconvert_voxel_to_world(mihandle_t volume,
                                    const double voxel[],
                                    double world[]);

/** Converts a 3-dimensional spatial position in world coordinates into a 
 * N-dimensional spatial position in voxel coordinates.
 *
 * The input world coordinate vector is in a standardized order, with
 * the X position first (at index 0), followed by the Y and Z coordinates.
 * The voxel coordinate vector is in the native order appropriate to the
 * file.
 *
 * \ingroup mi2Cvt
 */
int miconvert_world_to_voxel(mihandle_t volume,
                                    const double world[],
                                    double voxel[]);

/**
 * This function calculates the start values for the volume dimensions,
 * assuming that the spatial origin is relocated to the given world
 * coordinate.
 *
 * \ingroup mi2Cvt
 */
int
miconvert_world_origin_to_start( mihandle_t volume,
                                 double world[],
                                 double starts[]);

/**
 * This function calculates the start values for the volume dimensions,
 * assuming that the spatial origin is relocated to the given world
 * coordinate.
 *
 * \ingroup mi2Cvt
 */
int
miconvert_spatial_frequency_origin_to_start( mihandle_t volume,
                                             double world[],
                                             double starts[]);

/**
 * This function sets the world coordinates of the point (0,0,0) in voxel
 * coordinates.  This changes the constant offset of the two coordinate
 * systems.
 *
 * \ingroup mi2Cvt
 */
int
miset_spatial_frequency_origin(mihandle_t volume,
                               double world[]);

/** This function retrieves the real values of a position in the
 *  MINC volume.  The "real" value is the value at the given location 
 *  after scaling has been applied.
 *
 * \param volume A volume handle
 * \param coords The voxel position to retrieve
 * \param ndims The number of values in the \a coords array
 * \param value_ptr Pointer to a double variable to hold the returned value.
 *
 * \ingroup mi2Cvt
 */
int miget_real_value(mihandle_t volume,
                            const misize_t coords[],
                            int ndims,
                            double *value_ptr);

/** This function sets the  real value of a position in the MINC
 *  volume. The "real" value is the value at the given location 
 *  after scaling has been applied.
 *
 * \param volume A volume handle
 * \param coords The voxel position to retrieve
 * \param ndims The number of values in the \a coords array
 * \param value The value to save at this location.
 *
 * \ingroup mi2Cvt
 */
int miset_real_value(mihandle_t volume,
                            const misize_t coords[],
                            int ndims,
                            double value);


/** This function retrieves the voxel values of a position in the
 * MINC volume. The voxel value is the unscaled value, and corresponds
 * to the value actually stored in the file.
 *
 * \ingroup mi2Cvt
 */
int miget_voxel_value(mihandle_t volume,
                             const misize_t coords[],
                             int ndims,
                             double *voxel_ptr);


/** This function sets the voxel value of a position in the MINC
 * volume.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file.
 *
 * \ingroup mi2Cvt
 */
int miset_voxel_value(mihandle_t volume,
                             const misize_t coords[],
                             int ndims,
                             double voxel);

/** Get the absolute minimum and maximum values of a volume.
 *
 * \ingroup mi2Cvt
 */
int miget_volume_real_range(mihandle_t volume, double real_range[2]);

/**
 * This function sets the world coordinates of the point (0,0,0) in voxel
 * coordinates.  This changes the constant offset of the two coordinate
 * systems.
 *
 * \ingroup mi2Cvt
 */
int miset_world_origin(mihandle_t volume, double origin[MI2_3D]);

/* VALID functions */
/** This function gets the maximum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miget_volume_valid_max(mihandle_t volume, double *valid_max);

/** This function sets the maximum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miset_volume_valid_max(mihandle_t volume, double valid_max);

/** This function gets the minimum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miget_volume_valid_min(mihandle_t volume, double *valid_min);

/** This function sets the minimum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miset_volume_valid_min(mihandle_t volume, double valid_min);

/** This function gets the minimum and maximum valid value specific to the 
 * data type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miget_volume_valid_range(mihandle_t volume, double *valid_max, double *valid_min);

/** This function sets the minimum and maximum valid value specific to the 
 * data type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miset_volume_valid_range(mihandle_t volume, double valid_max, double valid_min);

/** \defgroup mi2Rec RECORD functions */
/** This method gets the name of the record dimension
 * TODO: set record name??
 * \ingroup mi2Rec
 */
int miget_record_name(mihandle_t volume, char **name);

/** This method gets the length (i.e., number of fields in the case of
 * uniform records and number of bytes for non_uniform ones) of the
 * record.
 * \ingroup mi2Rec
 */
int miget_record_length(mihandle_t volume, int *length);

/** This method returns the field name for the given field index.  Memory
 * for returned string is allocated on the heap and should be released using
 * mifree_name().
 * \ingroup mi2Rec
 */
int miget_record_field_name(mihandle_t volume, int index, char **name);

/** This method sets a field name for the volume record. The volume
 * must be of class "MI_CLASS_UNIFORM_RECORD".  The size of record
 * type will be increased if necessary to accomodate the new field.
 * \ingroup mi2Rec
 */
int miset_record_field_name(mihandle_t volume, int index, 
                                   const char *name);

/** \ingroup mi2Label LABEL functions */

/**
 * This function associates a label name with an integer value for the given
 * volume. Functions which read and write voxel values will read/write 
 * in integer values, and must call miget_label_name() to discover the 
 * descriptive text string which corresponds to the integer value.
 * \ingroup mi2Label
 */
int midefine_label(mihandle_t volume, int value, const char *name);

/**
 * For a labelled volume, this function retrieves the text name
 * associated with a given integer value.
 * 
 * The name pointer returned must be freed by calling mifree_name().
 * \ingroup mi2Label
*/
int miget_label_name(mihandle_t volume, int value, char **name);

/**
 * This function is the inverse of miget_label_name(). It is called to determine
 * what integer value, if any, corresponds to the given text string.
 * \ingroup mi2Label
*/
int miget_label_value(mihandle_t volume, const char *name, int *value);


/**
 * This function returns the number of defined labels, if any, or zero.
 * \ingroup mi2Label
*/
int miget_number_of_defined_labels(mihandle_t volume, int *number_of_labels);

/**
 * This function returns the label value associated with an index (0,1,...)
 * \ingroup mi2Label
*/
int miget_label_value_by_index(mihandle_t volume, int idx, int *value);

#ifdef __cplusplus
}
#endif /* __cplusplus defined */


#endif /*MINC2_API_H*/
// kate: indent-mode cstyle; indent-width 2; replace-tabs on; 
