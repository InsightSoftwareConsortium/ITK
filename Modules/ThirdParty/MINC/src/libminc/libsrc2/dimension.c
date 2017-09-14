/** \file dimension.c
 * \brief MINC 2.0 "dimension" Functions
 * \author Leila Baghdadi
 *
 * Functions to create, destroy, and manipulate MINC dimension objects.
 * All functions return MI_NOERROR upon success and MI_ERROR on failure.
 ************************************************************************/
#define _GNU_SOURCE 1
#include <stdlib.h>

/*#include <hdf5.h>*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "minc2.h"
#include "minc2_private.h"

#include <math.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif /*HAVE_STRING_H*/

/**
  Figure out whether a dimension is associated with a volume.
  \param dimension The dimension handle.
  \param volume    A pointer to the volume handle.
  *
  * This method returns the volume handle associated with a given dimension
  * or an error if the specified handle is not associated with the volume.
  * \ingroup mi2Dim
 */
int miget_volume_from_dimension ( midimhandle_t dimension, mihandle_t *volume )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  if ( dimension->volume_handle != NULL ) {
    *volume = dimension->volume_handle;
  } else {
    return ( MI_ERROR );
  }

  return ( MI_NOERROR );
}

/**
  Create a copy of a given dimension.
  \param dim_ptr The dimension handle of the dimension to copy.
  \param new_dim_ptr A pointer to the dimension handle of the copied dimension.
  *
  * This method creates a copy of the specified dimension and returns the handle
  * to the copied dimension or error on failure.
  * \ingroup mi2Dim
 */
int micopy_dimension ( midimhandle_t dim_ptr, midimhandle_t *new_dim_ptr )
{
  misize_t i;
  midimhandle_t handle;

  if ( dim_ptr == NULL ) {
    return ( MI_ERROR );
  }

  /* Allocate storage for the structure
   */
  handle = ( midimhandle_t ) malloc ( sizeof ( struct midimension ) );

  if ( handle == NULL ) { 
    return ( MI_ERROR );
  }

  handle->attr = dim_ptr->attr;
  handle->dim_class = dim_ptr->dim_class;
  /* Copy direction cosines */
  handle->direction_cosines[MI2_X] = dim_ptr->direction_cosines[0];
  handle->direction_cosines[MI2_Y] = dim_ptr->direction_cosines[1];
  handle->direction_cosines[MI2_Z] = dim_ptr->direction_cosines[2];

  switch ( dim_ptr->flipping_order ) {
  case MI_FILE_ORDER:
    handle->flipping_order = MI_FILE_ORDER;
    break;
  case MI_COUNTER_FILE_ORDER:
    handle->flipping_order = MI_COUNTER_FILE_ORDER;
    break;
  default:
      free(handle);
      return ( MI_ERROR );
  }

  handle->name = strdup ( dim_ptr->name );
  handle->length = dim_ptr->length;

  if ( dim_ptr->offsets != NULL ) {
    handle->offsets = ( double * ) malloc ( dim_ptr->length * sizeof ( double ) );

    if ( handle->offsets == NULL ) {
      free(handle);
      return ( MI_ERROR );
    }

    for ( i = 0; i < dim_ptr->length; i++ ) {
      handle->offsets[i] = dim_ptr->offsets[i];
    }
  } else {
    handle->offsets = NULL;
  }

  // check to make sure start and step are defined!
  if ( dim_ptr->step != 0 ) {
    handle->start = dim_ptr->start;
    handle->step = dim_ptr->step;
  } else {
    handle->step = 0.0;
  }

  //check to make sure string is not empty or null
  if ( dim_ptr->units == NULL  || *dim_ptr->units == '\0' ) {
    if (dim_ptr->dim_class == MI_DIMCLASS_TIME)
      handle->units = strdup("s");
    else
      handle->units = strdup ( "mm" );
  } else {
    handle->units = strdup ( dim_ptr->units );
  }

  handle->align = dim_ptr->align;
  handle->width = dim_ptr->width;

  if ( dim_ptr->widths != NULL ) {
    handle->widths = ( double * ) malloc ( dim_ptr->length * sizeof ( double ) );

    if ( handle->widths == NULL ) {
      return ( MI_ERROR );
    }

    for ( i = 0; i < dim_ptr->length; i++ ) {
      handle->widths[i] = dim_ptr->widths[i];
    }
  } else {
    handle->widths = NULL;
  }

  if ( dim_ptr->comments == NULL ) {
    handle->comments = NULL;
  } else {
    handle->comments = strdup ( dim_ptr->comments );
  }

  handle->volume_handle = dim_ptr->volume_handle;

  *new_dim_ptr = handle;

  return ( MI_NOERROR );
}

/**
  Define a new dimension in a MINC volume.
  \param name A pointer to the string specifying the dimension name.
  \param class The class of the dimension.
  \param attr  The attribute of the dimension.
  \param length The size of the dimension.
  \param new_dim_ptr A pointer to the dimension handle.
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
                     misize_t length, midimhandle_t *new_dim_ptr)
{

  midimhandle_t handle;
  misize_t i;
  /* Allocate space for the new dimension
   */
  handle = ( midimhandle_t ) malloc ( sizeof ( struct midimension ) );

  if ( handle == NULL ) {
    return ( MI_ERROR );
  }


  /* Duplicate the dimension name
   */
  handle->name = strdup ( name );
  /* Set the dimension comment to NULL unless otherwise
   */
  handle->comments = NULL;

  switch ( dimclass ) {
  case MI_DIMCLASS_SPATIAL:
    handle->dim_class  = MI_DIMCLASS_SPATIAL;

    if ( strcmp ( name, MIxspace ) == 0 ) {
      handle->direction_cosines[MI2_X] = 1.0;
      handle->direction_cosines[MI2_Y] = 0.0;
      handle->direction_cosines[MI2_Z] = 0.0;
      handle->comments = strdup ( "X increases from patient left to right" );
    } else
      if ( strcmp ( name, MIyspace ) == 0 ) {
        handle->direction_cosines[MI2_X] = 0.0;
        handle->direction_cosines[MI2_Y] = 1.0;
        handle->direction_cosines[MI2_Z] = 0.0;
        handle->comments = strdup ( "Y increases from patient posterior to anterior" );
      } else
        if ( strcmp ( name, MIzspace ) == 0 ) {
          handle->direction_cosines[MI2_X] = 0.0;
          handle->direction_cosines[MI2_Y] = 0.0;
          handle->direction_cosines[MI2_Z] = 1.0;
          handle->comments = strdup ( "Z increases from patient inferior to superior" );
        } else {
          handle->direction_cosines[MI2_X] = 1.0;
          handle->direction_cosines[MI2_Y] = 0.0;
          handle->direction_cosines[MI2_Z] = 0.0;
          handle->comments = NULL;
        }

    break;
  case MI_DIMCLASS_TIME:
    handle->dim_class  = MI_DIMCLASS_TIME;
    break;
  case MI_DIMCLASS_SFREQUENCY:
    handle->dim_class  = MI_DIMCLASS_SFREQUENCY;

    if ( strcmp ( name, "xfrequency" ) == 0 ) {
      handle->direction_cosines[MI2_X] = 1.0;
      handle->direction_cosines[MI2_Y] = 0.0;
      handle->direction_cosines[MI2_Z] = 0.0;
    } else
      if ( strcmp ( name, "yfrequency" ) == 0 ) {
        handle->direction_cosines[MI2_X] = 0.0;
        handle->direction_cosines[MI2_Y] = 1.0;
        handle->direction_cosines[MI2_Z] = 0.0;
      } else
        if ( strcmp ( name, "zfrequency" ) == 0 ) {
          handle->direction_cosines[MI2_X] = 0.0;
          handle->direction_cosines[MI2_Y] = 0.0;
          handle->direction_cosines[MI2_Z] = 1.0;
        } else {
          handle->direction_cosines[MI2_X] = 1.0;
          handle->direction_cosines[MI2_Y] = 0.0;
          handle->direction_cosines[MI2_Z] = 0.0;
        }

    break;
  case MI_DIMCLASS_TFREQUENCY:
    handle->dim_class  = MI_DIMCLASS_TFREQUENCY;
    break;
  case MI_DIMCLASS_USER:
    handle->dim_class  = MI_DIMCLASS_USER;
    break;
  case MI_DIMCLASS_RECORD:
    handle->dim_class  = MI_DIMCLASS_RECORD;
    break;
  case MI_DIMCLASS_ANY:
  default:
    free(handle);
    return MI_ERROR;
  }

  handle->offsets = NULL;
  handle->attr = attr;

  if ( attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED ) {
    handle->widths = ( double * ) malloc ( length * sizeof ( double ) );

    for ( i = 0; i < length; i++ ) {

      handle->widths[i] = 1.0;
    }
  } else {
    handle->widths = NULL;
  }

  // do not set start and step if vector_dimension present
  if ( strcmp ( name, MIvector_dimension ) ) {
    handle->start = 0.0;
    handle->step = 1.0;
  } else {
    handle->step = 0.0;
  }

  handle->width = 1.0;

  handle->flipping_order = MI_FILE_ORDER;

  if ( dimclass != MI_DIMCLASS_SPATIAL && dimclass != MI_DIMCLASS_SFREQUENCY ) {
    handle->direction_cosines[MI2_X] = 1.0;
    handle->direction_cosines[MI2_Y] = 0.0;
    handle->direction_cosines[MI2_Z] = 0.0;
  }

  handle->length = length;

  if (dimclass == MI_DIMCLASS_TIME) {
    handle->align = MI_DIMALIGN_START;
    handle->units = strdup( "s" );
  }
  else {
    handle->align = MI_DIMALIGN_CENTRE;
    handle->units = strdup ( "mm" );
  }
  /* volume_handle is the only NULL value once the dimension is created.
   */
  handle->volume_handle = NULL;

  *new_dim_ptr = handle;

  return ( MI_NOERROR );

}

/**
  Delete the dimension definition.
  \param dim_prt The dimension handle.
  *
  * Note: The original document stated that a dimension has to be
  * associated with a given volume before it can be deleted. This
  * feature was erased from the document and not considered here.
  * \ingroup mi2Dim
  */
int mifree_dimension_handle ( midimhandle_t dim_ptr )
{

  if ( dim_ptr == NULL ) {
    return ( MI_ERROR );
  }

  if ( dim_ptr->name != NULL ) {
    free ( dim_ptr->name );
  }

  if ( dim_ptr->offsets != NULL ) {
    free ( dim_ptr->offsets );
  }

  if ( dim_ptr->units != NULL ) {
    free ( dim_ptr->units );
  }

  if ( dim_ptr->widths != NULL ) {
    free ( dim_ptr->widths );
  }
  
  if( dim_ptr->comments != NULL ) {
    free( dim_ptr->comments );
  }

  free ( dim_ptr );

  return ( MI_NOERROR );
}

/** Retrieve the list of dimensions defined in a MINC volume,
 * with the same class \a class and attribute \a attr.
 * \param volume The volume handle.
 * \param class  The class of the dimensions.
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
int miget_volume_dimensions ( mihandle_t volume, midimclass_t class, midimattr_t attr,
                          miorder_t order, int array_length,
                          midimhandle_t dimensions[] )
{

  hsize_t number_of_dims, i = 0, max_dims;
  int num_ret_dims = 0;

  if ( volume == NULL ) {
    return ( MI_ERROR );
  }

  /* make sure the user has set the apparernt order before
   *  calling this function with MI_DIMORDER_APPARENT
   */
  if ( order == MI_DIMORDER_APPARENT && volume->dim_indices == NULL ) {
    return ( MI_ERROR );
  }

  number_of_dims = volume->number_of_dims;

  if ( (hsize_t)( array_length ) > number_of_dims ) {
    max_dims = number_of_dims;
  } else {
    max_dims = array_length;
  }

  /* Go through each dimension separately and figure out
     which one has a matching class and attribute.
     if the user has set the apparent order use the dim_indices
     to search the dimensions otherwise use dim_handles
   */

  for ( i = 0; i < max_dims; i++ ) {
    midimhandle_t hdim;

    if ( order == MI_DIMORDER_APPARENT ) {
      hdim = volume->dim_handles[volume->dim_indices[i]];
    } else {
      hdim = volume->dim_handles[i];
    }

    if ( class == MI_DIMCLASS_ANY || class == hdim->dim_class ) {
      if ( attr ==  MI_DIMATTR_ALL || hdim->attr == attr ) {
        dimensions[num_ret_dims++] = hdim;

      }
    }
  }

  return ( num_ret_dims );
}

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
int miset_apparent_dimension_order ( mihandle_t volume, int array_length,
                                 midimhandle_t dimensions[] )
{
  int diff;
  int i = 0, j = 0, k = 0;

  if ( volume == NULL || array_length <= 0 ) {
    return ( MI_ERROR );
  }

  /* If array_length was more than the number of dimensions
    the rest of the given dimensions will be ignored.
  */
  diff = volume->number_of_dims - array_length;

  if ( diff < 0 ) {
    diff = 0;
  }

  /* Allocated space for dimensions indices, if not already done.
   */
  if ( volume->dim_indices == NULL ) {
    volume->dim_indices = ( int * ) malloc ( volume->number_of_dims * sizeof ( int ) );
    memset ( volume->dim_indices, -1, sizeof ( volume->number_of_dims ) );
  }

  if ( diff > 0 ) {

    while ( i < volume->number_of_dims && k < diff ) {
      for ( j = 0; j < array_length; j++ ) {
        if ( volume->dim_handles[i] == dimensions[j] ) {
          break;
        }
      }

      if ( j == array_length ) {
        volume->dim_indices[k] = i;
        k++;
      }

      i++;
    }
  }

  for ( i = 0; i < volume->number_of_dims; i++ ) {
    for ( j = 0; j < array_length; j++ ) {
      if ( volume->dim_handles[i] == dimensions[j] ) {
        volume->dim_indices[j + diff] = i;
        break;
      }

    }
  }

  /*
  printf(" apparent dimension order \n");
  for(i=0; i < volume->number_of_dims; i++) {
    printf(" %d ", volume->dim_indices[i]);
  }
  printf("\n");
  */
  return ( MI_NOERROR );
}

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
int miset_apparent_dimension_order_by_name ( mihandle_t volume, int array_length,
    char **names )
{
  int diff;
  int i = 0, j = 0, k = 0;

  if ( volume == NULL ) {
    return ( MI_ERROR );
  }

  if ( names == NULL || array_length <= 0 ) {
    /* Reset the dimension ordering */
    if ( volume->dim_indices != NULL ) {
      free ( volume->dim_indices );
      volume->dim_indices = NULL;
    }

    return ( MI_NOERROR );
  }

  /* Note that all dimension names must be different or an error occurs.
   */
  for ( i = 0; i < array_length; i++ ) {
    for ( j = i + 1; j < array_length; j++ ) {
      if ( strcmp ( names[i], names[j] ) == 0 ) {
        return ( MI_ERROR );
      }
    }
  }

  /* If array_length was more than the number of dimensions
     the rest of the given dimensions will be ignored.
   */
  diff = volume->number_of_dims - array_length;

  if ( diff < 0 ) {
    diff = 0;
  }

  /* Allocated space for dimensions indices, if not already done.
   */
  if ( volume->dim_indices == NULL ) {
    volume->dim_indices = ( int * ) malloc ( volume->number_of_dims * sizeof ( int ) );
    memset ( volume->dim_indices, -1, sizeof ( volume->number_of_dims ) );
  }

  i = 0;
  j = 0;
  k = 0;

  if ( diff > 0 ) {

    while ( i < volume->number_of_dims && k < diff ) {
      for ( j = 0; j < array_length; j++ ) {
        if ( strcmp ( volume->dim_handles[i]->name, names[j] ) ) {
          break;
        }
      }

      if ( j == 3 ) {
        volume->dim_indices[k] = i;
        k++;
      }

      i++;
    }
  }

  for ( i = 0; i < volume->number_of_dims; i++ ) {
    for ( j = 0; j < array_length; j++ ) {
      if ( !strcmp ( volume->dim_handles[i]->name, names[j] ) ) {
        volume->dim_indices[j + diff] = i;
        break;
      }
    }

  }

  return ( MI_NOERROR );
}

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
int miset_apparent_record_dimension_flag ( mihandle_t volume, int record_flag )
{
  midimhandle_t handle;

  if ( volume == NULL ) {
    return ( MI_ERROR );
  }

  /* Allocate space for the dimension
   */
  handle = ( midimhandle_t ) malloc ( sizeof ( struct midimension ) );

  if ( handle == NULL ) {
    return ( MI_ERROR );
  }

  handle->dim_class = MI_DIMCLASS_RECORD;
  handle->volume_handle = volume;

  volume->dim_handles[volume->number_of_dims] = handle;
  /* Add one to the number of dimensions so the volume
     will appear to have (n+1) dimensions
   */
  volume->number_of_dims++;

  return ( MI_NOERROR );
}

/**
  * Get the apparent order of voxels (i.e., the order that voxel indices increase/decrease)
  * \param dimension The dimension handle
  * \param flipping_order The order of voxels.
  * \param sign The sign of the step value.
  *
  * This method gets the apparent order of voxels for the specified dimension
  * and the sign of the step values.
  * \ingroup mi2Dim
  */
int miget_dimension_apparent_voxel_order ( midimhandle_t dimension,
                                       miflipping_t *flipping_order,
                                       miflipping_t *sign )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  switch ( dimension->flipping_order ) {
  case MI_FILE_ORDER:
    *flipping_order = MI_FILE_ORDER;

    if ( dimension->step > 0 ) {
      *sign = MI_POSITIVE;
    } else {
      *sign = MI_NEGATIVE;
    }

    break;
  case MI_COUNTER_FILE_ORDER:
    *flipping_order = MI_COUNTER_FILE_ORDER;

    if ( dimension->step > 0 ) {
      *sign = MI_NEGATIVE;
    } else {
      *sign = MI_POSITIVE;
    }

    break;
  case MI_POSITIVE:
    *sign = MI_POSITIVE;

    if ( dimension->step > 0 ) {
      *flipping_order = MI_FILE_ORDER;
    } else {
      *flipping_order = MI_COUNTER_FILE_ORDER;
    }

    break;
  case MI_NEGATIVE:
    *sign = MI_NEGATIVE;

    if ( dimension->step > 0 ) {
      *flipping_order = MI_COUNTER_FILE_ORDER;
    } else {
      *flipping_order = MI_FILE_ORDER;
    }

    break;
  default:
    return ( MI_ERROR );
  }

  return ( MI_NOERROR );
}

/**
 * Set the apparent order of voxels.
 * \param dimension The dimension handle.
 * \param flipping_order The order of voxels.
 *
 * This method sets the apparent order of voxels for the specified dimension.
 * For the detailed description of voxel order refer to the MINC 2.0 API definition.
 * \ingroup mi2Dim
 */
int miset_dimension_apparent_voxel_order ( midimhandle_t dimension,
                                       miflipping_t flipping_order )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  switch ( flipping_order ) {
  case MI_FILE_ORDER:
    dimension->flipping_order  = MI_FILE_ORDER;
    break;
  case MI_COUNTER_FILE_ORDER:
    dimension->flipping_order  = MI_COUNTER_FILE_ORDER;
    break;
  case MI_POSITIVE:
    dimension->flipping_order  = MI_POSITIVE;
    break;
  case MI_NEGATIVE:
    dimension->flipping_order  = MI_NEGATIVE;
    break;
  default:
    return ( MI_ERROR );
  }

  return ( MI_NOERROR );
}

/**
 * Get the class of a MINC dimension.
 * \param dimension The dimension handle.
 * \param class A pointer to the dimension class.
 *
 * The "class" of a MINC dimension defines the general type of a dimension,
 * whether it is a spatial dimension, a time dimension, or a frequency dimension
 * as transformed from either space or time.  User-defined dimension are also
 * permitted, with no default handling assumed. Finally, a record can be specified
 * as a dimension.
 * \ingroup mi2Dim
 */
int miget_dimension_class ( midimhandle_t dimension, midimclass_t *class )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  switch ( dimension->dim_class ) {
  case MI_DIMCLASS_ANY:
    *class = MI_DIMCLASS_ANY;
    break;
  case MI_DIMCLASS_SPATIAL:
    *class = MI_DIMCLASS_SPATIAL;
    break;
  case MI_DIMCLASS_TIME:
    *class = MI_DIMCLASS_TIME;
    break;
  case MI_DIMCLASS_SFREQUENCY:
    *class = MI_DIMCLASS_SFREQUENCY;
    break;
  case MI_DIMCLASS_TFREQUENCY:
    *class = MI_DIMCLASS_TFREQUENCY;
    break;
  case MI_DIMCLASS_USER:
    *class = MI_DIMCLASS_USER;
    break;
  case MI_DIMCLASS_RECORD:
    *class = MI_DIMCLASS_RECORD;
    break;
  default:
    return ( MI_ERROR );
  }

  return ( MI_NOERROR );
}

/**
  * Set the class of a MINC dimension.
  * \param dimension The dimension handle.
  * \param class The dimension class.
  *
  * Refer to miget_dimension_class().
  * \ingroup mi2Dim
  */
int miset_dimension_class ( midimhandle_t dimension, midimclass_t class )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  switch ( class ) {
  case MI_DIMCLASS_ANY:
    dimension->dim_class = MI_DIMCLASS_ANY;
    break;
  case MI_DIMCLASS_SPATIAL:
    dimension->dim_class = MI_DIMCLASS_SPATIAL;
    break;
  case MI_DIMCLASS_TIME:
    dimension->dim_class = MI_DIMCLASS_TIME;
    break;
  case MI_DIMCLASS_SFREQUENCY:
    dimension->dim_class = MI_DIMCLASS_SFREQUENCY;
    break;
  case MI_DIMCLASS_TFREQUENCY:
    dimension->dim_class = MI_DIMCLASS_TFREQUENCY;
    break;
  case MI_DIMCLASS_USER:
    dimension->dim_class = MI_DIMCLASS_USER;
    break;
  case MI_DIMCLASS_RECORD:
    dimension->dim_class = MI_DIMCLASS_RECORD;
    break;
  default:
    return ( MI_ERROR );
  }

  return ( MI_NOERROR );
}

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
int miget_dimension_cosines ( midimhandle_t dimension, double direction_cosines[3] )
{
  if ( dimension == NULL || ( dimension->dim_class != MI_DIMCLASS_SPATIAL &&
                              dimension->dim_class != MI_DIMCLASS_SFREQUENCY ) ) {
    return ( MI_ERROR );
  }

  direction_cosines[0] = dimension->direction_cosines[0];
  direction_cosines[1] = dimension->direction_cosines[1];
  direction_cosines[2] = dimension->direction_cosines[2];

  return ( MI_NOERROR );
}

/**
  * Set the direction cosine vector for a given SPATIAL dimension.
  * \param dimension The dimension handle.
  * \param direction_cosines An array of direction_cosines(i.e., vector determining the direction cosine).
  *
  * Refer to miget_dimension_cosines().
  * \ingroup mi2Dim
  */
int miset_dimension_cosines ( midimhandle_t dimension,
                          const double direction_cosines[3] )
{

  if ( dimension == NULL || ( dimension->dim_class != MI_DIMCLASS_SPATIAL &&
                              dimension->dim_class != MI_DIMCLASS_SFREQUENCY ) ) {
    return ( MI_ERROR );
  }

  dimension->direction_cosines[0] = direction_cosines[0];
  dimension->direction_cosines[1] = direction_cosines[1];
  dimension->direction_cosines[2] = direction_cosines[2];

  return ( MI_NOERROR );
}

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
int miget_dimension_description ( midimhandle_t dimension, char **comments_ptr )
{

  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  *comments_ptr = strdup ( dimension->comments );

  return ( MI_NOERROR );
}

/**
  * Set the comments attribute for a given dimension.
  * \param dimension The dimension handle.
  * \param comments_ptr A pointer for the comments.
  *
  * Refer to miget_dimension_description().
  * \ingroup mi2Dim
  */
int miset_dimension_description ( midimhandle_t dimension, const char *comments )
{

  if ( dimension == NULL || comments == NULL ) {
    return ( MI_ERROR );
  }

  if ( ( strlen ( comments ) + 1 ) <= MI2_CHAR_LENGTH ) {
    dimension->comments = strdup ( comments );
  } else {
    return ( MI_ERROR );
  }

  return ( MI_NOERROR );
}


/**
  * Get the identifier (name) of a MINC dimension.
  * \param dimension The dimension handle.
  * \param name_ptr A string pointer for returning the dimension name, should be released with free
  * 
  * Retrieves the name of the given dimension.
  * \ingroup mi2Dim
  */
int miget_dimension_name ( midimhandle_t dimension, char **name_ptr )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  *name_ptr = strdup ( dimension->name );

  return ( MI_NOERROR );
}

/**
 * Set the identifier (name) of a given MINC dimension.
 * \param dimension The dimension handle.
 * \param name_ptr A pointer for the dimension name.
 *
 * Rename the given dimension.
 * \ingroup mi2Dim
 */
int miset_dimension_name ( midimhandle_t dimension, const char *name )
{
  if ( dimension == NULL || name == NULL ) {
    return ( MI_ERROR );
  }

  if ( ( strlen ( name ) + 1 ) > MI2_CHAR_LENGTH ) {
    return ( MI_ERROR );
  }

  /* Free the existing dimension name.
   */
  if ( dimension->name != NULL ) {
    free ( dimension->name );
  }

  dimension->name = strdup ( name );

  return ( MI_NOERROR );
}

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
int miget_dimension_offsets( midimhandle_t dimension, misize_t array_length,
                          misize_t start_position, double offsets[] )
{
  misize_t end_position;
  misize_t i, j;

  if ( dimension == NULL || start_position > dimension->length ) {
    return ( MI_ERROR );
  }

  if ( ( start_position + array_length ) > dimension->length ) {
    end_position = dimension->length;
  } else {
    end_position = start_position + array_length;
  }

  if ( dimension->offsets == NULL ) {
    for ( i = start_position, j = 0; i < end_position ; i++, j++ ) {
      /* For regularly sampled dimensions, the step value
       * is added to each value to get the next value.
       */
      offsets[j] = dimension->start + ( i * dimension->step );
    }
  } else {
    for ( i = start_position, j = 0; i < end_position ; i++, j++ ) {
      offsets[j] = dimension->offsets[i];
    }
  }

  return ( MI_NOERROR );
}

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
int miset_dimension_offsets ( midimhandle_t dimension,
                          misize_t array_length,
                          misize_t start_position,
                          const double offsets[] )
{
  misize_t end_position;
  misize_t i, j;

  /* Check to see whether the dimension is regularly sampled.
   */
  if ( dimension == NULL ||
       ( dimension->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED ) == 0 ||
       start_position > dimension->length ) {
    return ( MI_ERROR );
  }

  if ( ( start_position + array_length ) > dimension->length ) {
    end_position = dimension->length;
  } else {
    end_position = start_position + array_length;
  }

  /* Allocate space for the offsets if not already done.
   */
  if ( dimension->offsets == NULL ) {
    dimension->offsets =
      ( double * ) malloc ( dimension->length * sizeof ( double ) );
  }

  for ( i = start_position, j = 0; i < end_position; i++, j++ ) {
    dimension->offsets[i] = offsets[j] ;
  }

  return ( MI_NOERROR );
}

/**
  * Get the sampling flag for a MINC dimension.
  * \param dimension The dimension handle.
  * \param sampling_flag The flag to determine regular/irregular sampling dimensions.
  *
  * This flag is false (zero) if the dimension is sampled at regular
  * intervals, and true if the dimension is sampled irregularly.
  * If a dimension has regular sampling, the miget_dimension_separation()
  * may be used to retrieve the sampling interval, and the
  * miget_dimension_start() may be used to retrieve the origin
  * value along the axis.
  *
  * If a dimension has irregular sampling, the miget_dimension_offsets()
  * may be used to retrieve the positions of each sample along that axis.
  * \ingroup mi2Dim
  */
int miget_dimension_sampling_flag ( midimhandle_t dimension, miboolean_t *sampling_flag )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  *sampling_flag = ( dimension->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED ) != 0;

  return ( MI_NOERROR );
}

/**
  * Set the sampling flag for a MINC dimension.
  * \param dimension The dimension handle.
  * \param sampling_flag The flag to determine regular/irregular sampling dimensions.
  *
  * Refer to miget_dimension_sampling_flag().
  * \ingroup mi2Dim
  */
int miset_dimension_sampling_flag ( midimhandle_t dimension, miboolean_t sampling_flag )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  if ( sampling_flag ) {
    dimension->attr |= MI_DIMATTR_NOT_REGULARLY_SAMPLED;
    dimension->attr &= ~MI_DIMATTR_REGULARLY_SAMPLED;
  } else {
    dimension->attr &= ~MI_DIMATTR_NOT_REGULARLY_SAMPLED;
    dimension->attr |= MI_DIMATTR_REGULARLY_SAMPLED;
  }

  return ( MI_NOERROR );
}

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
int miget_dimension_separation ( midimhandle_t dimension,
                             mivoxel_order_t voxel_order,
                             double *separation_ptr )
{
  if ( dimension == NULL || dimension->step == 0 ) {
    return ( MI_ERROR );
  }

  if ( voxel_order == MI_ORDER_FILE ) {
    *separation_ptr = dimension->step;
  } else {
    if ( dimension->flipping_order == MI_COUNTER_FILE_ORDER ) {
      *separation_ptr = -dimension->step;
    } else
      if ( dimension->flipping_order == MI_POSITIVE ) {
        if ( dimension->step > 0 ) {
          *separation_ptr = dimension->step;
        } else {
          *separation_ptr = -dimension->step;
        }
      } else
        if ( dimension->flipping_order == MI_NEGATIVE ) {
          if ( dimension->step < 0 ) {
            *separation_ptr = dimension->step;
          } else {
            *separation_ptr = -dimension->step;
          }
        } else {
          *separation_ptr = dimension->step;
        }
  }

  return ( MI_NOERROR );
}

/**
  * Set the sampling interval (step) for a single dimension.
  * \param dimension The dimension handle.
  * \param separation The dimension sampling interval (step).
  *
  * Refer to miget_dimension_separation().
  * \ingroup mi2Dim
  */
int miset_dimension_separation ( midimhandle_t dimension, double separation )
{
  /* file-order of voxels is assumed.
   */
  if ( dimension == NULL || dimension->step == 0 ) {
    return ( MI_ERROR );
  }

  dimension->step = separation;
  /* If not explicitly set, the width will be assumed to be equal to the
     dimension's step size.
  */
  //dimension->width = separation;

  return ( MI_NOERROR );
}

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
int miget_dimension_separations ( const midimhandle_t dimensions[],
                              mivoxel_order_t voxel_order,
                              misize_t array_length,
                              double separations[] )
{
  misize_t i;

  for ( i = 0; i < array_length; i++ ) {
    miget_dimension_separation ( dimensions[i], voxel_order,
                                 &separations[i] );
  }

  return ( MI_NOERROR );
}

/**
  * Set the sampling interval (STEP) for a list of dimensions.
  * \param dimensions An array of dimension handles.
  * \param array_length The number of dimensions in the dimesions array.
  * \param separations An array of dimensions sampling intervals (step) values.
  *
  * Refer to miget_dimension_separations().
  * \ingroup mi2Dim
  */
int miset_dimension_separations ( const midimhandle_t dimensions[],
                              misize_t array_length,
                              const double separations[] )
{
  misize_t i;

  for ( i = 0; i < array_length; i++ ) {
    miset_dimension_separation ( dimensions[i], separations[i] );
  }

  return ( MI_NOERROR );
}

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
int miget_dimension_size ( midimhandle_t dimension, misize_t *size_ptr )
{
  if ( dimension == NULL ) {
    return ( MI_ERROR );
  }

  *size_ptr = dimension->length;
  return ( MI_NOERROR );
}

/**
  * Set the length of a MINC dimension if not associated with a volume.
  * \param dimension The dimension handle.
  * \param size  The size of the dimension.
  *
  * Refer to miget_dimension_size().
  * \ingroup mi2Dim
  */
int miset_dimension_size ( midimhandle_t dimension, misize_t size )
{
  /* Check whether the dimension is associated with a volume.
   */
  if ( dimension == NULL || dimension->volume_handle != NULL ) {
    return ( MI_ERROR );
  }

  dimension->length = size;
  return ( MI_NOERROR );
}

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
int miget_dimension_sizes ( const midimhandle_t dimensions[], misize_t array_length,
                        misize_t sizes[] )
{
  misize_t i;

  for ( i = 0; i < array_length; i++ ) {
    miget_dimension_size ( dimensions[i], &sizes[i] );
  }

  return ( MI_NOERROR );
}

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
int miget_dimension_start ( midimhandle_t dimension, mivoxel_order_t voxel_order,
                        double *start_ptr )
{
  /* If voxel_order is set to apparent file order (i.e., 1)
     start = start + step * (n-1)
   */
  if ( dimension == NULL || dimension->step == 0 ) {
    return ( MI_ERROR );
  }

  if ( voxel_order == MI_ORDER_FILE ) {
    *start_ptr = dimension->start;
  } else { // L.B March 16/2011, Properly reflect voxel ordering
    if ( dimension->flipping_order == MI_COUNTER_FILE_ORDER ) {
      *start_ptr = dimension->start + ( dimension->step * ( dimension->length - 1 ) );
    } else
      if ( dimension->flipping_order == MI_POSITIVE ) {
        if ( dimension->step > 0 ) {
          *start_ptr = dimension->start;
        } else {
          *start_ptr = dimension->start + ( dimension->step * ( dimension->length - 1 ) );
        }
      } else
        if ( dimension->flipping_order == MI_NEGATIVE ) {
          if ( dimension->step < 0 ) {
            *start_ptr = dimension->start;
          } else {
            *start_ptr = dimension->start + ( dimension->step * ( dimension->length - 1 ) );
          }
        }
  }

  return ( MI_NOERROR );
}

/** 
  * Set the start of a MINC dimension.
  * \param dimension The dimension handle.
  * \param start The start of the dimension.
  *
  * Refer to miget_dimension_start().
  * \ingroup mi2Dim
  */
int miset_dimension_start ( midimhandle_t dimension, double start )
{
  if ( dimension == NULL || dimension->step == 0 ) {
    return ( MI_ERROR );
  }

  dimension->start = start;
  return ( MI_NOERROR );
}

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
int miget_dimension_starts ( const midimhandle_t dimensions[],
                         mivoxel_order_t voxel_order,
                         misize_t array_length, double starts[] )
{
  misize_t i;

  for ( i = 0; i < array_length; i++ ) {
    miget_dimension_start ( dimensions[i], voxel_order, &starts[i] );
  }

  return ( MI_NOERROR );
}

/**
  * Set the start values for MINC dimensions in dimensions array.
  * \param dimensions The array of dimension handles.
  * \param array_length The number of dimensions in the dimensions array.
  * \param starts The array of dimension starts.
  *
  * Refer to miget_dimension_starts().
  * \ingroup mi2Dim
  */
int miset_dimension_starts ( const midimhandle_t dimensions[],
                         misize_t array_length,
                         const double starts[] )
{
  misize_t i;

  for ( i = 0; i < array_length; i++ ) {
    miset_dimension_start ( dimensions[i], starts[i] );
  }

  return ( MI_NOERROR );
}

/**
  * Get the unit string for a MINC dimension.
  * \param dimension The dimension handle.
  * \param units_ptr A string pointer to the dimension units.
  *
  * Retrieves the units of the given dimension,
  * The caller must free the string returned by this function.
  * \ingroup mi2Dim
  */
int miget_dimension_units ( midimhandle_t dimension, char **units_ptr )
{
  if ( dimension == NULL || units_ptr == NULL ) {
    return ( MI_ERROR );
  }

  *units_ptr = strdup ( dimension->units );

  return ( MI_NOERROR );
}

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
int miset_dimension_units ( midimhandle_t dimension, const char *units )
{
  if ( dimension == NULL || units == NULL ) {
    return ( MI_ERROR );
  }

  if ( strlen ( units ) + 1 > MI2_CHAR_LENGTH ) {
    return ( MI_ERROR );
  }

  dimension->units = strdup ( units );
  return ( MI_NOERROR );
}

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
int miget_dimension_width ( midimhandle_t dimension, double *width_ptr )
{

  if ( dimension == NULL ||
       ( dimension->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED ) != 0 ) {
    return ( MI_ERROR );
  }

  *width_ptr = dimension->width;
  return ( MI_NOERROR );
}

/**
  * Set the A single full-width half-maximum value for a
  * regularly sampled dimension.
  * \param dimension The dimension handle.
  * \param width The FWHM value.
  *
  * Refer to miget_dimension_width().
  * \ingroup mi2Dim
  */
int miset_dimension_width ( midimhandle_t dimension, double width )
{
  if ( dimension == NULL ||
       ( dimension->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED ) != 0 ) {
    return ( MI_ERROR );
  }

  /* Check to make sure width value is positive.
   */
  if ( width < 0 ) {
    dimension->width = -width;
  } else {
    dimension->width = width;
  }

  return ( MI_NOERROR );
}

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
int miget_dimension_widths ( midimhandle_t dimension,
                         mivoxel_order_t voxel_order,
                         misize_t array_length,
                         misize_t start_position,
                         double widths[] )
{
  misize_t end_position;
  misize_t i, j = 0;

  if ( dimension == NULL || start_position > dimension->length ) {
    return ( MI_ERROR );
  }

  if ( ( start_position + array_length ) > dimension->length ) {
    end_position = dimension->length;
  } else {
    end_position = start_position + array_length;
  }

  /* Check to see whether the dimension is regularly sampled.
   */
  if ( dimension->widths == NULL ) {
    if (dimension->width != 0) {
      for ( i = start_position; i < end_position; i++ ) {
        widths[j] = dimension->width;
        j++;
      }
    }
    else {
      for ( i = start_position; i < end_position; i++ ) {
        widths[j] = fabs(dimension->step);
        j++;
      }
    }
  } else {
    /* If the apparent order is requested, the widths are returned
       REVERSE (flip) order.
     */
    if ( voxel_order == 0 ) {
      for ( i = start_position; i < end_position; i++ ) {
        widths[j] = dimension->widths[i];
        j++;
      }
    } else {
      for ( i = end_position-1; i >= start_position; i-- ) {
        widths[j] = dimension->widths[i];
        j++;
      }
    }

  }
  
  return MI_NOERROR;
}

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
int miset_dimension_widths ( midimhandle_t dimension,
                         misize_t array_length,
                         misize_t start_position,
                         const double widths[] )
{
  misize_t end_position;
  misize_t i, j;

  /* Check to see whether the dimension is regularly sampled.
   */
  if ( dimension == NULL ||
       ( dimension->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED ) == 0 ||
       start_position > dimension->length ) {
    return ( MI_ERROR );
  }

  if ( ( start_position + array_length ) > dimension->length ) {
    end_position = dimension->length;
  } else {
    end_position = start_position + array_length;
  }

  /* Allocate space for widths array if not already done
   */
  if ( dimension->widths == NULL ) {
    dimension->widths = ( double * ) malloc ( dimension->length * sizeof ( double ) );
  }

  for ( i = start_position, j = 0; i < end_position; i++, j++ ) {
    if ( widths[j] < 0 ) { /* NOTE: is this test even useful? */
      dimension->widths[i] = -1 * widths[j];
    } else {
      dimension->widths[i] = widths[j];
    }
  }

  return ( MI_NOERROR );
}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
