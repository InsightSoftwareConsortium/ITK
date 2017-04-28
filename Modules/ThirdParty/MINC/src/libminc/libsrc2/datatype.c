/** \file datatype.c
 * \brief MINC 2.0 data type/space functions
 ************************************************************************/

#include <stdlib.h>
#include <hdf5.h>
#include "minc2.h"
#include "minc2_private.h"

/** Return the data class of a volume (See miclass_t).
 */
int miget_data_class ( mihandle_t volume, miclass_t *volume_class )
{
  *volume_class = volume->volume_class;
  return ( MI_NOERROR );
}

/** Return the data type of a volume (See mitype_t).
 */
int miget_data_type ( mihandle_t volume, mitype_t *data_type )
{
  *data_type = volume->volume_type;
  return ( MI_NOERROR );
}

/** Return the byte size of the voxel datatytpe
 */
int miget_data_type_size ( mihandle_t volume, misize_t *voxel_size )
{
  hid_t grp_id;
  hid_t dset_id;
  hid_t type_id;
  hid_t file_id = volume->hdf_id;

  grp_id = midescend_path ( file_id, MI_FULLIMAGE_PATH );

  if ( grp_id < 0 ) {
    return ( MI_ERROR );
  }

  dset_id = H5Dopen1 ( grp_id, "image" );

  if ( dset_id < 0 ) {
    return ( MI_ERROR );
  }

  type_id = H5Dget_type ( dset_id );

  if ( type_id < 0 ) {
    return ( MI_ERROR );
  }

  *voxel_size = H5Tget_size ( type_id );

  H5Tclose ( type_id );
  H5Dclose ( dset_id );
  H5Gclose ( grp_id );

  return ( MI_NOERROR );
}

/** Return the minc space type, name should be freed after use
 */
int miget_space_name ( mihandle_t volume, char **name )
{
  size_t length = 0;
  int result = MI_ERROR;
  int i;
  /* This is the order of the search for candidates for the space type.
     The reason for this is complication is to permit support for older-style
     MINC files which associate the spacetype with individual dimensions.
  */
  static const char *path_list[] = {
    MI_ROOT_PATH "/" MI_INFO_NAME,
    MI_ROOT_PATH "/dimensions/xspace",
    MI_ROOT_PATH "/dimensions/yspace",
    MI_ROOT_PATH "/dimensions/zspace",
    NULL
  };

  /* Search for the spacetype attribute in all available paths.
   */
  for ( i = 0; path_list[i] != 0; i++ ) {
    result = miget_attr_length ( volume, path_list[i], "spacetype",
                                 &length );

    if ( result == MI_NOERROR ) {
      break;
    }
  }

  if ( result != MI_NOERROR ) {
    /* Nothing found, so use the default.
     */
    length = strlen ( MI_NATIVE );
    *name = malloc ( length + 1 );
    strcpy ( *name, MI_NATIVE );
  } else {
    *name = malloc ( length + 1 );
    result = miget_attr_values ( volume, MI_TYPE_STRING, path_list[i],
                                 "spacetype", length+1, *name );
  }

  return ( result );
}

/**
 * Set minc space type
 */
int miset_space_name ( mihandle_t volume, const char *name )
{
  return miset_attr_values ( volume, MI_TYPE_STRING, MI_ROOT_PATH "/" MI_INFO_NAME,
                             "spacetype", strlen ( name ), name );

}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
