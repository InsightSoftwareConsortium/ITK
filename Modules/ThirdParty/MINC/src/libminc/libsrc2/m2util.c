/** \file m2util.c
* \brief MINC 2.0 Private utility functions
* \author Leila Baghdadi, Bert Vincent
*
************************************************************************/
#include <hdf5.h>

#include <stdlib.h>
#include <math.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <limits.h>
#include <float.h>

#include "minc2.h"
#include "minc2_private.h"

#ifndef HAVE_RINT
double rint(double v)
{
  return floor(v+0.5);
}
#endif


/*! Convert a MINC 2 datatype into a HDF5 datatype.  Actually returns a copy
* of the datatype, so the returned value must be explicitly freed with a
* call to H5Tclose().
* \param mitype The MINC 2 data type to convert
* \param is_native Convert to native type if TRUE
*/
hid_t mitype_to_hdftype ( mitype_t mitype, int is_native )
{
  hid_t type_id;

  if ( is_native ) {
    /* Native types are in the byte-ordering of the native system.
    * They are appropriate for the "in-memory" types for data.
    */
    switch ( mitype ) {
    case MI_TYPE_BYTE:
      type_id = H5Tcopy ( H5T_NATIVE_SCHAR );
      break;
    case MI_TYPE_SHORT:
      type_id = H5Tcopy ( H5T_NATIVE_SHORT );
      break;
    case MI_TYPE_INT:
      type_id = H5Tcopy ( H5T_NATIVE_INT );
      break;
    case MI_TYPE_FLOAT:
      type_id = H5Tcopy ( H5T_NATIVE_FLOAT );
      break;
    case MI_TYPE_DOUBLE:
      type_id = H5Tcopy ( H5T_NATIVE_DOUBLE );
      break;
    case MI_TYPE_UBYTE:
      type_id = H5Tcopy ( H5T_NATIVE_UCHAR );
      break;
    case MI_TYPE_USHORT:
      type_id = H5Tcopy ( H5T_NATIVE_USHORT );
      break;
    case MI_TYPE_UINT:
      type_id = H5Tcopy ( H5T_NATIVE_UINT );
      break;
    case MI_TYPE_SCOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 4 );
      H5Tinsert ( type_id, "real", 0, H5T_NATIVE_SHORT );
      H5Tinsert ( type_id, "imag", 2, H5T_NATIVE_SHORT );
      break;
    case MI_TYPE_ICOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 8 );
      H5Tinsert ( type_id, "real", 0, H5T_NATIVE_INT );
      H5Tinsert ( type_id, "imag", 4, H5T_NATIVE_INT );
      break;
    case MI_TYPE_FCOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 8 );
      H5Tinsert ( type_id, "real", 0, H5T_NATIVE_FLOAT );
      H5Tinsert ( type_id, "imag", 4, H5T_NATIVE_FLOAT );
      break;
    case MI_TYPE_DCOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 16 );
      H5Tinsert ( type_id, "real", 0, H5T_NATIVE_DOUBLE );
      H5Tinsert ( type_id, "imag", 8, H5T_NATIVE_DOUBLE );
      break;
    default:
      type_id = H5Tcopy ( mitype ); /* It is a standard HDF type handle? */
      break;
    }
  } else {
    /* The non-native types are standardized to be in
    * "little-endian" form.  That's an arbitrary decision which
    * could certainly be debated.
    */
    switch ( mitype ) {
    case MI_TYPE_BYTE:
      type_id = H5Tcopy ( H5T_STD_I8LE );
      break;
    case MI_TYPE_SHORT:
      type_id = H5Tcopy ( H5T_STD_I16LE );
      break;
    case MI_TYPE_INT:
      type_id = H5Tcopy ( H5T_STD_I32LE );
      break;
    case MI_TYPE_FLOAT:
      type_id = H5Tcopy ( H5T_IEEE_F32LE );
      break;
    case MI_TYPE_DOUBLE:
      type_id = H5Tcopy ( H5T_IEEE_F64LE );
      break;
    case MI_TYPE_UBYTE:
      type_id = H5Tcopy ( H5T_STD_U8LE );
      break;
    case MI_TYPE_USHORT:
      type_id = H5Tcopy ( H5T_STD_U16LE );
      break;
    case MI_TYPE_UINT:
      type_id = H5Tcopy ( H5T_STD_U32LE );
      break;
    case MI_TYPE_SCOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 4 );
      H5Tinsert ( type_id, "real", 0, H5T_STD_I16LE );
      H5Tinsert ( type_id, "imag", 2, H5T_STD_I16LE );
      break;
    case MI_TYPE_ICOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 8 );
      H5Tinsert ( type_id, "real", 0, H5T_STD_I32LE );
      H5Tinsert ( type_id, "imag", 4, H5T_STD_I32LE );
      break;
    case MI_TYPE_FCOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 8 );
      H5Tinsert ( type_id, "real", 0, H5T_IEEE_F32LE );
      H5Tinsert ( type_id, "imag", 4, H5T_IEEE_F32LE );
      break;
    case MI_TYPE_DCOMPLEX:
      type_id = H5Tcreate ( H5T_COMPOUND, 16 );
      H5Tinsert ( type_id, "real", 0, H5T_IEEE_F64LE );
      H5Tinsert ( type_id, "imag", 8, H5T_IEEE_F64LE );
      break;
    default:
      type_id = H5Tcopy ( mitype ); /* It is a standard HDF type handle? */
      break;
    }
  }

  return ( type_id );
}

/** get byte size of a specific data type
 */
int mitype_len ( mitype_t mitype )
{
  switch ( mitype ) {
  case MI_TYPE_BYTE:
  case MI_TYPE_UBYTE:
    return 1;
  case MI_TYPE_USHORT:
  case MI_TYPE_SHORT:
    return 2;
  case MI_TYPE_INT:
  case MI_TYPE_UINT:
    return 4;
  case MI_TYPE_FLOAT:
    return 4;
  case MI_TYPE_DOUBLE:
    return 8;
  case MI_TYPE_SCOMPLEX:
    return 2 * 2;
  case MI_TYPE_ICOMPLEX:
    return 2 * 4;
  case MI_TYPE_FCOMPLEX:
    return 2 * 4;
  case MI_TYPE_DCOMPLEX:
    return 2 * 8;
  default:
    break;
  }

  fprintf ( stderr, _ ( "Unknown type %d" ), ( int ) mitype );
  return ( -1 );
}

/** get string identifying if datatype is signed
 */
const char * mitype_sign ( mitype_t mitype )
{
  switch ( mitype ) {
    case MI_TYPE_BYTE:
    case MI_TYPE_SHORT:
    case MI_TYPE_INT:
    case MI_TYPE_FLOAT:
    case MI_TYPE_DOUBLE:
    case MI_TYPE_SCOMPLEX:
    case MI_TYPE_ICOMPLEX:
    case MI_TYPE_FCOMPLEX:
    case MI_TYPE_DCOMPLEX:
      return MI_SIGNED;
    case MI_TYPE_UBYTE:
    case MI_TYPE_USHORT:
    case MI_TYPE_UINT:
      return MI_UNSIGNED;
    default:
      break;
  }
  fprintf ( stderr, _ ( "Unknown type %d" ), ( int ) mitype );
  return MI_SIGNED;
}


/*! Convert a MINC 2 datatype into a NetCDF datatype.
* \param mitype The MINC 2 data type to convert
* \param is_signed Set to TRUE if the data type is signed, FALSE if unsigned.
*/
/* TODO:CNV */

#if 0

int mitype_to_nctype ( mitype_t mitype, int *is_signed )
{
  int nctype;

  *is_signed = 1;   /* Assume signed by default. */

  switch ( mitype ) {
  case MI_TYPE_BYTE:
    nctype = NC_BYTE;
    break;
  case MI_TYPE_SHORT:
    nctype = NC_SHORT;
    break;
  case MI_TYPE_INT:
    nctype = NC_INT;
    break;
  case MI_TYPE_FLOAT:
    nctype = NC_FLOAT;
    break;
  case MI_TYPE_DOUBLE:
    nctype = NC_DOUBLE;
    break;
  case MI_TYPE_UBYTE:
    nctype = NC_BYTE;
    *is_signed = 0;
    break;
  case MI_TYPE_USHORT:
    nctype = NC_SHORT;
    *is_signed = 0;
    break;
  case MI_TYPE_UINT:
    nctype = NC_INT;
    *is_signed = 1;
    break;
  default:
    nctype = -1;    /* ERROR!! */
    break;
  }

  return ( nctype );
}
#endif /*TODO:CNV*/

/*! Return the group or dataset ID of the last item in a "path",
* specified like a UNIX pathname /black/white/red etc.
* \param file_id The HDF5 handle of the file (or group) at which to start
* the search.
* \param path A string consisting of a slash-separated list of
* HDF5 groupnames
*/
hid_t midescend_path ( hid_t file_id, const char *path )
{
  hid_t tmp_id;

  /* Put H5E_BEGIN_TRY/H5E_END_TRY around this to avoid the overzealous
  * automatic error reporting of HDF5.
  */
  H5E_BEGIN_TRY {
    tmp_id = H5Dopen1 ( file_id, path );

    /* If the dataset open fails, try opening the object as a group.
    */
    if ( tmp_id < 0 ) {
      tmp_id = H5Gopen1 ( file_id, path );
    }
  } H5E_END_TRY;
  return ( tmp_id );
}


int miset_attr_at_loc ( hid_t hdf_loc, const char *name, mitype_t data_type,
                        size_t length, const void *values )
{
  hid_t ftyp_id=-1;
  hid_t mtyp_id=-1;
  hid_t spc_id=-1;
  hid_t hdf_attr=-1;
  hsize_t hdf_len;
  int status=MI_ERROR;

  H5E_BEGIN_TRY {
    /* Delete attribute if it already exists. */
    H5Adelete ( hdf_loc, name );
  } H5E_END_TRY;

  switch ( data_type ) {
  case MI_TYPE_INT:
    ftyp_id = H5Tcopy ( H5T_STD_I32LE );
    mtyp_id = H5Tcopy ( H5T_NATIVE_INT );
    break;
  case MI_TYPE_FLOAT:
    ftyp_id = H5Tcopy ( H5T_IEEE_F32LE );
    mtyp_id = H5Tcopy ( H5T_NATIVE_FLOAT );
    break;
  case MI_TYPE_DOUBLE:
    ftyp_id = H5Tcopy ( H5T_IEEE_F64LE );
    mtyp_id = H5Tcopy ( H5T_NATIVE_DOUBLE );
    break;
  case MI_TYPE_STRING:
    ftyp_id = H5Tcopy ( H5T_C_S1 );
    H5Tset_size ( ftyp_id, length );
    mtyp_id = H5Tcopy ( ftyp_id );
    length = 1;             /* Apparent length is one. */
    break;
  default:
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Unsupported attribute type");
  }

  if ( length == 1 ) {
    if( (spc_id = H5Screate ( H5S_SCALAR ))<0)
      goto cleanup;
  } else {
    hdf_len = ( hsize_t ) length;
    if( (spc_id = H5Screate_simple ( 1, &hdf_len, NULL ))<0 )
      goto cleanup;
  }

  if((hdf_attr = H5Acreate2 ( hdf_loc, name, ftyp_id, spc_id, H5P_DEFAULT, H5P_DEFAULT  ))<0)
    goto cleanup;
  
  
  if(H5Awrite ( hdf_attr, mtyp_id, values ) < 0) 
    goto cleanup;
  
  status=MI_NOERROR;

cleanup:  
  if(hdf_attr >=0 )  H5Aclose ( hdf_attr );
  if(ftyp_id  >=0 )  H5Tclose ( ftyp_id );
  if(mtyp_id  >=0 )  H5Tclose ( mtyp_id );
  if(spc_id   >=0 )  H5Sclose ( spc_id );
  return status;
}

/** Set an attribute from a minc file */
int miset_attribute ( mihandle_t volume, const char *path, const char *name,
                      mitype_t data_type, size_t length, const void *values )
{
  hid_t hdf_file;
  hid_t hdf_loc;

  /* Get a handle to the actual HDF file
  */
  hdf_file = volume->hdf_id;

  if ( hdf_file < 0 ) {
    return ( MI_ERROR );
  }

  /* Search through the path, descending into each group encountered.
  */
  hdf_loc = midescend_path ( hdf_file, path );

  if ( hdf_loc < 0 ) {
    return ( MI_ERROR );
  }

  miset_attr_at_loc ( hdf_loc, name, data_type, length, values );

  /* The hdf_loc identifier could be a group or a dataset.
  */
  if ( H5Iget_type ( hdf_loc ) == H5I_GROUP ) {
    H5Gclose ( hdf_loc );
  } else {
    H5Dclose ( hdf_loc );
  }

  return ( MI_NOERROR );
}

/** Get a double attribute from a minc file */
int miget_attribute ( mihandle_t volume, const char *path, const char *name,
                      mitype_t data_type, size_t length, void *values )
{
  hid_t hdf_file;
  hid_t hdf_loc;
  hid_t mtyp_id = -1;         /* Parameter type */
  hid_t spc_id = -1;
  hid_t hdf_attr = -1;
  int status = MI_ERROR;      /* Guilty until proven innocent */

  /* Get a handle to the actual HDF file
  */
  hdf_file = volume->hdf_id;

  if ( hdf_file < 0 ) {
    return ( MI_ERROR );
  }

  /* Find the group or dataset referenced by the path.
  */
  hdf_loc = midescend_path ( hdf_file, path );

  if ( hdf_loc < 0 ) {
    return ( MI_ERROR );
  }

  H5E_BEGIN_TRY {
    hdf_attr = H5Aopen_name ( hdf_loc, name );
  } H5E_END_TRY;

  if ( hdf_attr < 0 ) {
    goto cleanup;
  }

  switch ( data_type ) {
  case MI_TYPE_INT:
    mtyp_id = H5Tcopy ( H5T_NATIVE_INT );
    break;
  case MI_TYPE_UINT:
    mtyp_id = H5Tcopy ( H5T_NATIVE_UINT );
    break;
  case MI_TYPE_FLOAT:
    mtyp_id = H5Tcopy ( H5T_NATIVE_FLOAT );
    break;
  case MI_TYPE_DOUBLE:
    mtyp_id = H5Tcopy ( H5T_NATIVE_DOUBLE );
    break;
  case MI_TYPE_STRING:
    mtyp_id = H5Tcopy ( H5T_C_S1 );
    H5Tset_size ( mtyp_id, length );
    break;
  default:
    goto cleanup;
  }

  spc_id = H5Aget_space ( hdf_attr );

  if ( spc_id < 0 ) {
    goto cleanup;
  }

  /* If we're retrieving a vector, make certain the length passed into this
  * function is sufficient.
  */
  if ( H5Sget_simple_extent_ndims ( spc_id ) == 1 ) {
    hsize_t hdf_dims[1];

    H5Sget_simple_extent_dims ( spc_id, hdf_dims, NULL );

    if ( length < hdf_dims[0] ) {
      goto cleanup;
    }
  }

  if ( H5Aread ( hdf_attr, mtyp_id, values ) < 0 ) {
    goto cleanup;
  }

  status = MI_NOERROR;        /* We succeeded! */

  /* Be certain that the string is null-terminated.
  */
  if ( data_type == MI_TYPE_STRING ) {
    hid_t atype;            /* Attribute type */
    size_t alength;

    atype = H5Aget_type ( hdf_attr );
    alength = H5Tget_size ( atype );

    ( ( char * ) values ) [alength] = '\0';

    H5Tclose ( atype );
  }

cleanup:

  if ( hdf_attr >= 0 ) {
    H5Aclose ( hdf_attr );
  }

  if ( mtyp_id >= 0 ) {
    H5Tclose ( mtyp_id );
  }

  if ( spc_id >= 0 ) {
    H5Sclose ( spc_id );
  }

  if ( hdf_loc >= 0 ) {
    /* The hdf_loc identifier could be a group or a dataset.
    */
    if ( H5Iget_type ( hdf_loc ) == H5I_GROUP ) {
      H5Gclose ( hdf_loc );
    } else {
      H5Dclose ( hdf_loc );
    }
  }

  return ( status );
}

/* Get the mapping from spatial dimension - x, y, z - to file dimensions
* and vice-versa.
*/
/*TODO: convert to MINC2 API?*/
#if 0
void mifind_spatial_dims(int mincid, int space_to_dim[], int dim_to_space[])
{
  int imgid;
  int dim[MI2_MAX_VAR_DIMS];
  int idim, ndims, world_index;
  char dimname[MI2_MAX_DIM_NAME];

  /* Set default values */
  for (idim = 0; idim < 3; idim++)
    space_to_dim[idim] = -1;

  for (idim = 0; idim < MI2_MAX_VAR_DIMS; idim++)
    dim_to_space[idim] = -1;

  /* Get the dimension ids for the image variable
  */
  imgid = ncvarid(mincid, MIimage);
  ncvarinq(mincid, imgid, NULL, NULL, &ndims, dim, NULL);

  /* Loop over them to find the spatial ones
  */
  for (idim = 0; idim < ndims; idim++) {
    /* Get the name and check that this is a spatial dimension
    */
    ncdiminq(mincid, dim[idim], dimname, NULL);

    if (!strcmp(dimname, MIxspace)) {
      world_index = MI2_X;
    } else if (!strcmp(dimname, MIyspace)) {
      world_index = MI2_Y;
    } else if (!strcmp(dimname, MIzspace)) {
      world_index = MI2_Z;
    } else {
      continue;
    }

    space_to_dim[world_index] = idim;
    dim_to_space[idim] = world_index;
  }
}
#endif 

/** Get the voxel to world transform (for column vectors) */
void miget_voxel_to_world ( mihandle_t volume, mi_lin_xfm_t voxel_to_world )
{
  int i;
  int j;
  int k;
  double dircos[MI2_3D];
  double step;
  double start;

  /* Zero the matrix */
  for ( i = 0; i < MI2_LIN_XFM_SIZE; i++ ) {
    for ( j = 0; j < MI2_LIN_XFM_SIZE; j++ ) {
      voxel_to_world[i][j] = 0.0;
    }

    voxel_to_world[i][i] = 1.0;
  }

  for ( j = 0; j < volume->number_of_dims; j++ ) {
    midimhandle_t hdim = volume->dim_handles[j];

    if ( hdim->dim_class == MI_DIMCLASS_SPATIAL ||
         hdim->dim_class == MI_DIMCLASS_SFREQUENCY ) {
      k = hdim->world_index;
    } else {
      continue;
    }

    start = hdim->start;
    step = hdim->step;
    dircos[0] = hdim->direction_cosines[0];
    dircos[1] = hdim->direction_cosines[1];
    dircos[2] = hdim->direction_cosines[2];

    minormalize_vector ( dircos );

    /* Put them in the matrix */
    for ( i = 0; i < MI2_3D; i++ ) {
      voxel_to_world[i][k] = step * dircos[i];
      voxel_to_world[i][MI2_3D] += start * dircos[i];
    }
  }
}

/** Normalize a 3 element vector */
void minormalize_vector ( double vector[MI2_3D] )
{
  int i;
  double magnitude;

  magnitude = 0.0;

  for ( i = 0; i < MI2_3D; i++ ) {
    magnitude += ( vector[i] * vector[i] );
  }

  magnitude = sqrt ( magnitude );

  if ( magnitude > 0.0 ) {
    for ( i = 0; i < MI2_3D; i++ ) {
      vector[i] /= magnitude;
    }
  }
}

/** Transforms a coordinate through a linear transform */
void mitransform_coord ( double out_coord[],
                         mi_lin_xfm_t transform,
                         const double in_coord[] )
{
  int i, j;
  double in_homogeneous[MI2_3D + 1];
  double out_homogeneous[MI2_3D + 1];

  for ( i = 0; i < MI2_3D; i++ ) {
    in_homogeneous[i] = in_coord[i];
  }

  in_homogeneous[MI2_3D] = 1.0;

  for ( i = 0; i < MI2_3D + 1; i++ ) {
    out_homogeneous[i] = 0.0;

    for ( j = 0; j < MI2_3D + 1; j++ ) {
      out_homogeneous[i] += transform[i][j] * in_homogeneous[j];
    }
  }

#if 0
  printf ( "W = %f\n", out_homogeneous[3] );
#endif /* 0 */

  for ( i = 0; i < MI2_3D; i++ ) {
    out_coord[i] = out_homogeneous[i];
  }
}

/** For conversions from double to integer, rounding may be performed
* by setting this variable to non-zero.
* However, at present, no API is available to control this.
*/
static int rounding_enabled = FALSE;

static void miswap8 ( unsigned char *tmp_ptr )
{
  unsigned char x;

  x = tmp_ptr[0];
  tmp_ptr[0] = tmp_ptr[7];
  tmp_ptr[7] = x;

  x = tmp_ptr[1];
  tmp_ptr[1] = tmp_ptr[6];
  tmp_ptr[6] = x;

  x = tmp_ptr[2];
  tmp_ptr[2] = tmp_ptr[5];
  tmp_ptr[5] = x;

  x = tmp_ptr[3];
  tmp_ptr[3] = tmp_ptr[4];
  tmp_ptr[4] = x;
}

static void miswap4 ( unsigned char *tmp_ptr )
{
  unsigned char x;

  x = tmp_ptr[0];
  tmp_ptr[0] = tmp_ptr[3];
  tmp_ptr[3] = x;

  x = tmp_ptr[1];
  tmp_ptr[1] = tmp_ptr[2];
  tmp_ptr[2] = x;
}

static void miswap2 ( unsigned char *tmp_ptr )
{
  unsigned char x;

  x = tmp_ptr[0];
  tmp_ptr[0] = tmp_ptr[1];
  tmp_ptr[1] = x;
}

/** Generic HDF5 integer-to-double converter.
*/

static herr_t mi2_int_to_dbl ( hid_t src_id,
                               hid_t dst_id,
                               H5T_cdata_t *cdata,
                               size_t nelements,
                               size_t buf_stride,
                               size_t bkg_stride,
                               void *buf_ptr,
                               void *bkg_ptr,
                               hid_t dset_xfer_plist )
{
  unsigned char *dst_ptr;
  unsigned char *src_ptr;
  size_t src_nb;
  size_t dst_nb;
  H5T_sign_t src_sg;
  double t;
  size_t dst_cnt;
  size_t src_cnt;
  int src_swap;
  int dst_swap;

  switch ( cdata->command ) {
  case H5T_CONV_INIT:
    cdata->need_bkg = H5T_BKG_NO;
    src_nb = H5Tget_size ( src_id );

    if ( src_nb != 1 && src_nb != 2 && src_nb != 4 ) {
      return ( -1 );
    }

    dst_nb = H5Tget_size ( dst_id );

    if ( dst_nb != 8 ) {
      return ( -1 );
    }

    break;

  case H5T_CONV_CONV:
    src_nb = H5Tget_size ( src_id );
    src_sg = H5Tget_sign ( src_id );
    dst_nb = H5Tget_size ( dst_id );

    if ( buf_stride == 0 ) {
      dst_cnt = dst_nb;
      src_cnt = src_nb;
    } else {
      dst_cnt = buf_stride;
      src_cnt = buf_stride;
    }

    /* Convert starting from "far side" of buffer... (Hope this works!)
    */
    dst_ptr = ( ( unsigned char * ) buf_ptr ) + ( ( nelements - 1 ) * dst_nb );
    src_ptr = ( ( unsigned char * ) buf_ptr ) + ( ( nelements - 1 ) * src_nb );

    if ( H5Tget_order ( H5T_NATIVE_INT ) != H5Tget_order ( src_id ) ) {
      src_swap = 1;
    } else {
      src_swap = 0;
    }

    if ( H5Tget_order ( H5T_NATIVE_DOUBLE ) != H5Tget_order ( dst_id ) ) {
      dst_swap = 1;
    } else {
      dst_swap = 0;
    }

    if ( src_sg == H5T_SGN_2 ) {
      switch ( src_nb ) {
      case 4:

        while ( nelements-- > 0 ) {
          if ( src_swap ) {
            miswap4 ( src_ptr );
          }

          t = * ( int * ) src_ptr;
          * ( double * ) dst_ptr = t;

          if ( dst_swap ) {
            miswap8 ( dst_ptr );
          }

          src_ptr -= src_cnt;
          dst_ptr -= dst_cnt;
        }

        break;
      case 2:

        while ( nelements-- > 0 ) {
          if ( src_swap ) {
            miswap2 ( src_ptr );
          }

          t = * ( short * ) src_ptr;
          * ( double * ) dst_ptr = t;

          if ( dst_swap ) {
            miswap8 ( dst_ptr );
          }

          src_ptr -= src_cnt;
          dst_ptr -= dst_cnt;
        }

        break;
      case 1:

        while ( nelements-- > 0 ) {
          t = * ( char * ) src_ptr;
          * ( double * ) dst_ptr = t;

          if ( dst_swap ) {
            miswap8 ( dst_ptr );
          }

          src_ptr -= src_cnt;
          dst_ptr -= dst_cnt;
        }

        break;
      default:
        /* Should not happen! */
        break;
      }
    } else {
      switch ( src_nb ) {
      case 4:

        while ( nelements-- > 0 ) {
          if ( src_swap ) {
            miswap4 ( src_ptr );
          }

          t = * ( unsigned int * ) src_ptr;
          * ( double * ) dst_ptr = t;

          if ( dst_swap ) {
            miswap8 ( dst_ptr );
          }

          src_ptr -= src_cnt;
          dst_ptr -= dst_cnt;
        }

        break;
      case 2:

        while ( nelements-- > 0 ) {
          if ( src_swap ) {
            miswap2 ( src_ptr );
          }

          t = * ( unsigned short * ) src_ptr;
          * ( double * ) dst_ptr = t;

          if ( dst_swap ) {
            miswap8 ( dst_ptr );
          }

          src_ptr -= src_cnt;
          dst_ptr -= dst_cnt;
        }

        break;
      case 1:

        while ( nelements-- > 0 ) {
          t = * ( unsigned char * ) src_ptr;
          * ( double * ) dst_ptr = t;

          if ( dst_swap ) {
            miswap8 ( dst_ptr );
          }

          src_ptr -= src_cnt;
          dst_ptr -= dst_cnt;
        }

        break;
      default:
        /* Should not happen! */
        break;
      }
    }

    break;

  case H5T_CONV_FREE:
    break;

  default:
    /* Unknown command */
    return ( -1 );
  }

  return ( 0 );
}

/** Generic HDF5 double-to-integer converter.
*/
static herr_t mi2_dbl_to_int ( hid_t src_id,
                               hid_t dst_id,
                               H5T_cdata_t *cdata,
                               size_t nelements,
                               size_t buf_stride,
                               size_t bkg_stride,
                               void *buf_ptr,
                               void *bkg_ptr,
                               hid_t dset_xfer_plist )
{
  unsigned char *dst_ptr;
  unsigned char *src_ptr;
  size_t src_nb;
  size_t dst_nb;
  H5T_sign_t dst_sg;
  double t;
  size_t dst_cnt;
  size_t src_cnt;
  int src_swap;
  int dst_swap;

  switch ( cdata->command ) {
  case H5T_CONV_INIT:
    cdata->need_bkg = H5T_BKG_NO;
    /* Verify that we can handle this conversion.
    */
    src_nb = H5Tget_size ( src_id );

    if ( src_nb != 8 ) {
      return ( -1 );
    }

    dst_nb = H5Tget_size ( dst_id );

    if ( dst_nb != 4 && dst_nb != 2 && dst_nb != 1 ) {
      return ( -1 );
    }

    break;

  case H5T_CONV_CONV:
    dst_nb = H5Tget_size ( dst_id );
    dst_sg = H5Tget_sign ( dst_id );
    src_nb = H5Tget_size ( src_id );
    dst_ptr = ( unsigned char * ) buf_ptr;
    src_ptr = ( unsigned char * ) buf_ptr;

    if ( H5Tget_order ( H5T_NATIVE_DOUBLE ) != H5Tget_order ( src_id ) ) {
      src_swap = 1;
    } else {
      src_swap = 0;
    }

    if ( H5Tget_order ( H5T_NATIVE_INT ) != H5Tget_order ( dst_id ) ) {
      dst_swap = 1;
    } else {
      dst_swap = 0;
    }

    /* The logic of HDF5 seems to be that if a stride is specified,
    * both the source and destination pointers should advance by that
    * amount.  This seems wrong to me, but I've examined the HDF5 sources
    * and that's what their own type converters do.
    */
    if ( buf_stride == 0 ) {
      dst_cnt = dst_nb;
      src_cnt = src_nb;
    } else {
      dst_cnt = buf_stride;
      src_cnt = buf_stride;
    }

    if ( rounding_enabled ) {
      if ( dst_sg == H5T_SGN_2 ) {
        switch ( dst_nb ) {
        case 4:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = rint ( * ( double * ) src_ptr );

            if ( t > INT_MAX ) {
              t = INT_MAX;
            } else
              if ( t < INT_MIN ) {
                t = INT_MIN;
              }

            * ( ( int * ) dst_ptr ) = ( int ) t;

            if ( dst_swap ) {
              miswap4 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        case 2:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = rint ( * ( double * ) src_ptr );

            if ( t > SHRT_MAX ) {
              t = SHRT_MAX;
            } else
              if ( t < SHRT_MIN ) {
                t = SHRT_MIN;
              }

            * ( ( short * ) dst_ptr ) = ( short ) t;

            if ( dst_swap ) {
              miswap2 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        case 1:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = rint ( * ( double * ) src_ptr );

            if ( t > CHAR_MAX ) {
              t = CHAR_MAX;
            } else
              if ( t < CHAR_MIN ) {
                t = CHAR_MIN;
              }

            * ( ( char * ) src_ptr ) = ( char ) t;
            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        default:
          /* Can't handle this! */
          break;
        }
      } else {
        switch ( dst_nb ) {
        case 4:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = rint ( * ( double * ) src_ptr );

            if ( t > UINT_MAX ) {
              t = UINT_MAX;
            } else
              if ( t < 0 ) {
                t = 0;
              }

            * ( ( unsigned int * ) dst_ptr ) = ( unsigned int ) t;

            if ( dst_swap ) {
              miswap4 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;

        case 2:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = rint ( * ( double * ) src_ptr );

            if ( t > USHRT_MAX ) {
              t = USHRT_MAX;
            } else
              if ( t < 0 ) {
                t = 0;
              }

            * ( ( unsigned short * ) dst_ptr ) = ( unsigned short ) t;

            if ( dst_swap ) {
              miswap2 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        case 1:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = rint ( * ( double * ) src_ptr );

            if ( t > UCHAR_MAX ) {
              t = UCHAR_MAX;
            } else
              if ( t < 0 ) {
                t = 0;
              }

            * ( ( unsigned char * ) dst_ptr ) = ( unsigned char ) t;
            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        default:
          /* Can't handle any other values */
          break;
        }
      }
    } else {
      if ( dst_sg == H5T_SGN_2 ) {
        switch ( dst_nb ) {
        case 4:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = ( int ) ( * ( double * ) src_ptr );

            if ( t > INT_MAX ) {
              t = INT_MAX;
            } else
              if ( t < INT_MIN ) {
                t = INT_MIN;
              }

            * ( ( int * ) dst_ptr ) = ( int ) t;

            if ( dst_swap ) {
              miswap4 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        case 2:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = ( int ) ( * ( double * ) src_ptr );

            if ( t > SHRT_MAX ) {
              t = SHRT_MAX;
            } else
              if ( t < SHRT_MIN ) {
                t = SHRT_MIN;
              }

            * ( ( short * ) dst_ptr ) = ( short ) t;

            if ( dst_swap ) {
              miswap4 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        case 1:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = ( int ) ( * ( double * ) src_ptr );

            if ( t > CHAR_MAX ) {
              t = CHAR_MAX;
            } else
              if ( t < CHAR_MIN ) {
                t = CHAR_MIN;
              }

            * ( ( char * ) src_ptr ) = ( char ) t;
            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        default:
          /* Can't handle this! */
          break;
        }
      } else {
        switch ( dst_nb ) {
        case 4:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = ( int ) ( * ( double * ) src_ptr );

            if ( t > UINT_MAX ) {
              t = UINT_MAX;
            } else
              if ( t < 0 ) {
                t = 0;
              }

            * ( ( unsigned int * ) dst_ptr ) = ( unsigned int ) t;

            if ( dst_swap ) {
              miswap4 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;

        case 2:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = ( int ) ( * ( double * ) src_ptr );

            if ( t > USHRT_MAX ) {
              t = USHRT_MAX;
            } else
              if ( t < 0 ) {
                t = 0;
              }

            * ( ( unsigned short * ) dst_ptr ) = ( unsigned short ) t;

            if ( dst_swap ) {
              miswap2 ( dst_ptr );
            }

            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        case 1:

          while ( nelements-- > 0 ) {
            if ( src_swap ) {
              miswap8 ( src_ptr );
            }

            t = ( int ) ( * ( double * ) src_ptr );

            if ( t > UCHAR_MAX ) {
              t = UCHAR_MAX;
            } else
              if ( t < 0 ) {
                t = 0;
              }

            * ( ( unsigned char * ) dst_ptr ) = ( unsigned char ) t;
            dst_ptr += dst_cnt;
            src_ptr += src_cnt;
          }

          break;
        default:
          /* Can't handle any other values */
          break;
        }
      }
    }

    break;

  case H5T_CONV_FREE:
    break;

  default:
    /* Unknown command */
    return ( -1 );
  }

  return ( 0 );
}


/** Initialize some critical pieces of the library.  For now all this does
is install the double-to-integer and integer-to-double conversion functions.
*/
void miinit ( void )
{
  MI_CHECK_HDF_CALL(H5Tregister ( H5T_PERS_SOFT, "i2d", H5T_NATIVE_INT, H5T_NATIVE_DOUBLE,
                mi2_int_to_dbl ),"H5Tregister")

  MI_CHECK_HDF_CALL(H5Tregister ( H5T_PERS_SOFT, "d2i", H5T_NATIVE_DOUBLE, H5T_NATIVE_INT,
                mi2_dbl_to_int ),"H5Tregister")
}

/** HDF5 type conversion function for converting among integer types.
 * This handles byte-swapping for enumerated types where needed.
 */
static herr_t mi2_int_to_int ( hid_t src_id,
			       hid_t dst_id,
			       H5T_cdata_t *cdata,
			       size_t nelements,
			       size_t buf_stride,
			       size_t bkg_stride,
			       void *buf_ptr,
			       void *bkg_ptr,
			       hid_t dset_xfer_plist )
{
  unsigned char *dst_ptr;
  size_t dst_cnt;
  size_t dst_sz;

  switch ( cdata->command ) {
  case H5T_CONV_INIT:
    break;
  case H5T_CONV_CONV:
    dst_ptr = ( unsigned char * ) buf_ptr;
    dst_sz = H5Tget_size( dst_id );

    if (dst_sz != H5Tget_size( src_id )) {
      return -1;		/* Can't change size for now. */
    }
    if ( H5Tget_order ( dst_id ) == H5Tget_order ( src_id ) ) {
      return 0;			/* Nothing to do. */
    }

    /* The logic of HDF5 seems to be that if a stride is specified,
    * both the source and destination pointers should advance by that
    * amount.  This seems wrong to me, but I've examined the HDF5 sources
    * and that's what their own type converters do.
    */
    if ( buf_stride == 0 ) {
      dst_cnt = dst_sz;
    } else {
      dst_cnt = buf_stride;
    }

    switch ( dst_sz ) {
    case 8:
      while ( nelements-- > 0 ) {
	miswap8 ( dst_ptr );
	dst_ptr += dst_cnt;
      }
      break;
    case 4:
      while ( nelements-- > 0 ) {
	miswap4 ( dst_ptr );
	dst_ptr += dst_cnt;
      }
      break;
    case 2:
      while ( nelements-- > 0 ) {
	miswap2 ( dst_ptr );
	dst_ptr += dst_cnt;
      }
      break;
    case 1:
      break;
    default:
      return (-1);
    }
    break;

  case H5T_CONV_FREE:
    break;

  default:
    return (-1);                /* Unknown command. */
  }
  return ( 0 );
}

/**
This function should be called when a labeled volume is created or opened
in order to facilitate conversions from the integer to the enumerated type.
*/
void miinit_enum ( hid_t type_id )
{
  H5Tregister ( H5T_PERS_SOFT, "i2e", H5T_NATIVE_INT, type_id,
                mi2_int_to_int );
  H5Tregister ( H5T_PERS_SOFT, "e2i", type_id, H5T_NATIVE_INT,
                mi2_int_to_int );
  H5Tregister ( H5T_PERS_SOFT, "d2e", H5T_NATIVE_DOUBLE, type_id,
                mi2_dbl_to_int );
  H5Tregister ( H5T_PERS_SOFT, "e2d", type_id, H5T_NATIVE_DOUBLE,
                mi2_int_to_dbl );
}

int minc_create_thumbnail ( mihandle_t volume, int grp )
{
  char path[MI2_MAX_PATH];
  hid_t grp_id;
  
  /* Don't handle negative or overly large numbers!
  */
  if ( grp <= 0 || grp > MI2_MAX_RESOLUTION_GROUP ) {
    return ( MI_ERROR );
  }

  sprintf ( path, MI_ROOT_PATH "/image/%d", grp );
  grp_id = H5Gcreate2 ( volume->hdf_id, path, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );

  if ( grp_id < 0 ) {
    return ( MI_ERROR );
  }

  H5Gclose ( grp_id );
  return ( MI_NOERROR );
}

/** Function to downsample a single slice of an image.
* \param in_ptr the 3D input slice, scale x isize[1] x isize[2]
* \param out_ptr the 2D output slice, osize[1] x osize[2]
*/
static void midownsample_slice ( double *in_ptr, double *out_ptr, hsize_t isize[],
                                 hsize_t osize[], int scale )
{
  hsize_t j, k;
  int x, y, z;
  double d;
  double total;

  total = (double) scale * (double) scale * (double) scale;

  /* These two loops iterate over all of the voxels in the 2D output
  * image.
  */
  for ( j = 0; j < osize[1]; j++ ) {
    for ( k = 0; k < osize[2]; k++ ) {
      /* The three inner loops iterate all scale^3
      * voxels in the input image which will be averaged
      * to form the output image.
      */
      d = 0;

      for ( x = 0; x < scale; x++ ) {
        for ( y = 0; y < scale; y++ ) {
          for ( z = 0; z < scale; z++ ) {
            size_t x1, y1, z1;
            double t;
            x1 = x;
            y1 = y + ( j * scale );
            z1 = z + ( k * scale );
            t = in_ptr[ ( ( x1 * isize[1] ) + y1 ) * isize[2] + z1];
            d += t;
          }
        }
      }

      d /= total;
      out_ptr[ ( j * osize[2] ) + k] = d;
    }
  }
}

/** Convert the hyperslab from real to voxel values, calculating and
* returning the minimum and maximum real values for the slab.  This
* could form the basis for a public function one day, but for now it
* is considered private.
*/
static void miconvert_hyperslab_to_voxel ( mihandle_t volume, hsize_t start[],
    hsize_t count[], double *slab_ptr,
    double *max_ptr, double *min_ptr )
{
  /* This code is not intended to be a general hyperslab-to-voxel
  * converter yet.  That is why it is not public.
  */
  double real_min, real_max;  /* Minimum and maximum values */
  hsize_t index;
  hsize_t total;
  double voxel_range, voxel_offset;
  double real_range, real_offset;
  int i;
  double tmp_val;

  real_min = DBL_MAX;
  real_max = -DBL_MAX;

  total = 1;

  for ( i = 0; i < volume->number_of_dims; i++ ) {
    total *= count[i];
  }

  /* Find the global minimum and maximum for this hyperslab.
  */
  for ( index = 0; index < total; index++ ) {
    tmp_val = slab_ptr[index];

    if ( tmp_val > real_max ) {
      real_max = tmp_val;
    }

    if ( tmp_val < real_min ) {
      real_min = tmp_val;
    }
  }

  voxel_range = volume->valid_max - volume->valid_min;
  voxel_offset = volume->valid_min;
  real_range = real_max - real_min;
  real_offset = real_min;

  for ( index = 0; index < total; index++ ) {
    tmp_val = slab_ptr[index];
    tmp_val = ( tmp_val - real_offset ) / real_range;
    tmp_val = ( tmp_val * voxel_range ) + voxel_offset;
    slab_ptr[index] = rint ( tmp_val );
  }

  if ( min_ptr != NULL ) {
    *min_ptr = real_min;
  }

  if ( max_ptr != NULL ) {
    *max_ptr = real_max;
  }
}

/** Convert the hyperslab from voxel to real values.  This could form
* the basis for a public function one day, but for now it is
* considered private.
*/
static void miconvert_hyperslab_to_real ( mihandle_t volume, hsize_t start[],
    hsize_t count[], double *slab_ptr )
{
  /* This code is not intended to be a general hyperslab-to-real
  * converter yet.  That is why it is not public.
  */
  double real_min, real_max;  /* Minimum and maximum values */
  hsize_t index;
  hsize_t total;
  double voxel_range, voxel_offset;
  double real_range, real_offset;
  int i;
  double tmp_val;
  misize_t pos[MI2_MAX_VAR_DIMS];
  int r;

  total = 1;

  for ( i = 0; i < volume->number_of_dims; i++ ) {
    total *= count[i];
    pos[i] = start[i];
  }

  voxel_offset = volume->valid_min;
  voxel_range = volume->valid_max - volume->valid_min;

  /* Get the initial real minimum & maximum.
  */
  r = miget_slice_range ( volume, pos, volume->number_of_dims,
                          &real_max, &real_min );

  if ( r == MI_ERROR ) {
    fprintf ( stderr, "Oops - failed to get slice range\n" );
  }

  real_offset = real_min;
  real_range = real_max - real_min;


  for ( index = 0; index < total; index++ ) {
    /* Since this calculation may cross slice boundaries, I need to
    * grab the correct real minimum and maximum for the coordinates
    * I happen to be in at the time.
    *
    * This next loop attempts to keep track of the current position,
    * and reloads the minimum and maximum whenever we change any other
    * than the fastest-varying dimension.
    */
    for ( i = volume->number_of_dims - 1; i >= 0; i-- ) {
      pos[i]++;

      if ( pos[i] >= count[i] ) {
        pos[i] = start[i];
        r = miget_slice_range ( volume, pos, volume->number_of_dims,
                                &real_max, &real_min );

        if ( r == MI_ERROR ) {
          fprintf ( stderr, "Oops - failed to get slice range\n" );
        }

        real_offset = real_min;
        real_range = real_max - real_min;
      } else {
        break;
      }
    }

    tmp_val = ( slab_ptr[index] - voxel_offset ) / voxel_range;
    slab_ptr[index] = ( tmp_val * real_range ) + real_offset;
  }
}

/** Update an individual thumbnail for the \a volume.  Updates group
* number \a ogrp from source group \a igrp.  The whole image tree must
* be rooted at \a loc_id.
*/
int
minc_update_thumbnail ( mihandle_t volume, hid_t loc_id, int igrp, int ogrp )
{
  hsize_t isize[MI2_MAX_VAR_DIMS];
  hsize_t osize[MI2_MAX_VAR_DIMS];
  hsize_t count[MI2_MAX_VAR_DIMS];
  hsize_t start[MI2_MAX_VAR_DIMS];
  hid_t idst_id=-1;              /* Input dataset */
  hid_t odst_id=-1;              /* Output dataset */
  hid_t ifspc_id=-1;             /* Input "file" dataspace */
  hid_t ofspc_id=-1;             /* Output "file" dataspace */
  hid_t typ_id=-1;               /* Type ID */
  hid_t imspc_id=-1;             /* Input memory dataspace */
  hid_t omspc_id=-1;             /* Output memory dataspace */
  char path[MI2_MAX_PATH];
  int ndims;                  /* Number of dimensions in the image */
  int scale;
  int i;                      /* Generic loop counter */
  double *in_ptr;
  double *out_ptr;
  hsize_t slice;
  size_t in_bytes;
  size_t out_bytes;
  double smax, smin;          /* Slice minimum and maximum */
  hid_t omax_id=-1;              /* Output image-max dataset */
  hid_t omin_id=-1;              /* Output image-min dataset */
  hid_t tfspc_id=-1;             /* Dimensionality of image-max/image-min */
  hid_t tmspc_id=-1;
  hid_t dcpl_id=-1;              /* Dataset creation property list */
  herr_t result=0;               /* return value for various functions*/

  miinit();

  /* Check arguments for basic validity. */
  if ( ogrp <= igrp ) {
    return ( MI_ERROR );
  }

  /* Calculate scale factor (always a power of 2) */
  for ( i = igrp, scale = 1; i < ogrp; i++, scale <<= 1 )
    ;

  /* Open the input path.
  */
  sprintf ( path, "%d/image", igrp );
  idst_id = H5Dopen1 ( loc_id, path );

  if ( idst_id < 0 ) {
    return ( MI_ERROR );
  }

  /* Get the input type.
  */
  typ_id = H5Dget_type ( idst_id );

  /* Get the input dataspace.
  */
  ifspc_id = H5Dget_space ( idst_id );

  /* Get the input dataset creation property list
  */
  dcpl_id = H5Dget_create_plist ( idst_id );

  ndims = H5Sget_simple_extent_ndims ( ifspc_id );
  H5Sget_simple_extent_dims ( ifspc_id, isize, NULL );

  /* Calculate the size of the new thumbnail.
  */
  for ( i = 0; i < ndims; i++ ) {
    osize[i] = isize[i] / scale;

    if ( osize[i] == 0 ) { /* Too small? */
      return ( MI_ERROR );
    }
  }

  /* Create dataspace for new resolution
  */
  ofspc_id = H5Screate_simple ( ndims, osize, NULL );

  sprintf ( path, "%d/image", ogrp );

  H5E_BEGIN_TRY {
    odst_id = H5Dcreate1 ( loc_id, path, typ_id, ofspc_id, H5P_DEFAULT );
  } H5E_END_TRY;

  if ( odst_id < 0 ) {
    odst_id = H5Dopen1 ( loc_id, path );

    if ( odst_id < 0 ) {
      return ( MI_ERROR );
    }
  }

  H5Pclose ( dcpl_id );       /* No longer needed. */

  if ( volume->volume_class == MI_CLASS_REAL ) {
    /* TODO: This is a bit of a hack - I need a better way to get the
    * dimensionality of the source image-min and image-max.
    */

    tfspc_id = H5Screate_simple ( 1, &osize[0], NULL );

    /* Create a simple scalar dataspace. */
    tmspc_id = H5Screate ( H5S_SCALAR );

    sprintf ( path, "%d/image-max", ogrp );
    H5E_BEGIN_TRY {
      omax_id = H5Dcreate1 ( loc_id, path, H5T_IEEE_F64LE, tfspc_id,
      H5P_DEFAULT );
    } H5E_END_TRY;

    if ( omax_id < 0 ) {
      omax_id = H5Dopen1 ( loc_id, path );
    }

    sprintf ( path, "%d/image-min", ogrp );
    H5E_BEGIN_TRY {
      omin_id = H5Dcreate1 ( loc_id, path, H5T_IEEE_F64LE, tfspc_id,
      H5P_DEFAULT );
    } H5E_END_TRY;

    if ( omin_id < 0 ) {
      omin_id = H5Dopen1 ( loc_id, path );
    }
  }

  /* Calculate the input buffer size - scale slices.
  */
  in_bytes = scale * isize[1] * isize[2] * sizeof ( double );
  in_ptr = malloc ( in_bytes );

  out_bytes = osize[1] * osize[2] * sizeof ( double );
  out_ptr = malloc ( out_bytes );

  count[0] = scale;
  count[1] = isize[1];
  count[2] = isize[2];
  imspc_id = H5Screate_simple ( ndims, count, NULL );

  count[0] = 1;
  count[1] = osize[1];
  count[2] = osize[2];
  omspc_id = H5Screate_simple ( ndims, count, NULL );

  /*
  * read image & TODO: convert to "real" range.
  */
  for ( slice = 0; slice < osize[0]; slice++ ) {

    start[0] = slice * scale;
    start[1] = 0;
    start[2] = 0;
    count[0] = scale;
    count[1] = isize[1];
    count[2] = isize[2];

    MI_CHECK_HDF_CALL(result=H5Sselect_hyperslab ( ifspc_id, H5S_SELECT_SET, start, NULL, count,
                          NULL ),"H5Sselect_hyperslab");
    if(result>=0)
    {
      MI_CHECK_HDF_CALL(H5Dread ( idst_id, H5T_NATIVE_DOUBLE, imspc_id, ifspc_id, H5P_DEFAULT,
                in_ptr ),"H5Dread");

      /* Scale slice from voxel to real values. */

      miconvert_hyperslab_to_real ( volume, start, count, in_ptr );

      midownsample_slice ( in_ptr, out_ptr, isize, osize, scale );
    }
    
    start[0] = slice;
    start[1] = 0;
    start[2] = 0;
    count[0] = 1;
    count[1] = osize[1];
    count[2] = osize[2];
    
    MI_CHECK_HDF_CALL(result=H5Sselect_hyperslab ( ofspc_id, H5S_SELECT_SET, start, NULL, count,
                                           NULL ),"H5Sselect_hyperslab");
    if(result>=0)
    {
      miconvert_hyperslab_to_voxel ( volume, start, count, out_ptr,
                                     &smax, &smin );

      MI_CHECK_HDF_CALL(H5Dwrite ( odst_id, H5T_NATIVE_DOUBLE, omspc_id, ofspc_id, H5P_DEFAULT,
                                  out_ptr ),"H5Dwrite");
    }
    
    if ( volume->volume_class == MI_CLASS_REAL ) {
      /* Select the right point in tfspc_id */
      H5Sselect_elements ( tfspc_id, H5S_SELECT_SET, 1, &start[0] );

      H5Dwrite ( omax_id, H5T_NATIVE_DOUBLE, tmspc_id, tfspc_id,
                 H5P_DEFAULT, &smax );

      H5Dwrite ( omin_id, H5T_NATIVE_DOUBLE, tmspc_id, tfspc_id,
                 H5P_DEFAULT, &smin );
    }
  }

  free ( in_ptr );
  free ( out_ptr );

  if(omax_id>0)  H5Dclose ( omax_id );
  if(omin_id>0)  H5Dclose ( omin_id );
  if(tfspc_id>0) H5Sclose ( tfspc_id );
  if(tmspc_id>0) H5Sclose ( tmspc_id );

  if(omspc_id>0) H5Sclose ( omspc_id );
  if(imspc_id>0) H5Sclose ( imspc_id );
  if(odst_id>0)  H5Dclose ( odst_id );
  if(typ_id>0)   H5Tclose ( typ_id );
  H5Sclose ( ofspc_id );
  H5Sclose ( ifspc_id );

  return ( MI_NOERROR );
}

/** Cycle through and update each of the lower-resolution images in
* the file.
*/
int
minc_update_thumbnails ( mihandle_t volume )
{
  int grp_no, prv_grp_no;
  hid_t grp_id;
  hsize_t n;
  hsize_t i;
  char name[MI2_MAX_PATH];

  grp_id = H5Gopen1 ( volume->hdf_id, MI_ROOT_PATH "/image" );

  if ( grp_id < 0 ) {
    return ( MI_ERROR );    /* Error opening group. */
  }

  if ( H5Gget_num_objs ( grp_id, &n ) < 0 ) {
    return ( MI_ERROR );    /* Error getting object count. */
  }

  grp_no = -1;

  for ( i = 0; i < n; i++ ) {
    if ( H5Gget_objname_by_idx ( grp_id, i, name, MI2_MAX_PATH ) < 0 ) {
      return ( MI_ERROR );
    }

    prv_grp_no = grp_no;
    grp_no = atoi ( name );

    if ( grp_no != 0 ) {
      minc_update_thumbnail ( volume, grp_id, prv_grp_no, grp_no );
    }
  }

  H5Gclose ( grp_id );
  return ( MI_NOERROR );

}

double *
alloc1d ( int n )
{
  return ( ( double * ) malloc ( sizeof ( double ) * n ) );
}

double **
alloc2d ( int n, int m )
{
  double **mat;
  int i;

  mat = ( double ** ) malloc ( n * sizeof ( double * ) );

  if ( mat == NULL ) {
    return NULL;
  }

  for ( i = 0; i < n; i++ ) {
    mat[i] = ( double * ) malloc ( m * sizeof ( double ) );

    if ( mat[i] == NULL ) {
      free(mat);
      return NULL;
    }
  }

  return ( mat );
}

void
free2d ( int n, double **mat )
{
  int i;

  for ( i = 0; i < n; i++ ) {
    free ( mat[i] );
  }

  free ( mat );
}

/** Common code to create either standard or non-standard MINC datasets
 * as required. Used by create_dataset and create_standard_dataset.
 * Note that in the normal course of operations, it is possible for 
 * the dataset creation to fail (perhaps it already exists). As a 
 * result we can't take the return value too seriously.
 * \param hdf_file An open HDF5 file handle.
 * \param name The dataset (variable) name to create.
 * \param is_std Non-zero if this is a MINC-standard dataset, to be decorated
 * with the full set of standard MINC attributes.
 */
static int create_new_dataset(hid_t hdf_file, const char *name, int is_std)
{
  hid_t dataset_info;
  hid_t dataspace_info;
  hid_t grp_info;
  int result;

  grp_info = H5Gopen1 ( hdf_file, MI_ROOT_PATH "/" MI_INFO_NAME );

  if ( grp_info < 0 ) {
    return ( MI_ERROR );
  }

  dataspace_info = H5Screate ( H5S_SCALAR );

  if ( dataspace_info < 0 ) {
    H5Gclose(grp_info);
    return ( MI_ERROR );
  }

  dataset_info = H5Dcreate1 ( grp_info, name,
                              H5T_STD_I32LE, dataspace_info, H5P_DEFAULT );

  if ( dataset_info < 0 ) {
    H5Sclose(dataspace_info);
    H5Gclose(grp_info);
    return ( MI_ERROR );
  }

  if (is_std) {
    result = add_standard_minc_attributes(hdf_file,dataset_info);
  }
  else {
    result = add_minimal_minc_attributes(hdf_file,dataset_info);
  }

  H5Dclose ( dataset_info );
  H5Sclose ( dataspace_info );
  H5Gclose ( grp_info );

  return ( result );
}

/** Function to create a NON-STANDARD dataset (e.g. other than 
 * "acquisition", "patient", or "study"). For internal use only.
 */

int
create_dataset ( hid_t hdf_file, const char *name )
{
  return create_new_dataset(hdf_file, name, FALSE);
}

/** Function to create a standard MINC dataset (acquisition, patient, study).
 * For internal use only.
 */
int
create_standard_dataset ( hid_t hdf_file, const char *name )
{
  return create_new_dataset(hdf_file, name, TRUE);
}

int 
add_minimal_minc_attributes(hid_t hdf_file, hid_t dset_id)
{
  int result;
  
  result = miset_attr_at_loc ( dset_id, MIvartype, MI_TYPE_STRING, strlen ( MI_GROUP ), MI_GROUP );

  if ( result < 0 ) {
    return ( MI_ERROR );
  }
  
  return result;
}

int 
add_standard_minc_attributes(hid_t hdf_file, hid_t dset_id)
{
  int result;
  result = miset_attr_at_loc ( dset_id, MIvarid, MI_TYPE_STRING, strlen ( MI_STDVAR ), MI_STDVAR );

  if ( result < 0 ) {
    return ( MI_ERROR );
  }

  result = miset_attr_at_loc ( dset_id, MIvartype, MI_TYPE_STRING, strlen ( MI_GROUP ), MI_GROUP );

  if ( result < 0 ) {
    return ( MI_ERROR );
  }

  result = miset_attr_at_loc ( dset_id, MIversion, MI_TYPE_STRING, strlen ( MI_VERSION_2_0 ), MI_VERSION_2_0 );

  if ( result < 0 ) {
    return ( MI_ERROR );
  }

  return result;
}



/** Performs scaled maximal pivoting gaussian elimination as a
    numerically robust method to solve systems of linear equations.
*/
int
scaled_maximal_pivoting_gaussian_elimination ( int   n,
    int   row[],
    double **a,
    int   n_values,
    double **solution )
{
  int i, j, k, p, v, tmp;
  double *s, val, best_val, m, scale_factor;
  int success;

  s = alloc1d ( n );

  for ( i = 0; i < n; i++ )
    row[i] = i;

  for ( i = 0; i < n; i++ ) {
    s[i] = fabs ( a[i][0] );

    for ( j = 1; j < n; j++ ) {
      if ( fabs ( a[i][j] ) > s[i] )
        s[i] = fabs ( a[i][j] );
    }

    if ( s[i] == 0.0 ) {
      free ( s );

      return ( FALSE );
    }
  }

  success = TRUE;

  for ( i = 0; i < n - 1; i++ ) {
    p = i;
    best_val = a[row[i]][i] / s[row[i]];
    best_val = fabs ( best_val );

    for ( j = i + 1; j < n; j++ ) {
      val = a[row[j]][i] / s[row[j]];
      val = fabs ( val );

      if ( val > best_val ) {
        best_val = val;
        p = j;
      }
    }

    if ( a[row[p]][i] == 0.0 ) {
      success = FALSE;
      break;
    }

    if ( i != p ) {
      tmp = row[i];
      row[i] = row[p];
      row[p] = tmp;
    }

    for ( j = i + 1; j < n; j++ ) {
      if ( a[row[i]][i] == 0.0 ) {
        success = FALSE;
        break;
      }

      m = a[row[j]][i] / a[row[i]][i];

      for ( k = i + 1; k < n; k++ )
        a[row[j]][k] -= m * a[row[i]][k];

      for ( v = 0; v < n_values; v++ )
        solution[row[j]][v] -= m * solution[row[i]][v];
    }

    if ( !success )
      break;
  }

  if ( success && a[row[n - 1]][n - 1] == 0.0 )
    success = FALSE;

  if ( success ) {
    for ( i = n - 1;  i >= 0;  --i ) {
      for ( j = i + 1; j < n; j++ ) {
        scale_factor = a[row[i]][j];

        for ( v = 0; v < n_values; v++ )
          solution[row[i]][v] -= scale_factor * solution[row[j]][v];
      }

      for ( v = 0; v < n_values; v++ )
        solution[row[i]][v] /= a[row[i]][i];
    }
  }

  free ( s );

  return ( success );
}

int
scaled_maximal_pivoting_gaussian_elimination_real ( int n,
    double **coefs,
    int n_values,
    double **values )
{
  int i, j, v, *row;
  double **a, **solution;
  int success;

  row = ( int * ) alloc1d ( n ); /* Ugly and wasteful but OK for 1D array */
  a = alloc2d ( n, n );
  solution = alloc2d ( n, n_values );

  for ( i = 0; i < n; i++ ) {
    for ( j = 0; j < n; j++ )
      a[i][j] = coefs[i][j];

    for ( v = 0; v < n_values; v++ )
      solution[i][v] = values[v][i];
  }

  success = scaled_maximal_pivoting_gaussian_elimination ( n, row, a,
            n_values,
            solution );

  if ( success ) {
    for ( i = 0; i < n; i++ ) {
      for ( v = 0; v < n_values; v++ ) {
        values[v][i] = solution[row[i]][v];
      }
    }
  }

  free2d ( n, a );
  free2d ( n, solution );
  free ( row );

  return ( success );
}

/** Computes the inverse of a square matrix.
*/
static int
invert_4x4_matrix ( double matrix[4][4], /**< Input matrix */
                    double inverse[4][4] ) /**< Output (inverted) matrix */
{
  int result;
  int i, j;
  double **mtmp;
  double **itmp;

  mtmp = alloc2d ( 4, 4 );
  itmp = alloc2d ( 4, 4 );

  /* Start off with the identity matrix. */
  for ( i = 0; i < 4; i++ ) {
    for ( j = 0; j < 4; j++ ) {
      itmp[i][j] = 0.0;
      mtmp[i][j] = matrix[i][j];
    }

    itmp[i][i] = 1.0;
  }

  result = scaled_maximal_pivoting_gaussian_elimination_real ( 4, mtmp,
           4, itmp );

  if ( result )  {
    for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 4; j++ ) {
        inverse[i][j] = itmp[j][i];
      }
    }
  }

  free2d ( 4, mtmp );
  free2d ( 4, itmp );

  return ( result ? MI_NOERROR : MI_ERROR );
}

/** Fills in the transform with the identity matrix.
*/
static void
mimake_identity_transform ( mi_lin_xfm_t transform )
{
  int i, j;

  for ( i = 0; i < MI2_LIN_XFM_SIZE; i++ ) {
    for ( j = 0; j < MI2_LIN_XFM_SIZE; j++ ) {
      transform[i][j] = 0.0;
    }

    transform[i][i] = 1.0;
  }
}

/** Function for inverting a MINC linear transform.
*/
int
miinvert_transform ( mi_lin_xfm_t transform, mi_lin_xfm_t inverse )
{
  int result;

  result = invert_4x4_matrix ( transform, inverse );

  if ( result != MI_NOERROR ) {
    mimake_identity_transform ( inverse );
  }

  return ( result );
}

/** Function to read a scalar variable of HDF5 type \a type_id from the given
*  \a path relative to the HDF5 file or group \a loc_id.
*/
int
miget_scalar ( hid_t loc_id, hid_t type_id, const char *path, void *data )
{
  hid_t dset_id;
  hid_t spc_id;
  int result = MI_ERROR;

  H5E_BEGIN_TRY {
    dset_id = H5Dopen1 ( loc_id, path );
  } H5E_END_TRY;

  if ( dset_id >= 0 ) {
    spc_id = H5Dget_space ( dset_id );

    if ( spc_id >= 0 ) {
      if ( H5Sget_simple_extent_ndims ( spc_id ) == 0 ) {
        if ( H5Dread ( dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                       data ) >= 0 ) {
          result = MI_NOERROR;
        }
      }

      H5Sclose ( spc_id );
    }

    H5Dclose ( dset_id );
  }

  return ( result );
}
/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
