/** \file volume.c
* \brief MINC 2.0 Volume Functions
* \author Leila Baghdadi, Bert Vincent
*
* Functions to create, open, and close MINC volume objects.
************************************************************************/
#include <stdlib.h>
#include <hdf5.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif //HAVE_SYS_TYPES_H

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif //HAVE_UNISTD_H

#ifdef HAVE_MINC1
#include "minc.h"
#endif //HAVE_MINC1

#include <limits.h>
#include <float.h>
#include <time.h>
#include <math.h>

#include "minc_config.h"
#include "minc2.h"
#include "minc2_private.h"

/* So we build with 1.8.4 */  
#ifndef H5F_LIBVER_18
#define H5F_LIBVER_18 H5F_LIBVER_LATEST
#endif

/*Used to optimize chunking size for faster MINC1 API access*/
#define _MI1_MAX_VAR_BUFFER_SIZE 1000000


/**
* \defgroup mi2Vol MINC 2.0 Volume Functions
*/

/* Forward declarations */

static void miread_valid_range(mihandle_t volume, double *valid_max,
                               double *valid_min);

static int _miset_volume_class(mihandle_t volume, miclass_t volclass);
static int _miget_volume_class(mihandle_t volume, miclass_t *volclass);

/**
 * Creates a (hopefully) unique identifier to associate with a
 *              MINC file, by concatenating various information about the
 *              system, process, etc.
 * returns the length of identifier 
 */
static int _generate_ident( char * id_str, size_t length )
{
  static int identx = 1;      /* Static ID counter */
  time_t now;
  struct tm tm_buf;
  char host_str[128];
  char user_str[128];
  char *temp_ptr;
  char time_str[26];
  int result;
  
// Linking in ws2_32  for gethostname is problematic with static libraries.
#ifdef _WIN32
  strcpy(host_str, "unknown");
#else
  if (gethostname(host_str, sizeof(host_str)) != 0) {
    strcpy(host_str, "unknown");
  }
#endif
  
  temp_ptr = getenv("LOGNAME");
  if (temp_ptr != NULL) {
    strncpy(user_str, temp_ptr, sizeof(user_str) - 1);
  }
  else {
    strcpy(user_str, "nobody");
  }
  
  
  time(&now);
#ifdef _WIN32
  memcpy(&tm_buf, localtime(&now), sizeof(tm_buf));
#else
  localtime_r(&now, &tm_buf);
#endif
  strftime(time_str, sizeof(time_str), "%Y.%m.%d.%H.%M.%S", &tm_buf);
  
  result = snprintf(id_str, length, "%s:%s:%s:%u:%u", 
                    user_str, 
                    host_str, 
                    time_str, 
                    getpid(), 
                    identx++);
  return result;
}

/**
 * open HDF5 file 
 */
static hid_t _hdf_open(const char *path, int mode)
{
  hid_t fd;
  hid_t prp_id;
/*  hid_t grp_id;
  hid_t dset_id;
  int ndims;*/
  
  prp_id = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_cache(prp_id, 0, 2503, miget_cfg_present(MICFG_MINC_FILE_CACHE)?miget_cfg_int(MICFG_MINC_FILE_CACHE)*100000:_MI1_MAX_VAR_BUFFER_SIZE*10, 1.0);
  
  H5E_BEGIN_TRY {
#ifdef HDF5_MMAP_TEST
    if (mode & 0x8000) {
      
      H5Pset_fapl_mmap(prp_id, 8192, 1);
      fd = H5Fopen(path, mode & 0x7FFF, prp_id);
    } else {
      fd = H5Fopen(path, mode, prp_id);
    }
#else
    fd = H5Fopen(path, mode, prp_id);
#endif
  } H5E_END_TRY;
  
  H5Pclose(prp_id);
  
  
  /* Open the image variables.
   */
  //TODO: convert
  
//   H5E_BEGIN_TRY 
//   {
//     dset_id = H5Dopen1(fd, "/minc-2.0/image/0/image");
//     if (dset_id >= 0) {
//       hid_t type_id;
//       int is_compound = 0;
//       
//       hdf_get_diminfo(dset_id, &ndims, dims);
//       
//       #ifndef NO_EMULATE_VECTOR_DIMENSION
//       /* See if a vector_dimension needs to be emulated.
//        */
//       type_id = H5Dget_type(dset_id);
//       if (type_id >= 0) {
//         if (H5Tget_class(type_id) == H5T_COMPOUND) {
//           /* OK, it's compound type. */
//           struct m2_dim *dim = hdf_dim_add(file, MIvector_dimension, 
//                                            H5Tget_nmembers(type_id));
//           dim->is_fake = 1;
//           dims[ndims++] = H5Tget_nmembers(type_id);
//           is_compound = 1;
//         }
//         H5Tclose(type_id);
//       }
//       #endif /* NO_EMULATE_VECTOR_DIMENSION */
//       
//       var = hdf_var_add(file, MIimage, "/minc-2.0/image/0/image", 
//                         ndims, dims);
//       var->is_cmpd = is_compound;
//       
//       H5Dclose(dset_id);
//     }
//     
//     dset_id = H5Dopen1(fd, "/minc-2.0/image/0/image-min");
//     if (dset_id >= 0) {
//       hdf_get_diminfo(dset_id, &ndims, dims);
//       hdf_var_add(file, MIimagemin, "/minc-2.0/image/0/image-min", 
//                   ndims, dims);
//       H5Dclose(dset_id);
//     }
//     
//     dset_id = H5Dopen1(fd, "/minc-2.0/image/0/image-max");
//     if (dset_id >= 0) {
//       hdf_get_diminfo(dset_id, &ndims, dims);
//       hdf_var_add(file, MIimagemax, "/minc-2.0/image/0/image-max", 
//                   ndims, dims);
//       H5Dclose(dset_id);
//     }
//   } H5E_END_TRY;
//   
//   /* Open all of the datasets in the "dimensions" category.
//    */
//   grp_id = H5Gopen2(fd, "/minc-2.0/dimensions", H5P_DEFAULT);
//   hdf_open_dsets(file, grp_id, "/minc-2.0/dimensions/", 1);
//   H5Gclose(grp_id);
//   
//   /* Open all of the datasets in the "info" category.
//    */
//   grp_id = H5Gopen2(fd, "/minc-2.0/info", H5P_DEFAULT);
//   hdf_open_dsets(file, grp_id, "/minc-2.0/info/", 0);
//   H5Gclose(grp_id);
  return fd;
}


/** 
 * Create an HDF5 file. 
 */
static hid_t _hdf_create(const char *path, int cmode)
{
  hid_t grp_id;
  hid_t fd;
  hid_t tmp_id;
  hid_t hdf_gpid;
  hid_t fpid;
  
  fpid = H5Pcreate (H5P_FILE_ACCESS);

  /*VF use all the features of new HDF5 1.8*/
  H5Pset_libver_bounds (fpid, H5F_LIBVER_18, H5F_LIBVER_18);
  
  H5Pset_cache(fpid, 0, 2503, miget_cfg_present(MICFG_MINC_FILE_CACHE)?miget_cfg_int(MICFG_MINC_FILE_CACHE)*100000:_MI1_MAX_VAR_BUFFER_SIZE*100, 1.0);
  
  H5E_BEGIN_TRY {
    fd = H5Fcreate(path, cmode, H5P_DEFAULT, fpid);
  } H5E_END_TRY;
  
  if (fd < 0) {
    /*TODO: report error properly*/
    return MI_LOG_ERROR(MI2_MSG_CREATEFILE,path);
  }
  
  /* Create the default groups.
   * Should we use a non-zero value for size_hint (parameter 3)???
   */

  hdf_gpid = H5Pcreate (H5P_GROUP_CREATE);
  H5Pset_attr_phase_change (hdf_gpid, 0, 0);
  
  MI_CHECK_HDF_CALL_RET(grp_id = H5Gcreate2(fd, MI_ROOT_PATH , H5P_DEFAULT, hdf_gpid, H5P_DEFAULT),"H5Gcreate2")

  MI_CHECK_HDF_CALL_RET(tmp_id = H5Gcreate2(grp_id, "dimensions", H5P_DEFAULT, hdf_gpid, H5P_DEFAULT),"H5Gcreate2")
  H5Gclose(tmp_id);
  
  MI_CHECK_HDF_CALL_RET(tmp_id = H5Gcreate2(grp_id, "info", H5P_DEFAULT, hdf_gpid, H5P_DEFAULT),"H5Gcreate2")
  H5Gclose(tmp_id);
  
  MI_CHECK_HDF_CALL_RET(tmp_id = H5Gcreate2(grp_id, "image", H5P_DEFAULT, hdf_gpid, H5P_DEFAULT),"H5Gcreate2")
  
  H5Gclose(tmp_id);
  MI_CHECK_HDF_CALL_RET(tmp_id = H5Gcreate2(grp_id, "image/0", H5P_DEFAULT, hdf_gpid, H5P_DEFAULT),"H5Gcreate2")
  
  H5Pclose ( hdf_gpid );
  H5Gclose(tmp_id);
  H5Gclose(grp_id);
  
  return fd;
}

static int _hdf_close(hid_t fd)
{
  //TODO: make sure we save all that is needed
  
  MI_CHECK_HDF_CALL_RET(H5Fclose(fd),"H5Fclose")
  return MI_NOERROR;
}


/** Create the actual image for the volume.
  * Note that the image dataset muct be created in the hierarchy
  * before the image data can be added.
  * \ingroup mi2Vol
*/
int micreate_volume_image(mihandle_t volume)
{
  char dimorder[MI2_CHAR_LENGTH];
  int i;
  int dimorder_len=0;
  hid_t dataspace_id;
  hid_t dset_id;
  hsize_t hdf_size[MI2_MAX_VAR_DIMS];

  /* Try creating IMAGE dataset i.e. /minc-2.0/image/0/image
  */

  dimorder[0] = '\0'; /* Set string to empty */

  for (i = 0; i < volume->number_of_dims; i++) {
    hdf_size[i] = volume->dim_handles[i]->length;

    /* Create the dimorder string, ordered comma-separated
      list of dimension names.
    */
    strncat(dimorder, volume->dim_handles[i]->name, MI2_CHAR_LENGTH - 1 - strlen(dimorder)); /*as a replacement for strlcat*/
    if (i != volume->number_of_dims - 1) {
      strncat(dimorder, ",", MI2_CHAR_LENGTH - 1);
    }
  }


  /* Create a SIMPLE dataspace  */
  dataspace_id = H5Screate_simple(volume->number_of_dims, hdf_size, NULL);
  if (dataspace_id < 0) {
    return MI_ERROR;
  }

  MI_CHECK_HDF_CALL_RET(dset_id = H5Dcreate2(volume->hdf_id, MI_ROOT_PATH "/image/0/image",
                                             volume->ftype_id,
                                             dataspace_id, H5P_DEFAULT,
                                             volume->plist_id,H5P_DEFAULT),"H5Dcreate2")

  volume->image_id = dset_id;

  add_standard_minc_attributes(volume->hdf_id,volume->image_id);
  
  /* Create the dimorder attribute, ordered comma-separated
    list of dimension names.
  */
  miset_attr_at_loc(dset_id, "dimorder", MI_TYPE_STRING,
                    strlen(dimorder), dimorder);

  H5Sclose(dataspace_id);

  if (volume->volume_class == MI_CLASS_REAL) {
    int ndims;
    hid_t dcpl_id;
    double dtmp;

    MI_CHECK_HDF_CALL_RET(dcpl_id = H5Pcreate(H5P_DATASET_CREATE),"H5Pcreate")

    if (volume->has_slice_scaling && (volume->number_of_dims > 2) ) {
      /* TODO: Find the slowest-varying spatial dimension; that forms
      * the basis for the image-min and image-max variables.  Right
      * now this is an oversimplification!
      */
      ndims = volume->number_of_dims - 2;
      MI_CHECK_HDF_CALL_RET(dataspace_id = H5Screate_simple(ndims, hdf_size, NULL),"H5Screate_simple")
    } else {
      ndims = 0;
      MI_CHECK_HDF_CALL_RET(dataspace_id = H5Screate(H5S_SCALAR),"H5Screate")
    }

    if (ndims != 0) {
      dimorder[0] = '\0'; /* Set string to empty */
      for (i = 0; i < ndims; i++) {
        /* Create the dimorder string, ordered comma-separated
          list of dimension names.
        */
        strncat(dimorder, volume->dim_handles[i]->name, MI2_CHAR_LENGTH - 1 - strlen(dimorder));
        if (i != ndims - 1) {
          strncat(dimorder, ",", MI2_CHAR_LENGTH - 1 - strlen(dimorder));
        }
      }
    }

    /* Create the image minimum dataset for FULL-RESOLUTION storage of data
    */
    dtmp = 0.0;
    H5Pset_fill_value(dcpl_id, H5T_NATIVE_DOUBLE, &dtmp);

    MI_CHECK_HDF_CALL_RET(dset_id = H5Dcreate2(volume->hdf_id, MI_ROOT_PATH "/image/0/image-min",
                         H5T_IEEE_F64LE, dataspace_id, H5P_DEFAULT,dcpl_id,H5P_DEFAULT),"H5Dcreate2")
    if (ndims != 0) {
      miset_attr_at_loc(dset_id, "dimorder", MI_TYPE_STRING,
                        strlen(dimorder), dimorder);
    }
    volume->imin_id = dset_id;
    add_standard_minc_attributes(volume->hdf_id,volume->imin_id);

    /* Create the image maximum dataset for FULL-RESOLUTION storage of data
    */
    dtmp = 1.0;
    H5Pset_fill_value(dcpl_id, H5T_NATIVE_DOUBLE, &dtmp);

    MI_CHECK_HDF_CALL_RET(dset_id = H5Dcreate2(volume->hdf_id, MI_ROOT_PATH "/image/0/image-max",
                         H5T_IEEE_F64LE, dataspace_id,H5P_DEFAULT, dcpl_id, H5P_DEFAULT),"H5Dcreate2")
    if (ndims != 0) {
      miset_attr_at_loc(dset_id, "dimorder", MI_TYPE_STRING,
                        strlen(dimorder), dimorder);
    }
    volume->imax_id = dset_id;
    add_standard_minc_attributes(volume->hdf_id,volume->imax_id);
    H5Sclose(dataspace_id);
    H5Pclose(dcpl_id);
  }

  return (MI_NOERROR);
}

/** Set up the array of conversions from voxel to world coordinate order.
*/
static int miset_volume_world_indices(mihandle_t hvol)
{
  int i;

  for (i = 0; i < hvol->number_of_dims; i++) {
    midimhandle_t hdim = hvol->dim_handles[i];

    hdim->world_index = -1;
    if (hdim->dim_class == MI_DIMCLASS_SPATIAL) {
      if (!strcmp(hdim->name, MIxspace)) {
        hdim->world_index = MI2_X;
      } else if (!strcmp(hdim->name, MIyspace)) {
        hdim->world_index = MI2_Y;
      } else if (!strcmp(hdim->name, MIzspace)) {
        hdim->world_index = MI2_Z;
      }
    } else if (hdim->dim_class == MI_DIMCLASS_SFREQUENCY) {
      if (!strcmp(hdim->name, MIxfrequency)) {
        hdim->world_index = MI2_X;
      } else if (!strcmp(hdim->name, MIyfrequency)) {
        hdim->world_index = MI2_Y;
      } else if (!strcmp(hdim->name, MIzfrequency)) {
        hdim->world_index = MI2_Z;
      }
    }
  }
  return (MI_NOERROR);
}

/** Create and initialize a MINC 2.0 volume structure.
*/
static mihandle_t mialloc_volume_handle(void)
{
  mihandle_t handle = (mihandle_t) malloc(sizeof(struct mivolume));

  if (handle != NULL) {
    /* Clear the memory by default. */
    memset(handle, 0, sizeof(struct mivolume));

    /* Set the defaults for the data structure */
    handle->scale_min = 0.0;
    handle->scale_max = 1.0;
    handle->image_id = -1;
    handle->imax_id = -1;
    handle->imin_id = -1;
    handle->plist_id = -1;
    handle->has_slice_scaling = FALSE;
    handle->is_dirty = FALSE;
    handle->dim_indices = NULL;
    handle->selected_resolution = 0;
  }
  return (handle);
}

/** Create a volume with the specified name, dimensions,
    type, class, volume properties and retrieve the volume handle.
    \ingroup mi2Vol
*/
int micreate_volume(const char *filename, int number_of_dimensions,
                midimhandle_t dimensions[], mitype_t volume_type,
                miclass_t volume_class, mivolumeprops_t create_props,
                mihandle_t *volume)
{
  int i;
  int stat;
  hid_t file_id;
  hid_t hdf_type;
  hid_t hdf_plist;
  hid_t fspc_id;
  hsize_t dim[1];
  hid_t grp_id;
  herr_t status;
  hid_t dataset_id = -1;
  hid_t dataset_width = -1;
  hid_t dataspace_id = -1;
  char *name;
  size_t size;
  hsize_t hdf_size[MI2_MAX_VAR_DIMS];
  mihandle_t handle;
  mivolumeprops_t props_handle;
  char ident_str[128];
  hid_t tmp_type;
  int   dimension_is_vector = 0;

  /* Initialization.
    For the actual body of this function look at m2utils.c
  */
  miinit();

  /* Validate the parameters.
  */
  if (filename == NULL) {
    return MI_LOG_ERROR(MI2_MSG_CREATEFILE," (NULL) ");
  }

  if (dimensions == NULL && number_of_dimensions != 0) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC," Can't create volume with undefined dimensions");
  }

  /* Allocate space for the volume handle
  */
  handle = mialloc_volume_handle();
  if (handle == NULL) {
    return MI_LOG_ERROR(MI2_MSG_OUTOFMEM,sizeof(struct mivolume));
  }

  /* Initialize some of the variables associated with the volume handle.
  */
  handle->mode = MI2_OPEN_RDWR;
  handle->number_of_dims = number_of_dimensions;

  /* convert minc type to hdf type
  */
  hdf_type = mitype_to_hdftype(volume_type, FALSE);

  /* Setting up volume type_id
  */
  switch (volume_class) {
  case MI_CLASS_REAL:
  case MI_CLASS_INT:
    handle->ftype_id = hdf_type;
    handle->mtype_id = H5Tget_native_type(handle->ftype_id,
                                          H5T_DIR_ASCEND);
    break;

  case MI_CLASS_LABEL:
    /* A volume of class LABEL must have an integer type (positive).
    */
    switch (volume_type) {
    case MI_TYPE_UBYTE:
    case MI_TYPE_USHORT:
    case MI_TYPE_UINT:
    case MI_TYPE_BYTE:
    case MI_TYPE_SHORT:
    case MI_TYPE_INT:
      MI_CHECK_HDF_CALL_RET(handle->ftype_id = H5Tenum_create(hdf_type),"H5Tenum_create")

      tmp_type = H5Tget_native_type(hdf_type, H5T_DIR_ASCEND);
      H5Tclose(hdf_type);
      hdf_type = tmp_type;

      /* Create an enumerated type with the native type as it's base.
      */
      MI_CHECK_HDF_CALL_RET(handle->mtype_id = H5Tenum_create(hdf_type),"H5Tenum_create")
      
      H5Tclose(hdf_type);

      miinit_enum(handle->ftype_id);
      miinit_enum(handle->mtype_id);
      break;
    default:
      free(handle);
      return (MI_ERROR);
    }
    break;

  case MI_CLASS_COMPLEX:
    switch (volume_type) {
    case MI_TYPE_SCOMPLEX:
    case MI_TYPE_ICOMPLEX:
    case MI_TYPE_FCOMPLEX:
    case MI_TYPE_DCOMPLEX:
      handle->ftype_id = hdf_type;
      handle->mtype_id = mitype_to_hdftype(volume_type, TRUE);
      break;
    default:
      free(handle);
      return MI_LOG_ERROR(MI2_MSG_BADTYPE,volume_type);
    }
    break;

  case MI_CLASS_UNIFORM_RECORD:
    MI_CHECK_HDF_CALL_RET(handle->ftype_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(hdf_type)),"H5Tcreate")
    MI_CHECK_HDF_CALL_RET(handle->mtype_id = H5Tcreate(H5T_COMPOUND, H5Tget_size(hdf_type)),"H5Tcreate")
    H5Tclose(hdf_type);
    break;

  default:
    free(handle);
    return (MI_ERROR);
  }

  handle->volume_class = volume_class;

  /* Create file in HDF5 with the given filename and
    H5F_ACC_TRUNC: Truncate file, if it already exists,
                  erasing all data previously stored in the file.
    and create ID and ID access as default.
  */

  file_id = _hdf_create(filename, H5F_ACC_TRUNC);
  if (file_id < 0) {
    free(handle);
    return (MI_ERROR);
  }

  handle->hdf_id = file_id;

  _generate_ident(ident_str, sizeof(ident_str));
  miset_attribute(handle, MI_ROOT_PATH, "ident", MI_TYPE_STRING,
                  strlen(ident_str), ident_str);
  miset_attribute(handle, MI_ROOT_PATH, "minc_version", MI_TYPE_STRING,
                  strlen(MINC_VERSION), MINC_VERSION);

  _miset_volume_class(handle, handle->volume_class);

  /* Create a new property list for the volume
  */
  MI_CHECK_HDF_CALL_RET(hdf_plist = H5Pcreate(H5P_DATASET_CREATE),"H5Pcreate")

  handle->plist_id = hdf_plist;

  /* Set fill value to guarantee valid data on incomplete datasets.
  */
  if (volume_class != MI_CLASS_LABEL &&
      volume_class != MI_CLASS_UNIFORM_RECORD) {
    size_t siz = H5Tget_size(handle->ftype_id);
    char *tmp = calloc(1, siz);
    H5Pset_fill_value(hdf_plist, handle->ftype_id, tmp);
    free(tmp);
  }

  /* See if chunking and/or compression should be enabled
    and if yes set the type of storage used to store the
    raw data for a dataset.
  */

  if (create_props != NULL  &&
      ( create_props->compression_type == MI_COMPRESS_ZLIB ||
        create_props->edge_count != 0 )
      )
  {
    /* Set the storage to CHUNKED */
    MI_CHECK_HDF_CALL_RET( stat = H5Pset_layout(hdf_plist, H5D_CHUNKED),"H5Pset_layout")
    

    if(create_props->edge_count != 0) {
      /* Create an array, hdf_size, containing the size of each chunk
      */
      for ( i=0; i < number_of_dimensions; i++) {
        hdf_size[i] = create_props->edge_lengths[i];
        /* If the size of each chunk is greater than the size of
          the corresponding dimension, set the chunk size to the
          dimension size
        */
        if (hdf_size[i] > dimensions[i]->length) {
            hdf_size[i] = dimensions[i]->length;
        }
      }
    } else {
      hsize_t val = 1;
      size_t unit_size = H5Tget_size(handle->ftype_id);
      /*adopted code from hdf_convenience.c:1360 to match behaviour of MINC1 API*/
      for( i = number_of_dimensions-1; i >= 0; i-- ) {
          if( _MI1_MAX_VAR_BUFFER_SIZE > dimensions[i]->length * val * unit_size ) {
              hdf_size[i] = dimensions[i]->length;
          } else {
            if ( dimensions[i]->length < (hsize_t)( _MI1_MAX_VAR_BUFFER_SIZE / ( val * unit_size ) ) )
              hdf_size[i] = dimensions[i]->length;
            else
              hdf_size[i] = (hsize_t)( _MI1_MAX_VAR_BUFFER_SIZE / ( val * unit_size ));
          }
          val *= hdf_size[i];
      }
    }

    /* Sets the size of the chunks used to store a chunked layout dataset */
    MI_CHECK_HDF_CALL_RET(stat = H5Pset_chunk(hdf_plist, number_of_dimensions, hdf_size),"H5Pset_chunk")
    
    /* Sets compression method and compression level */
    MI_CHECK_HDF_CALL_RET(stat = H5Pset_deflate(hdf_plist, create_props->zlib_level),"H5Pset_deflate")

    
    if (create_props->checksum )
    {
      MI_CHECK_HDF_CALL_RET(H5Pset_fletcher32(hdf_plist),"H5Pset_fletcher32")
    }

    
  } else { /* No COMPRESSION or CHUNKING is enabled */
    
    MI_CHECK_HDF_CALL_RET(stat = H5Pset_layout(hdf_plist, H5D_CONTIGUOUS),"H5Pset_layout") /*  CONTIGUOUS data */
  }

  /* See if Multi-res is set to a level above 0 and if yes create subgroups
    i.e., /minc-2.0/image/1/..
          /minc-2.0/image/2/..  etc
  */
  // must add some code to make sure that the res level is possible
  if (create_props != NULL && create_props->depth > 0) {
    for (i=0; i < create_props->depth ; i++) {
      if (minc_create_thumbnail(handle, i+1) < 0) {
        free(handle);
        return (MI_ERROR);
      }
    }
  }
  

  /* Try creating DIMENSIONS GROUP i.e. /minc-2.0/dimensions
  */
  MI_CHECK_HDF_CALL_RET(grp_id = H5Gopen1(file_id, MI_FULLDIMENSIONS_PATH),"H5Gopen1")
  /* Once the DIMENSIONS GROUP is opened, create each dimension.
  */

  for (i=0; i < number_of_dimensions ; i++) {
    /* First create the dataspace required to create a
      dimension variable (dataset)
    */
    if (dimensions[i]->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED) {
      dim[0] = dimensions[i]->length;
      MI_CHECK_HDF_CALL_RET(dataspace_id = H5Screate_simple(1, dim, NULL),"H5Screate_simple")
    } else {
      MI_CHECK_HDF_CALL_RET(dataspace_id = H5Screate(H5S_SCALAR),"H5Screate")
    }

    if (dataspace_id < 0) {
      free(handle);
      return (MI_ERROR);
    }
    
    dimension_is_vector= (strcmp ( dimensions[i]->name, MIvector_dimension ) == 0 );
    
    
    /* Create a dataset(dimension variable name) in DIMENSIONS GROUP */
    MI_CHECK_HDF_CALL_RET(dataset_id = H5Dcreate2(grp_id, dimensions[i]->name,
                            H5T_IEEE_F64LE, dataspace_id, H5P_DEFAULT,  H5P_DEFAULT, H5P_DEFAULT),"H5Dcreate2")

    /* Dimension variable for a regular dimension contains
      no meaningful data. Whereas, Dimension variable for
      an irregular dimension contains a vector with the lengths
      equal to the sampled points along the dimension.
      Also, create a variable named "<dimension>-width" and
      write the dimension->widths.
    */

    if(!dimension_is_vector )
      add_standard_minc_attributes(file_id,dataset_id);
      /*vector dimension is a record (?)*/
    
    /* Check for irregular dimension and make sure
      offset values are provided for this dimension
    */
    if (dimensions[i]->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED) {
      if (dimensions[i]->offsets == NULL) {
        free(handle);
        return (MI_ERROR);
      } else {

        /* If dimension is regularly sampled */
        MI_CHECK_HDF_CALL_RET(fspc_id = H5Dget_space(dataset_id),"H5Dget_space")
        
        /* Write the raw data from buffer (dimensions[i]->offsets)
          to the dataset.
        */
        MI_CHECK_HDF_CALL_RET(status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, dataspace_id,
                          fspc_id, H5P_DEFAULT, dimensions[i]->offsets),"H5Dwrite")
        
        /* Write the raw data from buffer (dimensions[i]->offsets)
          to the dataset.
        */
        size = strlen(dimensions[i]->name) + 6 + 1;
        name = malloc(size);
        strcpy(name, dimensions[i]->name);
        strcat(name, "-width");

        /* Create dataset dimension_name-width */
        dataset_width = H5Dcreate2(grp_id, name, H5T_IEEE_F64LE,
                                   dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        /* Return an Id for the dataspace of the dataset dataset_width */
        MI_CHECK_HDF_CALL_RET(fspc_id = H5Dget_space(dataset_width),"H5Dget_space")
        
        /* Write the raw data from buffer (dimensions[i]->widths)
          to the dataset.
        */
        MI_CHECK_HDF_CALL_RET(status = H5Dwrite(dataset_width, H5T_NATIVE_DOUBLE, dataspace_id, fspc_id, H5P_DEFAULT, dimensions[i]->widths),"H5Dwrite")
        
        miset_attr_at_loc(dataset_id, "dimorder", MI_TYPE_STRING,
                          strlen(dimensions[i]->name), dimensions[i]->name);

        miset_attr_at_loc(dataset_width, "dimorder", MI_TYPE_STRING,
                          strlen(dimensions[i]->name), dimensions[i]->name);

        /* Create new attribute "length", with appropriate
          type (to hdf5) conversion.
          miset_attr_at_loc(..) is implemented at m2utils.c
        */
        miset_attr_at_loc(dataset_width, "length", MI_TYPE_INT,
                          1, &dimensions[i]->length);
        /* Close the specified datatset */
        H5Dclose(dataset_width);
        free(name);
      }
    }

    if (dimensions[i]->attr & MI_DIMATTR_NOT_REGULARLY_SAMPLED) {
      name = "irregular";
    } else {
      name = "regular__";
    }
    /* Create attribute "spacing" and set its value to
      "regular__" or "irregular"
    */
    
    if(!dimension_is_vector)
      miset_attr_at_loc(dataset_id, "spacing", MI_TYPE_STRING,
                      strlen(name), name);

    switch (dimensions[i]->dim_class) {
    case MI_DIMCLASS_SPATIAL:
      name = "spatial";
      break;
    case MI_DIMCLASS_TIME:
      name = "time___";
      break;
    case MI_DIMCLASS_SFREQUENCY:
      name = "sfreq__";
      break;
    case MI_DIMCLASS_TFREQUENCY:
      name = "tfreq__";
      break;
    case MI_DIMCLASS_USER:
      name = "user___";
      break;
    case MI_DIMCLASS_RECORD:
      name = "record_";
      break;
    case MI_DIMCLASS_ANY:
    default:
      /* These should not be seen in this context!!!
      */
      return (MI_ERROR);
    }

    /* Save dimension length */
    miset_attr_at_loc(dataset_id, "length", MI_TYPE_INT,
                      1, &dimensions[i]->length);
    
    /* Create Dimension attribute "direction_cosines"  */
    if(dimensions[i]->dim_class == MI_DIMCLASS_SPATIAL)
      miset_attr_at_loc(dataset_id, "direction_cosines", MI_TYPE_DOUBLE,
                      3, dimensions[i]->direction_cosines);

    if(!dimension_is_vector)
    {
      const char *align_str;

      miset_attr_at_loc(dataset_id, "class", MI_TYPE_STRING, strlen(name),
                      name);


      /* Save step value. */
      miset_attr_at_loc(dataset_id, "step", MI_TYPE_DOUBLE,
                      1, &dimensions[i]->step);

      /* Save start value. */
      miset_attr_at_loc(dataset_id, "start", MI_TYPE_DOUBLE,
                      1, &dimensions[i]->start);

      if (dimensions[i]->align == MI_DIMALIGN_END)
        align_str = "end___";
      else if (dimensions[i]->align == MI_DIMALIGN_START)
        align_str = "start_";
      else
        align_str = "centre";
      miset_attr_at_loc(dataset_id, "alignment", MI_TYPE_STRING,
                        strlen(align_str), align_str);

      /* Save units. */
      miset_attr_at_loc(dataset_id, "units", MI_TYPE_STRING,
                      strlen(dimensions[i]->units), dimensions[i]->units);

      /* Save sample width. */
      miset_attr_at_loc(dataset_id, "width", MI_TYPE_DOUBLE,
                      1,  &dimensions[i]->width);
    }
    
    /* Save comments. If user has not specified
      any comments, do not add this attribute
    */

    if (dimensions[i]->comments != NULL) {
      miset_attr_at_loc(dataset_id, "comments", MI_TYPE_STRING,
                        strlen(dimensions[i]->comments),
                        dimensions[i]->comments);
    }

    /* Close the dataset with the specified Id
    */
    H5Dclose(dataset_id);


  } //for (i=0; i < number_of_dimensions ; i++)

  /* Close the group with the specified Id
  */
  H5Gclose(grp_id);

  /* Allocate space for all the dimension handles
    Note, each volume handle is associated with an array of
    dimension handles in the order that they were create (i.e, file order)
  */
  handle->dim_handles = (midimhandle_t *)malloc(number_of_dimensions *
                        sizeof(midimhandle_t));

  if (handle->dim_handles == NULL) {
    return MI_LOG_ERROR(MI2_MSG_OUTOFMEM,number_of_dimensions * sizeof(midimhandle_t));
  }

  /* Once the space for all dimension handles is created
    fill the dimension handle array with the appropriate
    dimension handle.
  */
  for (i = 0; i < number_of_dimensions; i++) {
    handle->dim_handles[i] = dimensions[i];
    dimensions[i]->volume_handle = handle;
  }

  miset_volume_world_indices(handle);

  /* Verify the volume type.
  */
  switch (volume_type) {
  case MI_TYPE_BYTE:
  case MI_TYPE_SHORT:
  case MI_TYPE_INT:
  case MI_TYPE_FLOAT:
  case MI_TYPE_DOUBLE:
  case MI_TYPE_STRING:
  case MI_TYPE_UBYTE:
  case MI_TYPE_USHORT:
  case MI_TYPE_UINT:
  case MI_TYPE_SCOMPLEX:
  case MI_TYPE_ICOMPLEX:
  case MI_TYPE_FCOMPLEX:
  case MI_TYPE_DCOMPLEX:
  case MI_TYPE_UNKNOWN:
    break;
  default:
    return MI_LOG_ERROR(MI2_MSG_BADTYPE,volume_type);
  }

  handle->volume_type = volume_type;

  /* Set the initial value of the valid-range
  */
  miinit_default_range(handle->volume_type,
                       &handle->valid_max,
                       &handle->valid_min);

  /* Get the voxel to world transform for the volume
  */
  miget_voxel_to_world(handle, handle->v2w_transform);

  /* Calculate the inverse transform */
  miinvert_transform(handle->v2w_transform, handle->w2v_transform);

  /* Allocated space for the volume properties */
  props_handle = (mivolumeprops_t)malloc(sizeof(struct mivolprops));
  /* Initialize volume properties with zero */
  memset(props_handle, 0, sizeof (struct mivolprops));
  /* If volume properties is specified by the user
    set all the properties of the volume handle
  */
  if (create_props != NULL) {
    /* Set the enable_flag for multi-resolution */
    props_handle->enable_flag = create_props->enable_flag;
    /* Set the depth of multi-resolution, i.e., how many
    levels of resolution is specified maximum is 16.
    */
    props_handle->depth = create_props->depth;
    /* Set compression type, currently two values
    either no compression or zlib is applicable.
    */
    switch (create_props->compression_type) {
    case MI_COMPRESS_NONE:
      props_handle->compression_type = MI_COMPRESS_NONE;
      break;
    case MI_COMPRESS_ZLIB:
      props_handle->compression_type = MI_COMPRESS_ZLIB;
      break;
    default:
      free(props_handle);
      return MI_LOG_ERROR(MI2_MSG_BADTYPE,create_props->compression_type);
    }
    /* Note that setting compression on (i.e., MI_COMPRESS_ZLIB)
    turns chunking on by default. Need to set the number of chunks
    (edge_count)
    */
    props_handle->zlib_level = create_props->zlib_level;
    props_handle->edge_count = create_props->edge_count;
    /* Allocate space for an array which holds the size of each chunk
    and fill the array with the appropriiate chunk sizes.
    */
    props_handle->edge_lengths = (int *)malloc(create_props->max_lengths*sizeof(int));
    for (i=0; i<create_props->max_lengths; i++) {
      props_handle->edge_lengths[i] = create_props->edge_lengths[i];
    }

    props_handle->max_lengths = create_props->max_lengths;
    props_handle->record_length = create_props->record_length;

    /* Explicitly allocate storage for name
    */
    if (create_props->record_name != NULL) {
      props_handle->record_name =malloc(strlen(create_props->record_name) + 1 );
      strcpy(props_handle->record_name, create_props->record_name);
    }
    props_handle->template_flag = create_props->template_flag;
  }
  /* Set the handle to volume properties */
  handle->create_props = props_handle;
  /* Return volume handle */
  *volume = handle;

  return (MI_NOERROR);
}

/** Return the number of dimensions associated with this volume.
  * \ingroup mi2Vol
*/
int miget_volume_dimension_count(mihandle_t volume, midimclass_t cls,
                             midimattr_t attr, int *number_of_dimensions)
{
  int i, count=0;
  /* Validate the parameters */
  if (volume == NULL || number_of_dimensions == NULL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to get dimension count with null volume or null variable");
  }
  /* For each dimension check to make sure that dimension class and
    attribute match with the specified parameters and if yes
    increment the dimension count
  */
  for (i=0; i< volume->number_of_dims; i++) {
    if ((cls == MI_DIMCLASS_ANY || volume->dim_handles[i]->dim_class == cls) &&
        (attr == MI_DIMATTR_ALL || volume->dim_handles[i]->attr == attr)) {
      count++;
    }
  }

  *number_of_dimensions = count;
  return (MI_NOERROR);
}

/** Returns the number of voxels in the volume.
  * \ingroup mi2Vol
*/
int miget_volume_voxel_count(mihandle_t volume, misize_t *number_of_voxels)
{
  char path[MI2_MAX_PATH];
  hid_t dset_id;
  hid_t fspc_id;

  /* Validate parameters */
  if (volume == NULL || number_of_voxels == NULL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to get voxel count with null volume or null variable");
  }

  /* Quickest way to do this is with the dataspace identifier of the
  * volume. Use the volume's current resolution.
  */
  sprintf(path, MI_ROOT_PATH "/image/%d/image", volume->selected_resolution);
  /* Open the dataset with the specified path
  */
  MI_CHECK_HDF_CALL_RET(dset_id = H5Dopen1(volume->hdf_id, path),"H5Dopen1");
  
  /* Get an Id to the copy of the dataspace */
  MI_CHECK_HDF_CALL_RET(fspc_id = H5Dget_space(dset_id),"H5Dget_space");
  /* Determines the number of elements in the dataspace and
    cast the result to an integer.
  */
  *number_of_voxels = (misize_t) H5Sget_simple_extent_npoints(fspc_id);
  /* Close the dataspace */
  H5Sclose(fspc_id);
  /* Close the dataset */
  H5Dclose(dset_id);
  return (MI_NOERROR);
}

/* Get the number of dimensions in the file */
static int _miget_file_dimension_count(hid_t file_id)
{
  hid_t dset_id;
  hid_t space_id;
  int result = -1;
  /* hdf5 macro can temporarily disable the automatic error printing */
  H5E_BEGIN_TRY {
    dset_id = midescend_path(file_id, MI_ROOT_PATH "/image/0/image");
  } H5E_END_TRY;

  if (dset_id >= 0) {
    /* Get an Id to the copy of the dataspace */
    MI_CHECK_HDF_CALL(space_id = H5Dget_space(dset_id),"H5Dget_space");
    
    if (space_id > 0) {
      /* Determine the dimensionality of the dataspace */
      MI_CHECK_HDF_CALL(result = H5Sget_simple_extent_ndims(space_id),"H5Sget_simple_extent_ndims");
      /* Close the dataspace */
      H5Sclose(space_id);
    }
    /* Close the dataset */
    H5Dclose(dset_id);
  }
  return (result);
}

/* Get dimension variable attributes for the given dimension name */
static int _miset_volume_class(mihandle_t volume, miclass_t volume_class)
{
  const char *class_ptr;

  switch (volume_class) {
  case MI_CLASS_REAL:
    class_ptr = "real___";
    break;
  case MI_CLASS_INT:
    class_ptr = "integer";
    break;
  case MI_CLASS_LABEL:
    class_ptr = "label__";
    break;
  case MI_CLASS_COMPLEX:
    class_ptr = "complex";
    break;
  case MI_CLASS_UNIFORM_RECORD:
    class_ptr = "array__";
    break;
  default:
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Unknown volume class");
  }
  miset_attribute(volume, MI_ROOT_PATH, "class", MI_TYPE_STRING,
                  strlen(class_ptr), class_ptr);
  return (MI_NOERROR);
}

static int _miget_volume_class(mihandle_t volume, miclass_t *volume_class)
{
  char class_buf[MI2_CHAR_LENGTH];

  if( miget_attribute(volume, MI_ROOT_PATH, "class", MI_TYPE_STRING,
                  MI2_CHAR_LENGTH, class_buf) == MI_NOERROR )
  {
    if (!strcmp(class_buf, "label__")) {
      *volume_class = MI_CLASS_LABEL;
    } else if (!strcmp(class_buf, "integer")) {
      *volume_class = MI_CLASS_INT;
    } else if (!strcmp(class_buf, "complex")) {
      *volume_class = MI_CLASS_COMPLEX;
    } else if (!strcmp(class_buf, "array__")) {
      *volume_class = MI_CLASS_UNIFORM_RECORD;
    } else {
      *volume_class = MI_CLASS_REAL;
    }
  } else {
    /*probably volume doesn't have this attribute*/
    *volume_class = MI_CLASS_REAL;
  }
  
  return (MI_NOERROR);
}

/* Read the irregular spacing information from a file.
 */
static int _miget_irregular_spacing(mihandle_t hvol, midimhandle_t hdim)
{
  herr_t status;
  hid_t dset_id;
  hid_t dspc_id;
  char path[MI2_CHAR_LENGTH];
  hssize_t n_points;

  sprintf(path, MI_ROOT_PATH "/dimensions/%s", hdim->name);
  MI_CHECK_HDF_CALL_RET(dset_id = H5Dopen1(hvol->hdf_id, path),"H5Dopen1");
  MI_CHECK_HDF_CALL_RET(dspc_id = H5Dget_space(dset_id), "H5Dget_space");

  n_points = H5Sget_simple_extent_npoints(dspc_id);

  hdim->offsets = malloc(n_points * sizeof(double));
  if (hdim->offsets == NULL)
    return MI_LOG_ERROR(MI2_MSG_OUTOFMEM, n_points * sizeof(double));

  /* Read the raw data to buffer (dimensions[i]->offsets)
     from the dataset.
  */
  MI_CHECK_HDF_CALL_RET(status = H5Dread(dset_id, H5T_NATIVE_DOUBLE, 
                                         H5S_ALL, H5S_ALL, H5P_DEFAULT,
                                         hdim->offsets), "H5Dread")
        
  H5Dclose(dset_id);
  sprintf(path, MI_ROOT_PATH "/dimensions/%s-width", hdim->name);
  dset_id = H5Dopen1(hvol->hdf_id, path);
  if (dset_id < 0) {
    /* Unfortunately, the emulation library in MINC1 puts this variable
     * in the wrong place.
     */
    sprintf(path, MI_ROOT_PATH "/info/%s-width", hdim->name);
    dset_id = H5Dopen1(hvol->hdf_id, path);
    if (dset_id < 0) {
      return 0;
    }
  }
  hdim->widths = malloc(n_points * sizeof(double));
  if (hdim->widths == NULL)
    return MI_LOG_ERROR(MI2_MSG_OUTOFMEM, n_points * sizeof(double));

  MI_CHECK_HDF_CALL_RET(status = H5Dread(dset_id, H5T_NATIVE_DOUBLE, 
                                         H5S_ALL, H5S_ALL, H5P_DEFAULT, 
                                         hdim->widths), "H5Dread")
  H5Dclose(dset_id);
  return 0;
}

/* Get dimension variable attributes for the given dimension name */
static int _miget_file_dimension(mihandle_t volume, const char *dimname,
                      midimhandle_t *hdim_ptr)
{
  char path[MI2_CHAR_LENGTH];
  char temp[MI2_CHAR_LENGTH];
  midimhandle_t hdim;
  unsigned int len;

  /* Create a path with the dimension name */
  sprintf(path, MI_ROOT_PATH "/dimensions/%s", dimname);
  /* Allocate space for the dimension handle */
  hdim = (midimhandle_t) malloc(sizeof (*hdim));
  /* Initialize everything to zero */
  memset(hdim, 0, sizeof (*hdim));

  hdim->name = strdup(dimname);

  /* hdf5 macro can temporarily disable the automatic error printing */
  H5E_BEGIN_TRY {
    int r;
    /* Get the attribute (spacing) from a minc file */
    r = miget_attribute(volume, path, "spacing", MI_TYPE_STRING, MI2_CHAR_LENGTH, temp);
    
    if (r==MI_NOERROR && !strcmp(temp, "irregular")) {
      hdim->attr |= MI_DIMATTR_NOT_REGULARLY_SAMPLED;
      _miget_irregular_spacing(volume, hdim);
    } else {
      hdim->attr |= MI_DIMATTR_REGULARLY_SAMPLED;
    }

    /* Get the attribute (class) from a minc file */
    r = miget_attribute(volume, path, "class", MI_TYPE_STRING,  MI2_CHAR_LENGTH, temp);
    if (r < 0) {
      /* Get the default class. */
      if (!strcmp(dimname, MItime)) {
        hdim->dim_class = MI_DIMCLASS_TIME;
      } else if (!strcmp(dimname, MIvector_dimension)) {
        hdim->dim_class = MI_DIMCLASS_RECORD;
        hdim->step = 0.0;
      } else {
        hdim->dim_class =  MI_DIMCLASS_SPATIAL;
      }
    } else {
      if (!strcmp(temp, "spatial")) {
        hdim->dim_class = MI_DIMCLASS_SPATIAL;
      } else if (!strcmp(temp, "time___")) {
        hdim->dim_class = MI_DIMCLASS_TIME;
      } else if (!strcmp(temp, "sfreq__")) {
        hdim->dim_class = MI_DIMCLASS_SFREQUENCY;
      } else if (!strcmp(temp, "tfreq__")) {
        hdim->dim_class = MI_DIMCLASS_TFREQUENCY;
      } else if (!strcmp(temp, "user___")) {
        hdim->dim_class = MI_DIMCLASS_USER;
      } else if (!strcmp(temp, "record_")) {
        hdim->dim_class = MI_DIMCLASS_RECORD;
      } else {
        MI_LOG_ERROR(MI2_MSG_GENERIC,"Unknown dimension type");
      }
    }
    /* Get the attribute (length) from a minc file. We have to do this in
     * two steps, as MI_TYPE_UINT is not necessarily the same size as 
     * hsize_t/misize_t, so we have to read the value into a variable of
     * the right type, then assign it to the structure member, to guarantee 
     * proper promotion.
     */
    r = miget_attribute(volume, path, "length", MI_TYPE_UINT, 1, &len);
    if (r < 0) {
      MI_LOG_ERROR(MI2_MSG_GENERIC,"Can't determine dimension length");
    }
    hdim->length = len;         /* Will promote unsigned int to misize_t. */

    /* Get the attribute (start) from a minc file for NON vector_dimension only */
    if (strcmp(dimname, "vector_dimension")) {
      r = miget_attribute(volume, path, MIstart, MI_TYPE_DOUBLE, 1, &hdim->start);
      if (r < 0) {
        hdim->start = 0.0;
      }
      /* Get the attribute (step) from a minc file */
      r = miget_attribute(volume, path, MIstep, MI_TYPE_DOUBLE, 1, &hdim->step);
      if (r < 0) {
        hdim->step = 1.0;
      }
    }
    /* Get the attribute (direction_cosines) from a minc file */
    r = miget_attribute(volume, path, MIdirection_cosines, MI_TYPE_DOUBLE, 3,
                        hdim->direction_cosines);
    if (r < 0) {
      hdim->direction_cosines[MI2_X] = 0.0;
      hdim->direction_cosines[MI2_Y] = 0.0;
      hdim->direction_cosines[MI2_Z] = 0.0;
      if (!strcmp(dimname, MIxspace)) {
        hdim->direction_cosines[MI2_X] = 1.0;
      } else if (!strcmp(dimname, MIyspace)) {
        hdim->direction_cosines[MI2_Y] = 1.0;
      } else if (!strcmp(dimname, MIzspace)) {
        hdim->direction_cosines[MI2_Z] = 1.0;
      }
    }

    r = miget_attribute(volume, path, "units", MI_TYPE_STRING,
                        MI2_CHAR_LENGTH, temp);
    if (r < 0) {
      hdim->units = strdup("");
    } else {
      hdim->units = strdup(temp);
    }

  } H5E_END_TRY;
  /* Return the dimension handle */
  *hdim_ptr = hdim;
  hdim->volume_handle = volume;
  return (MI_NOERROR);
}


/** Opens an existing MINC volume for read-only access if mode argument is
  * MI2_OPEN_READ, or read-write access if mode argument is MI2_OPEN_RDWR.
  * \ingroup mi2Vol
*/
int miopen_volume(const char *filename, int mode, mihandle_t *volume)
{
  hid_t file_id;
  hid_t dset_id;
  hid_t space_id;
  mihandle_t handle;
  int hdf_mode;
  char dimorder[MI2_CHAR_LENGTH];
  int i,r;
  char *p1, *p2;
  H5T_class_t hdf_class;
  size_t nbytes;
  int is_signed;
  int n_dimensions;

  /* Initialization.
    For the actual body of this function look at m2utils.c
  */
  miinit();
  /* Convert the specified mode to hdf mode */
  if (mode == MI2_OPEN_READ) {
    hdf_mode = H5F_ACC_RDONLY;
  } else if (mode == MI2_OPEN_RDWR) {
    hdf_mode = H5F_ACC_RDWR;
  } else {
    return (MI_ERROR);
  }
  /* Allocate space for the volume handle */
  handle = mialloc_volume_handle();
  if (handle == NULL) {
    return MI_LOG_ERROR(MI2_MSG_OUTOFMEM,sizeof(struct mivolume));
  }
  
  /* Open the hdf file using the given filename and mode */
  file_id = _hdf_open(filename, hdf_mode);
 
  if (file_id < 0) {
    /*try to convert MINC1 file*/
#ifdef HAVE_MINC1
    char * temp_file=NULL;

    if ( mode == MI2_OPEN_READ )
    {
      if( (temp_file=micreate_tempfile()))
      {
         if( minc_format_convert(filename,temp_file) == MI_NOERROR )
         {
           if( (file_id = _hdf_open(temp_file, hdf_mode) ) >0)
           {
            unlink( temp_file ); /*file will be deleted immedeately after closing...*/
            free( temp_file );
           } else {
            unlink( temp_file );
            free( temp_file );
            free( handle );
            return MI_LOG_ERROR(MI2_MSG_OPENFILE,filename);
           }
         } else {
           free( temp_file );
           free( handle );
           return MI_LOG_ERROR(MI2_MSG_OPENFILE,filename);
         }
      } else {
         free( temp_file );
         free( handle );
         return MI_LOG_ERROR(MI2_MSG_OPENFILE,filename);
      }
    } else {
      free( handle );
      return MI_LOG_ERROR(MI2_MSG_OPENFILE,filename);
    }
#else
    free( handle );
    return MI_LOG_ERROR(MI2_MSG_OPENFILE,filename);
#endif    
  }
  /* Set some varibales associated with the volume handle */
  handle->hdf_id = file_id;
  handle->mode = mode;

  /* Get the volume class.
  */
  _miget_volume_class(handle, &handle->volume_class);

  /* GET THE DIMENSION COUNT
  */
  n_dimensions = handle->number_of_dims = _miget_file_dimension_count(file_id);
  
  if( n_dimensions <= 0 ) {
    free(handle);
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to open minc file without image variable");
  }

  /* READ EACH OF THE DIMENSIONS
  */
  handle->dim_handles = (midimhandle_t *)malloc(n_dimensions *
                        sizeof(midimhandle_t));
  
  if(handle->dim_handles == NULL) {
    free(handle);
    return MI_LOG_ERROR(MI2_MSG_OUTOFMEM, n_dimensions * sizeof(midimhandle_t));
  }
  
  /* Get the attribute (dimorder) from the image dataset */
  r =  miget_attribute(handle, MI_ROOT_PATH "/image/0/image", "dimorder",
                       MI_TYPE_STRING, sizeof(dimorder), dimorder);

  if ( r < 0) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Can't determine dimension order");
  }
  p1 = dimorder;
  /* Break the ordered, comma-separated list of dimension names
    to get each individual dimension name */
  for (i = 0; i < handle->number_of_dims; i++) {
    p2 = strchr(p1, ',');
    if (p2 != NULL) {
      *p2 = '\0';
    }
    /* Get dimension variable attributes for each dimension */
    _miget_file_dimension(handle, p1, &handle->dim_handles[i]);
    p1 = p2 + 1;
  }

  if( miset_volume_world_indices(handle) < 0 ) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Can't determine world indices");
  }

  /* SEE IF SLICE SCALING IS ENABLED
  */
  handle->has_slice_scaling = FALSE;
  /* hdf5 macro can temporarily disable the automatic error printing */
  H5E_BEGIN_TRY {
    /* Open the dataset image-max at the specified path*/
    dset_id = H5Dopen1(file_id, MI_ROOT_PATH "/image/0/image-max");
  } H5E_END_TRY;
  
  if (dset_id >= 0) {
    /* Get the Id of the copy of the dataspace of the dataset */
    space_id = H5Dget_space(dset_id);
    if (space_id >= 0) {
      
      /* If the dimensionality of the image-max variable is one or
      * greater, we consider this volume to have slice-scaling enabled.
      */
      if ( H5Sget_simple_extent_ndims(space_id) >= 1) {
        handle->has_slice_scaling = TRUE;
      }
      H5Sclose(space_id);	/* Close the dataspace handle */
    }
    H5Dclose(dset_id);	/* Close the dataset handle */
  }

  if (!handle->has_slice_scaling) {
    /* Read the minimum scalar of the given type at the specified path */
    miget_scalar(handle->hdf_id, H5T_NATIVE_DOUBLE,
                 MI_ROOT_PATH "/image/0/image-min", &handle->scale_min);
    /* Read the maximum scalar of the given type at the specified path */
    miget_scalar(handle->hdf_id, H5T_NATIVE_DOUBLE,
                 MI_ROOT_PATH "/image/0/image-max", &handle->scale_max);
  }

  /* Read the current voxel-to-world transform */
  miget_voxel_to_world(handle, handle->v2w_transform);

  /* Calculate the inverse transform */
  miinvert_transform(handle->v2w_transform, handle->w2v_transform);

  /* Open the image dataset */
  MI_CHECK_HDF_CALL_RET(handle->image_id = H5Dopen1(file_id, MI_ROOT_PATH "/image/0/image"),"H5Dopen1");
  /* Get the Id for the copy of the datatype for the dataset */
  MI_CHECK_HDF_CALL_RET(handle->ftype_id = H5Dget_type(handle->image_id),"H5Dget_type");

  switch (H5Tget_class(handle->ftype_id)) {
  case H5T_INTEGER:
  case H5T_FLOAT:
    handle->mtype_id = H5Tget_native_type(handle->ftype_id,
                                          H5T_DIR_ASCEND);
    break;

  case H5T_COMPOUND:
    handle->mtype_id = H5Tcreate(H5T_COMPOUND,
                                 H5Tget_size(handle->ftype_id));
    for (i = 0; i < H5Tget_nmembers(handle->ftype_id); i++) {
      hid_t tmp_id = H5Tget_member_type(handle->ftype_id, i);
      size_t tmp_off = H5Tget_member_offset(handle->ftype_id, i);
      char *tmp_nm = H5Tget_member_name(handle->ftype_id, i);
      hid_t tmp2_id = H5Tget_native_type(tmp_id, H5T_DIR_ASCEND);
      H5Tinsert(handle->mtype_id, tmp_nm, tmp_off, tmp2_id);

      free(tmp_nm);
      H5Tclose(tmp_id);
      H5Tclose(tmp2_id);
    }
    break;

  case H5T_ENUM:
    handle->mtype_id = H5Tget_native_type(handle->ftype_id, H5T_DIR_ASCEND);
    miinit_enum(handle->ftype_id);
    miinit_enum(handle->mtype_id);
    break;

  default:
    return (MI_ERROR);
  }

  /* hdf5 macro can temporarily disable the automatic error printing */
  H5E_BEGIN_TRY {
    /* Open both image-min and image-max datasets */
    handle->imax_id = H5Dopen1(file_id, MI_ROOT_PATH "/image/0/image-max");
    handle->imin_id = H5Dopen1(file_id, MI_ROOT_PATH "/image/0/image-min");
  } H5E_END_TRY;

  /* Convert the type to a MINC type.
  */
  /* Get the class Id for the datatype */
  hdf_class = H5Tget_class(handle->ftype_id);
  /* Get the size of the datatype */
  nbytes = H5Tget_size(handle->ftype_id);

  switch (hdf_class) {
  case H5T_INTEGER:
  case H5T_ENUM:              /* label images */
    is_signed = (H5Tget_sign(handle->ftype_id) == H5T_SGN_2);

    switch (nbytes) {
    case 1:
      handle->volume_type = (is_signed ? MI_TYPE_BYTE : MI_TYPE_UBYTE);
      break;
    case 2:
      handle->volume_type = (is_signed ? MI_TYPE_SHORT : MI_TYPE_USHORT);
      break;
    case 4:
      handle->volume_type = (is_signed ? MI_TYPE_INT : MI_TYPE_UINT);
      break;
    default:
      return MI_LOG_ERROR(MI2_MSG_BADTYPE,hdf_class);
    }
    break;
  case H5T_FLOAT:
    handle->volume_type = (nbytes == 4) ? MI_TYPE_FLOAT : MI_TYPE_DOUBLE;
    break;
  case H5T_STRING:
    handle->volume_type = MI_TYPE_STRING;
    break;
  case H5T_ARRAY:
    /* TODO: handle this case for uniform records (arrays)? */
    break;
  case H5T_COMPOUND:
    /* TODO: handle this case for non-uniform records? */
    break;
  default:
    return MI_LOG_ERROR(MI2_MSG_BADTYPE,hdf_class);
  }

  /* Read the current settings for valid-range */
  miread_valid_range(handle, &handle->valid_max, &handle->valid_min);

  *volume = handle;
  return (MI_NOERROR);
}

/** Writes any changes associated with the volume to disk.
    \ingroup mi2Vol
*/
static int miflush_volume(mihandle_t volume)
{
  if ((volume->mode & MI2_OPEN_RDWR) != 0) {
    H5Fflush(volume->hdf_id, H5F_SCOPE_GLOBAL);
    misave_valid_range(volume);
  }
  return (MI_NOERROR);
}

/** Close an existing MINC volume. If the volume was newly created,
  *  all changes will be written to disk. In all cases this function closes
  *  the open volume and frees memory associated with the volume handle.
  *  \ingroup mi2Vol
*/
int miclose_volume(mihandle_t volume)
{
  int i;
  
  if (volume == NULL) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to close null volume");
  }

  if (volume->is_dirty) {
    minc_update_thumbnails(volume);
    volume->is_dirty = FALSE;
  }

  miflush_volume(volume);

  if (volume->image_id > 0) {
    H5Dclose(volume->image_id);
  }
  if (volume->imax_id > 0) {
    H5Dclose(volume->imax_id);
  }
  if (volume->imin_id > 0) {
    H5Dclose(volume->imin_id);
  }
  if (volume->ftype_id > 0) {
    H5Tclose(volume->ftype_id);
  }
  if (volume->mtype_id > 0) {
    H5Tclose(volume->mtype_id);
  }
  if (volume->plist_id > 0) {
    H5Pclose(volume->plist_id);
  }
  if (_hdf_close(volume->hdf_id) < 0) {
    return (MI_ERROR);
  }
  if (volume->dim_handles != NULL) {
    
    for(i=0;i<volume->number_of_dims;i++)
    {
      mifree_dimension_handle(volume->dim_handles[i]);
    }
    
    free(volume->dim_handles);
  }
  if (volume->dim_indices != NULL) {
    free(volume->dim_indices);
  }
  if (volume->create_props != NULL) {
    mifree_volume_props(volume->create_props);
  }
  
  free(volume);

  return (MI_NOERROR);
}



/** \internal
*/
void miinit_default_range(mitype_t mitype, double *valid_max, double *valid_min)
{
  switch (mitype) {
  case MI_TYPE_BYTE:
    *valid_min = (double)CHAR_MIN;
    *valid_max = (double)CHAR_MAX;
    break;
  case MI_TYPE_SHORT:
    *valid_min = (double)SHRT_MIN;
    *valid_max = (double)SHRT_MAX;
    break;
  case MI_TYPE_INT:
    *valid_min = (double)INT_MIN;
    *valid_max = (double)INT_MAX;
    break;
  case MI_TYPE_UBYTE:
    *valid_min = 0.0;
    *valid_max = (double)UCHAR_MAX;
    break;
  case MI_TYPE_USHORT:
    *valid_min = 0.0;
    *valid_max = (double)USHRT_MAX;
    break;
  case MI_TYPE_UINT:
    *valid_min = 0.0;
    *valid_max = (double)UINT_MAX;
    break;
  case MI_TYPE_FLOAT:
    *valid_min = (double)-FLT_MAX;
    *valid_max = (double)FLT_MAX;
    break;
  case MI_TYPE_DOUBLE:
    *valid_min = -DBL_MAX;
    *valid_max = DBL_MAX;
    break;
  case MI_TYPE_DCOMPLEX:
    *valid_min = -DBL_MAX;
    *valid_max = DBL_MAX;
    break;
  case MI_TYPE_FCOMPLEX:
    *valid_min = (double)-FLT_MAX;
    *valid_max = (double)FLT_MAX;
    break;
  default:
    *valid_min = 0.0;
    *valid_max = 1.0;
    MI_LOG_ERROR(MI2_MSG_BADTYPE,mitype);
    break;
  }
}

/** \internal
 */
static void miread_valid_range(mihandle_t volume, double *valid_max, double *valid_min)
{
  int r;
  double range[2];

  H5E_BEGIN_TRY {
    r = miget_attribute(volume, MI_ROOT_PATH "/image/0/image", "valid_range", MI_TYPE_DOUBLE, 2, range);
  } H5E_END_TRY;
  if (r == MI_NOERROR) {
    if (range[0] < range[1]) {
      *valid_min = range[0];
      *valid_max = range[1];
    } else {
      *valid_min = range[1];
      *valid_max = range[0];
    }
  } else {
    /* Didn't find the attribute, so assign default values. */
    miinit_default_range(volume->volume_type, valid_max, valid_min);
  }
}

/** \internal
* This function saves the current valid range set for a MINC file.
*/
void misave_valid_range(mihandle_t volume)
{
  double range[2];
  range[0] = volume->valid_min;
  range[1] = volume->valid_max;
  
  miset_attribute(volume, MI_ROOT_PATH "/image/0/image", "valid_range",
                  MI_TYPE_DOUBLE, 2, range);
}


int miget_slice_dimension_count(mihandle_t volume, midimclass_t dimclass,
                                midimattr_t attr, int *number_of_dimensions)
{
  int number_of_volume_dimensions=-1;
  hid_t image_max_fspc_id;
  int slice_ndims;
  int result=-1;
  if( miget_volume_dimension_count(volume,dimclass,attr, &number_of_volume_dimensions) <0 )
  {
    return -1;
  }

  if(!volume->has_slice_scaling)
  {
    *number_of_dimensions=number_of_volume_dimensions;
    return MI_NOERROR;
  }
  
  image_max_fspc_id=H5Dget_space(volume->imax_id);
  slice_ndims = H5Sget_simple_extent_ndims ( image_max_fspc_id );
  if(slice_ndims>=0)
  {
    result=MI_NOERROR;
    *number_of_dimensions=number_of_volume_dimensions-slice_ndims;
  }
  H5Sclose(image_max_fspc_id);
  return result;
}


/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
