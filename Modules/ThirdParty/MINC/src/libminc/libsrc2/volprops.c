/** \file volprops.c
 * \brief MINC 2.0 Volume properties functions
 * \author Leila Baghdadi
 *
 * These functions manipulate "volume properties" objects, which are
 * used to control several options related to MINC 2.0 volume structure.
 * These include compression, blocking, multi-resolution, and record s
 * structure.
 *
 * This approach was adopted with the intent that it would make the
 * default volume creation as simple as possible, while allowing a
 * lot of control for more advanced applications.  This approach to
 * managing properties is also believed to be more readily extensible than
 * any obvious alternative.
 ************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#define _GNU_SOURCE 1
#include <stdlib.h>
#include <hdf5.h>
#include "minc2.h"
#include "minc2_private.h"
#include "minc_config.h"

/**
 * \defgroup mi2VPrp MINC 2.0 Volume Properties Functions
 */

/** Maximum number of elements in a filter parameter list. */
#define MI2_MAX_CD_ELEMENTS 100

/** Create a volume property list.  The new list will be returned in the
 * \a props parameter.    When the program is finished
 * using the property list it should call  mifree_volume_props() to free the
 * memory associated with the list.
 * \param props A pointer to the returned volume properties handle.
 * \ingroup mi2VPrp
 */
int minew_volume_props(mivolumeprops_t  *props)
{
  mivolumeprops_t handle;
  
  handle = (mivolumeprops_t)malloc(sizeof(struct mivolprops));
  
  if (handle == NULL) {
    return (MI_ERROR);
  }
  /* Initialize all the fields.
   */
  handle->enable_flag = FALSE;
  handle->depth = 0;
  handle->compression_type = MI_COMPRESS_NONE;
  handle->zlib_level = 0;
  handle->edge_count = 0;
  handle->edge_lengths = NULL;
  handle->max_lengths = 0;
  handle->record_length = 0;
  handle->record_name = NULL;
  handle->template_flag = 0;
  handle->checksum = miget_cfg_bool(MICFG_MINC_CHECKSUM);
  
  *props = handle;
  
  return (MI_NOERROR);
}

/** Destroy a volume property list.
 * \param props The volume property list to delete.
 * \ingroup mi2VPrp
 */
int mifree_volume_props(mivolumeprops_t props)
{
  if (props == NULL) {
    return (MI_ERROR);
  }
  if (props->edge_lengths != NULL) {
    free(props->edge_lengths);
  }
  if (props->record_name != NULL) {
    free(props->record_name);
  }
  free(props);
  return (MI_NOERROR);
}

/*! Get a copy of the volume property list.  When the program is finished
 * using the property list it should call  mifree_volume_props() to free the
 * memory associated with the list.
 * \param volume A volume handle
 * \param props A pointer to the returned volume properties handle.
 * \ingroup mi2VPrp
 */
int miget_volume_props(mihandle_t volume, mivolumeprops_t *props)
{
  mivolumeprops_t handle;
  hid_t hdf_vol_dataset;
  hid_t hdf_plist;
  int nfilters;
  unsigned int flags;
  size_t cd_nelmts;
  unsigned int cd_values[MI2_MAX_CD_ELEMENTS];
  char fname[MI2_CHAR_LENGTH];
  int fcode;
  
  if (volume->hdf_id < 0) {
    return (MI_ERROR);
  }
  hdf_vol_dataset = midescend_path(volume->hdf_id, "/minc-2.0/image/0/image");
  if (hdf_vol_dataset < 0) {
    return (MI_ERROR);
  }
  hdf_plist = H5Dget_create_plist(hdf_vol_dataset);
  if (hdf_plist < 0) {
    return (MI_ERROR);
  }
  handle = (mivolumeprops_t)malloc(sizeof(struct mivolprops));
  if (handle == NULL) {
    return (MI_ERROR);
  }
  /* Get the layout of the raw data for a dataset.
   */
  if (H5Pget_layout(hdf_plist) == H5D_CHUNKED) {
    hsize_t dims[MI2_MAX_VAR_DIMS];
    int i;
    /* Returns chunk dimensionality */
    handle->edge_count = H5Pget_chunk(hdf_plist, MI2_MAX_VAR_DIMS, dims);
    if (handle->edge_count < 0) {
      free(handle);
      return (MI_ERROR);
    }
    handle->edge_lengths = (int *)malloc(handle->edge_count*sizeof(int));
    if (handle->edge_lengths == NULL) {
      free(handle);
      return (MI_ERROR);
    }
    for (i = 0; i < handle->edge_count; i++) {
      handle->edge_lengths[i] = dims[i];
    }
    /* Get the number of filters in the pipeline */
    nfilters = H5Pget_nfilters(hdf_plist);
    if (nfilters == 0) {
      handle->zlib_level = 0;
      handle->compression_type = MI_COMPRESS_NONE;
      handle->checksum = 0;
    }
    else {
      for (i = 0; i < nfilters; i++) {
        cd_nelmts = MI2_MAX_CD_ELEMENTS;
        fcode = H5Pget_filter1(hdf_plist, i, &flags, &cd_nelmts,
                               cd_values, sizeof(fname), fname);
        switch (fcode) {
          case H5Z_FILTER_DEFLATE:
            handle->compression_type = MI_COMPRESS_ZLIB;
            handle->zlib_level = cd_values[0];
            break;
          case H5Z_FILTER_SHUFFLE:
            break;
          case H5Z_FILTER_FLETCHER32:
            handle->checksum=1;
            break;
          case H5Z_FILTER_SZIP:
            break;
          default:
            break;
        }
      }
    }
    
  }
  else {
    handle->edge_count = 0;
    handle->edge_lengths = NULL;
    handle->zlib_level = 0;
    handle->compression_type = MI_COMPRESS_NONE;
    handle->checksum = 0;
  }
  
  *props = handle;
  
  H5Pclose(hdf_plist);
  H5Dclose(hdf_vol_dataset);
  
  return (MI_NOERROR);
  
}


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
                                 int depth)
{
  if (props == NULL || depth > MI2_MAX_RESOLUTION_GROUP || depth <= 0) {
    return (MI_ERROR);
  }
  
  props->enable_flag = enable_flag;
  props->depth = depth;
  return (MI_NOERROR);
}

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
                                 int *depth)
{
  if (props == NULL || enable_flag == NULL || depth == NULL) {
    return (MI_ERROR);
  }
  
  *enable_flag = props->enable_flag;
  *depth = props->depth;
  
  return (MI_NOERROR);
}

/** Select a different resolution from a multi-resolution image.
 * \ingroup mi2VPrp
 */
int miselect_resolution(mihandle_t volume, int depth)
{
  hid_t grp_id;
  char path[MI2_MAX_PATH];
  
  if ( volume->hdf_id < 0 || depth > MI2_MAX_RESOLUTION_GROUP || depth < 0) {
    return (MI_ERROR);
  }
  
  grp_id = H5Gopen1(volume->hdf_id, MI_ROOT_PATH "/image");
  if (grp_id < 0) {
    return (MI_ERROR);
  }
  /* Check given depth with the available depth in file.
   Make sure the selected resolution does exist.
   */
  if (depth > volume->create_props->depth) {
    return (MI_ERROR);
  }
  else if (depth != 0) {
    if (minc_update_thumbnail(volume, grp_id, 0, depth) < 0) {
      return (MI_ERROR);
    }
  }
  
  volume->selected_resolution = depth;
  
  if (volume->image_id >= 0) {
    H5Dclose(volume->image_id);
  }
  sprintf(path, "%d/image", depth);
  volume->image_id = H5Dopen1(grp_id, path);
  
  if (volume->volume_class == MI_CLASS_REAL) {
    if (volume->imax_id >= 0) {
      H5Dclose(volume->imax_id);
    }
    sprintf(path, "%d/image-max", depth);
    volume->imax_id = H5Dopen1(grp_id, path);
    
    if (volume->imin_id >= 0) {
      H5Dclose(volume->imin_id);
    }
    sprintf(path, "%d/image-min", depth);
    volume->imin_id = H5Dopen1(grp_id, path);
  }
  return (MI_NOERROR);
}

/** Compute or recompute all resolution groups.
 *
 * \ingroup mi2VPrp
 */
int miflush_from_resolution(mihandle_t volume, int depth)
{
  if ( volume->hdf_id < 0 || depth > MI2_MAX_RESOLUTION_GROUP || depth <= 0) {
    return (MI_ERROR);
  }
  
  if (depth > volume->create_props->depth) {
    return (MI_ERROR);
  }
  else {
    if (minc_update_thumbnails(volume) < 0) {
      return (MI_ERROR);
    }
    volume->is_dirty = FALSE;
  }
  
  return (MI_NOERROR);
}

/** Set compression type for a volume property list
 * Note that enabling compression will automatically
 * enable blocking with default parameters.
 * \param props A volume properties list
 * \param compression_type The type of compression to use (MI_COMPRESS_NONE
 * or MI_COMPRESS_ZLIB)
 * \ingroup mi2VPrp
 */
int miset_props_compression_type(mivolumeprops_t props,
                                 micompression_t compression_type)
{
/*
  int i;
  int edge_lengths[MI2_MAX_VAR_DIMS];
*/
  
  if (props == NULL) {
    return (MI_ERROR);
  }
  switch (compression_type) {
    case MI_COMPRESS_NONE:
      props->compression_type = MI_COMPRESS_NONE;
      break;
    case MI_COMPRESS_ZLIB:
      props->compression_type = MI_COMPRESS_ZLIB;
      props->zlib_level = MI2_DEFAULT_ZLIB_LEVEL;
      
      /*
       * VF: this will be set on volume creation, when dimensions are known!
       * 
      
      for (i = 0; i < MI2_MAX_VAR_DIMS; i++) {
        edge_lengths[i] = MI2_CHUNK_SIZE ;
      }
      miset_props_blocking(props, MI2_MAX_VAR_DIMS, edge_lengths);
      */
      
      break;
    default:
      return (MI_ERROR);
  }
  return (MI_NOERROR);
}

/** Get compression type for a volume property list
 * \param props A volume property list handle
 * \param compression_type A pointer to a variable to which the current
 * compression type will be assigned.
 * \ingroup mi2VPrp
 */
int miget_props_compression_type(mivolumeprops_t props,
                                 micompression_t *compression_type)
{
  
  if (props == NULL) {
    return (MI_ERROR);
  }
  
  *compression_type = props->compression_type;
  return (MI_NOERROR);
}

/** Set zlib compression properties for a volume list.  The \a zlib_level
 * parameter may range from 1 to 9, where higher numbers request that the
 * library attempt to use more memory (and possibly processing power) to
 * achieve the highest possible compression ratio.
 *
 * \param props A volume property list handle
 * \param zlib_level An integer specifying the desired compression level.
 * \ingroup mi2VPrp
 */
int miset_props_zlib_compression(mivolumeprops_t props, int zlib_level)
{
  if (props == NULL || zlib_level > MI2_MAX_ZLIB_LEVEL) {
    return (MI_ERROR);
  }
  
  props->zlib_level = zlib_level;
  return (MI_NOERROR);
}

/** Get zlib compression properties from a volume property list.
 * \param props A volume property list handle
 * \param zlib_level Pointer to an integer variable that will receive the
 * current compression level.
 * \ingroup mi2VPrp
 */
int miget_props_zlib_compression(mivolumeprops_t props, int *zlib_level)
{
  if (props == NULL) {
    return (MI_ERROR);
  }
  
  *zlib_level = props->zlib_level;
  return (MI_NOERROR);
}

/** Set blocking structure properties for the volume
 * \param props A volume property list handle
 * \param edge_count
 * \param edge_lengths
 * \ingroup mi2VPrp
 */
int miset_props_blocking(mivolumeprops_t props, int edge_count, const int *edge_lengths)
{
  int i;
  
  if (props == NULL || edge_count > MI2_MAX_VAR_DIMS) {
    return (MI_ERROR);
  }
  
  if (props->edge_lengths != NULL) {
    free(props->edge_lengths);
    props->edge_lengths = NULL;
  }
  
  props->edge_count = edge_count;
  if (edge_count != 0) {
    props->edge_lengths = (int *) malloc(edge_count*sizeof(int));
    if (props->edge_lengths == NULL) {
      return (MI_ERROR);
    }
    for (i=0; i< edge_count; i++){
      props->edge_lengths[i] = edge_lengths[i];
    }
  }
  
  return (MI_NOERROR);
}

/** Get blocking structure properties for the volume
 * \param props The properties structure from which to get the information
 * \param edge_count Returns the number of edges (dimensions) in a block
 * \param edge_lengths The lengths of the edges
 * \param max_lengths The number of elements of the edge_lengths array
 * \ingroup mi2VPrp
 */
int miget_props_blocking(mivolumeprops_t props, int *edge_count, int *edge_lengths,
                         int max_lengths)
{
  int i;
  
  if (props == NULL) {
    return (MI_ERROR);
  }
  *edge_count = props->edge_count;
  /* If max_lengths is greater than the actual edge count, reduce max_lengths
   * to the edge_count
   */
  if (max_lengths > props->edge_count) {
    max_lengths = props->edge_count;
  }
  
  /*edge_lengths = (int *) malloc(max_lengths *sizeof(int));*/
  
  for (i=0; i< max_lengths; i++){
    edge_lengths[i] = props->edge_lengths[i];
  }
  
  return (MI_NOERROR);
}

/** Set properties for uniform/nonuniform record dimension
 * \ingroup mi2VPrp
 */
int miset_props_record(mivolumeprops_t props, misize_t record_length, char *record_name)
{
  if (props == NULL) {
    return (MI_ERROR);
  }
  if (record_length > 0) {
    props->record_length = record_length;
  }
  if (props->record_name != NULL) {
    free(props->record_name);
    props->record_name = NULL;
  }
  
  props->record_name = strdup(record_name);
  
  return (MI_NOERROR);
}

/** Set the template volume flag
 * \ingroup mi2VPrp
 */
int miset_props_template(mivolumeprops_t props, int template_flag)
{
  if (props == NULL) {
    return (MI_ERROR);
  }
  
  props->template_flag = template_flag;
  return (MI_NOERROR);
}


int miset_props_checksum(mivolumeprops_t props, int on)
{
  if (props == NULL) {
    return (MI_ERROR);
  }
  props->checksum=on;
  return (MI_NOERROR);
}


int miget_props_checksum(mivolumeprops_t props,int *on)
{
  if (props == NULL) {
    return (MI_ERROR);
  }
  *on=props->checksum;
  return (MI_NOERROR);
}




// kate: indent-mode cstyle; indent-width 2; replace-tabs on; 
