/** \file hyper.c
 * \brief MINC 2.0 Hyperslab Functions
 * \author Bert Vincent
 *
 * Functions to manipulate hyperslabs of volume image data.
 ************************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif //HAVE_CONFIG_H

#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <limits.h>

#include "minc2.h"
#include "minc2_private.h"
#include "restructure.h"

#define MIRW_OP_READ 1
#define MIRW_OP_WRITE 2

#ifndef HAVE_COPYSIGN
double
copysign(double x, double y)
{
    /* use atan2 to distinguish -0. from 0. */
    if (y > 0. || (y == 0. && atan2(y, -1.) > 0.)) {
        return fabs(x);
    } else {
        return -fabs(x);
    }
}
#endif /* HAVE_COPYSIGN */

#ifndef HAVE_ROUND
double
round(double x)
{
    double absx, y;
    absx = fabs(x);
    y = floor(absx);
    if (absx - y >= 0.5)
    y += 1.0;
    return copysign(y, x);
}
#endif /* HAVE_ROUND */

/** Calculates and returns the number of bytes required to store the
 * hyperslab specified by the \a n_dimensions and the
 * \a count parameters, using hdf type id
 */
void miget_hyperslab_size_hdf(hid_t hdf_type_id, int n_dimensions, 
                                const hsize_t count[], 
                                misize_t *size_ptr)
{
  size_t voxel_size;
  misize_t temp;
  int i;
  voxel_size = H5Tget_size(hdf_type_id);

  temp = 1;
  for (i = 0; i < n_dimensions; i++) {
    temp *= count[i];
  }
  *size_ptr = (temp * voxel_size);
}


/** Calculates and returns the number of bytes required to store the
 * hyperslab specified by the \a n_dimensions and the
 * \a count parameters.
 */
int miget_hyperslab_size(mitype_t volume_data_type,   /**< Data type of a voxel. */
                     int n_dimensions,            /**< Dimensionality */
                     const hsize_t count[], /**< Dimension lengths  */
                     misize_t *size_ptr)          /**< Returned byte count */
{
  hid_t type_id;

  type_id = mitype_to_hdftype(volume_data_type, TRUE);
  if (type_id < 0) {
    return (MI_ERROR);
  }
  
  miget_hyperslab_size_hdf(type_id,n_dimensions,count,size_ptr);

  H5Tclose(type_id);
  return (MI_NOERROR);
}

/** "semiprivate" function for translating coordinates.
 */
int mitranslate_hyperslab_origin(mihandle_t volume,
                             const misize_t* start,
                             const misize_t* count,
                             hsize_t* hdf_start,
                             hsize_t* hdf_count,
                             int* dir) /* direction vector in file order */
{
  int n_different = 0;
  int file_i;
  int ndims = volume->number_of_dims;
  int j;
  
  for(j=0; j<ndims; j++) {
    hdf_count[j]=0;
    hdf_start[j]=0;
  }
  for (file_i = 0; file_i < ndims; file_i++) {
    midimhandle_t hdim;
    int user_i;

    /* Set up the basic translations of dimensions, for
     * flipping directions and swapping indices.
     */
    if (volume->dim_indices != NULL) {
      /* Have to swap indices */
      user_i = volume->dim_indices[file_i];
      if (user_i != file_i) {
        n_different++;
      }
    } else {
      user_i = file_i;
    }

    hdim = volume->dim_handles[user_i];
    switch (hdim->flipping_order) {
    case MI_FILE_ORDER:
      hdf_start[user_i] = start[file_i];
      dir[file_i] = 1;    /* Set direction positive */
      break;

    case MI_COUNTER_FILE_ORDER:
      hdf_start[user_i] = hdim->length - start[file_i] - count[file_i];
      dir[file_i] = -1;   /* Set direction negative */
      n_different++;
      break;

    case MI_POSITIVE:
      if (hdim->step >= 0.0) { /* Positive? */
        hdf_start[user_i] = start[file_i];
        dir[file_i] = 1; /* Set direction positive */
      } else {
        hdf_start[user_i] = hdim->length - start[file_i] - count[file_i];
        dir[file_i] = -1; /* Set direction negative */
        n_different++;
      }
      break;

    case MI_NEGATIVE:
      if (hdim->step < 0.0) { /* Negative? */
        hdf_start[user_i] = start[file_i];
        dir[file_i] = 1; /* Set direction positive */
      } else {
        hdf_start[user_i] = hdim->length - start[file_i] - count[file_i];
        dir[file_i] = -1; /* Set direction negative */
        n_different++;
      }
      break;
    }
    hdf_count[user_i] = count[file_i];
  }
  return (n_different);
}

/** Read/write a hyperslab of data.  This is the simplified function
 * which performs no value conversion.  It is much more efficient than
 * mirw_hyperslab_icv()
 */
static int mirw_hyperslab_raw(int opcode,
                              mihandle_t volume,
                              mitype_t midatatype,
                              const misize_t start[],
                              const misize_t count[],
                              void *buffer)
{
  hid_t dset_id = -1;
  hid_t mspc_id = -1;
  hid_t fspc_id = -1;
  hid_t type_id = -1;
  int result = MI_ERROR;
  hsize_t hdf_start[MI2_MAX_VAR_DIMS];
  hsize_t hdf_count[MI2_MAX_VAR_DIMS];
  int dir[MI2_MAX_VAR_DIMS];  /* Direction vector in file order */
  int ndims;
  int n_different = 0;
  misize_t buffer_size;
  void *temp_buffer=NULL;
  char path[MI2_MAX_PATH];
  size_t icount[MI2_MAX_VAR_DIMS];

  /* Disallow write operations to anything but the highest resolution.
   */
  if (opcode == MIRW_OP_WRITE && volume->selected_resolution != 0) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to write to a volume thumbnail");
  }

  sprintf(path, MI_ROOT_PATH "/image/%d/image", volume->selected_resolution);
  /*printf("Using:%s\n",path);*/
  
  /* Open the dataset with the specified path
  */
  MI_CHECK_HDF_CALL(dset_id = H5Dopen1(volume->hdf_id, path),"H5Dopen1");
  if (dset_id < 0) {
    return (MI_ERROR);
  }

  MI_CHECK_HDF_CALL(fspc_id = H5Dget_space(dset_id),"H5Dget_space");
  if (fspc_id < 0) {
    /*TODO: report can't get dataset*/
    goto cleanup;
  }

  if (midatatype == MI_TYPE_UNKNOWN) {
    type_id = H5Tcopy(volume->mtype_id);
  } else {
    type_id = mitype_to_hdftype(midatatype, TRUE);
  }

  ndims = volume->number_of_dims;

  if (ndims == 0) {
    /* A scalar volume is possible but extremely unlikely, not to
     * mention useless!
     */
    mspc_id = H5Screate(H5S_SCALAR);
  } else {

    n_different = mitranslate_hyperslab_origin(volume, start, count, hdf_start, hdf_count, dir);

    MI_CHECK_HDF_CALL(mspc_id = H5Screate_simple(ndims, hdf_count, NULL),"H5Screate_simple");
    if (mspc_id < 0) {
      goto cleanup;
    }
  }

  MI_CHECK_HDF_CALL(result = H5Sselect_hyperslab(fspc_id, H5S_SELECT_SET, hdf_start, NULL,
                               hdf_count, NULL),"H5Sselect_hyperslab");
  if (result < 0) {
    goto cleanup;
  }

  miget_hyperslab_size_hdf(type_id,ndims,hdf_count,&buffer_size);
  
  
  if (opcode == MIRW_OP_READ) {
    MI_CHECK_HDF_CALL(result = H5Dread(dset_id, type_id, mspc_id, fspc_id, H5P_DEFAULT,buffer),"H5Dread");
    
    /* Restructure the array after reading the data in file orientation.
     */
    if (n_different != 0) {
      int i;

      for (i = 0; i < ndims; i++) {
        icount[i] = count[i];
      }
      restructure_array(ndims, buffer, icount, H5Tget_size(type_id),
                        volume->dim_indices, dir);
    }
  } else {

    volume->is_dirty = TRUE; /* Mark as modified. */

    /* Restructure array before writing to file.
     * TODO: use temporary buffer for that!
     */

    if (n_different != 0) {
      int idir[MI2_MAX_VAR_DIMS];
      int imap[MI2_MAX_VAR_DIMS];
      int i;

      /* Invert before calling */
      for (i = 0; i < ndims; i++) {
        icount[volume->dim_indices[i]] = count[i];

        idir[volume->dim_indices[i]] = dir[i];

        // this one was correct the original way
        imap[volume->dim_indices[i]] = i;

      }

      /*Use temporary array to preserve input data*/
      temp_buffer=malloc(buffer_size);
      if(temp_buffer==NULL)
      {
        /*TODO: report memory error*/
        result=MI_ERROR;
        goto cleanup;
      }
      
      memcpy(temp_buffer,buffer,buffer_size);
      
      restructure_array(ndims, temp_buffer, icount, H5Tget_size(type_id),
                        imap, idir);
      MI_CHECK_HDF_CALL(result = H5Dwrite(dset_id, type_id, mspc_id, fspc_id, H5P_DEFAULT,
                      temp_buffer),"H5Dwrite");
    } else {
      MI_CHECK_HDF_CALL(result = H5Dwrite(dset_id, type_id, mspc_id, fspc_id, H5P_DEFAULT,
                        buffer),"H5Dwrite");
    }

  }

cleanup:

  if (type_id >= 0) {
    H5Tclose(type_id);
  }
  if (mspc_id >= 0) {
    H5Sclose(mspc_id);
  }
  if (fspc_id >= 0) {
    H5Sclose(fspc_id);
  }
  if ( dset_id >=0 ) {
    H5Dclose(dset_id);
  }
  if ( temp_buffer!= NULL) {
    free( temp_buffer );
  }
  return (result);
}

#define APPLY_DESCALING(type_in,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,voxel_min,voxel_max) \
  { \
    hsize_t _i,_j;\
    type_in *_buffer=(type_in *)buffer;\
    for(_i=0;_i<total_number_of_slices;_i++)\
    {\
      double _scale=(image_slice_max_buffer[_i]-image_slice_min_buffer[_i])/(voxel_max-voxel_min); \
      double _offset=image_slice_min_buffer[_i]-voxel_min*_scale; \
      for(_j=0;_j<image_slice_length;_j++)\
      {\
        double _temp;\
        _temp=(double)*_buffer;\
        _temp=_temp*_scale  + _offset ; \
        *_buffer=(type_in)(_temp); \
        _buffer++;\
      }\
    }\
  }

#define APPLY_SCALING(type_in,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,voxel_min,voxel_max) \
  { \
    hsize_t _i,_j;\
    type_in *_buffer=(type_in *)buffer;\
    for(_i=0;_i<total_number_of_slices;_i++)\
    {\
      double _scale= (voxel_max-voxel_min)/(image_slice_max_buffer[_i]-image_slice_min_buffer[_i]); \
      double _offset= image_slice_min_buffer[_i]*_scale-voxel_min; \
      for(_j=0;_j<image_slice_length;_j++)\
      {\
        double _temp;\
        _temp=(double)(*_buffer);\
        _temp= rint(_temp *_scale - _offset); \
        *_buffer = (type_in)(_temp); \
        _buffer++;\
      }\
    }\
  }

/** Read/write a hyperslab of data, performing dimension remapping
 * and data rescaling as needed.
 */
static int mirw_hyperslab_icv(int opcode,
                              mihandle_t volume,
                              mitype_t buffer_data_type,
                              const misize_t start[],
                              const misize_t count[],
                              void *buffer)
{
  hid_t dset_id = -1;
  hid_t mspc_id = -1;
  hid_t fspc_id = -1;
  hid_t buffer_type_id = -1;
  int result = MI_ERROR;
  hsize_t hdf_start[MI2_MAX_VAR_DIMS];
  hsize_t hdf_count[MI2_MAX_VAR_DIMS];
  int dir[MI2_MAX_VAR_DIMS];  /* Direction vector in file order */
  hsize_t ndims;
  int slice_ndims;
  int n_different = 0;
  double volume_valid_min, volume_valid_max;
  misize_t buffer_size;
  void *temp_buffer=NULL;
  size_t icount[MI2_MAX_VAR_DIMS];
  int idir[MI2_MAX_VAR_DIMS];
  int imap[MI2_MAX_VAR_DIMS];
  double *image_slice_max_buffer=NULL;
  double *image_slice_min_buffer=NULL;
  int scaling_needed=0;
  char path[MI2_MAX_PATH];
  
  hsize_t image_slice_start[MI2_MAX_VAR_DIMS];
  hsize_t image_slice_count[MI2_MAX_VAR_DIMS];
  hsize_t image_slice_length=0;
  hsize_t total_number_of_slices=0;
  hsize_t i;
  int j;


  /* Disallow write operations to anything but the highest resolution.
   */
  if (opcode == MIRW_OP_WRITE && volume->selected_resolution != 0) {
    return MI_LOG_ERROR(MI2_MSG_GENERIC,"Trying to write to a volume thumbnail");
  }
  
  sprintf(path, MI_ROOT_PATH "/image/%d/image", volume->selected_resolution);
  /*printf("Using:%s\n",path);*/
  
  /* Open the dataset with the specified path
  */
  MI_CHECK_HDF_CALL(dset_id = H5Dopen1(volume->hdf_id, path),"H5Dopen1");
  if (dset_id < 0) {
    return (MI_ERROR);
  }

  MI_CHECK_HDF_CALL(fspc_id = H5Dget_space(dset_id),"H5Dget_space");
  if (fspc_id < 0) {
    goto cleanup;
  }

 
  buffer_type_id = mitype_to_hdftype(buffer_data_type, TRUE);
  if(buffer_type_id<0)
  {
    goto cleanup;
  }
  
  ndims = volume->number_of_dims;
  
  if (ndims == 0) {
    /* A scalar volume is possible but extremely unlikely, not to
     * mention useless!
     */
    mspc_id = H5Screate(H5S_SCALAR);
    hdf_count[0]=1; 
  } else {

    n_different = mitranslate_hyperslab_origin(volume, start, count, hdf_start, hdf_count, dir);

    mspc_id = H5Screate_simple(ndims, hdf_count, NULL);
    
    if (mspc_id < 0) {
      fprintf(stderr,"H5Screate_simple: Fail %s:%d\n",__FILE__,__LINE__);
      goto cleanup;
    }
  }
  
  miget_hyperslab_size_hdf(buffer_type_id, ndims, hdf_count, &buffer_size);

  MI_CHECK_HDF_CALL(result = H5Sselect_hyperslab(fspc_id, H5S_SELECT_SET, hdf_start, NULL,
                               hdf_count, NULL),"H5Sselect_hyperslab");
  if (result < 0) {
    goto cleanup;
  }

  if((result=miget_volume_valid_range( volume, &volume_valid_max, &volume_valid_min))<0)
  {
    goto cleanup;
  }

#ifdef _DEBUG
  printf("mirw_hyperslab_icv:Volume:%lx valid_max:%f valid_min:%f scaling:%d n_different:%d\n",(long int)(volume),volume_valid_max,volume_valid_min,volume->has_slice_scaling,n_different);
#endif  
  
  if(volume->has_slice_scaling)
  {
    hid_t image_max_fspc_id;
    hid_t image_min_fspc_id;
    hid_t scaling_mspc_id;
    total_number_of_slices=1;
    image_slice_length=1;
    scaling_needed=1;

    image_max_fspc_id=H5Dget_space(volume->imax_id);
    image_min_fspc_id=H5Dget_space(volume->imin_id);

    if ( image_max_fspc_id < 0 ) {
      /*Report error that image-max is not found!*/
      return ( MI_ERROR );
    }

    slice_ndims = H5Sget_simple_extent_ndims ( image_max_fspc_id );
    if(slice_ndims<0)
    {
      /*TODO: report read error somehow*/
      fprintf(stderr,"H5Sget_simple_extent_ndims: Fail %s:%d\n",__FILE__,__LINE__);
      goto cleanup;
    }

    if ( (hsize_t)slice_ndims > ndims ) { /*Can this really happen?*/
      slice_ndims = ndims;
    }

    for ( j = 0; j < slice_ndims; j++ ) {
      image_slice_count[j] = hdf_count[j];
      image_slice_start[j] = hdf_start[j];
      
      if(hdf_count[j]>1) /*avoid zero sized dimensions?*/
        total_number_of_slices*=hdf_count[j];
    }
    
    for (i = slice_ndims; i < ndims; i++ ) {
      if(hdf_count[i]>1) /*avoid zero sized dimensions?*/
        image_slice_length*=hdf_count[i];
      
      image_slice_count[i] = 0;
      image_slice_start[i] = 0;
    }
    
    image_slice_max_buffer=malloc(total_number_of_slices*sizeof(double));
    if(!image_slice_max_buffer)
    {
      result=MI_ERROR;
      MI_LOG_ERROR(MI2_MSG_OUTOFMEM,total_number_of_slices*sizeof(double));
      goto cleanup;
    }
    
    image_slice_min_buffer=malloc(total_number_of_slices*sizeof(double));
    
    if(!image_slice_min_buffer)
    {
      result=MI_ERROR;
      MI_LOG_ERROR(MI2_MSG_OUTOFMEM,total_number_of_slices*sizeof(double));
      goto cleanup;
    }
    
    scaling_mspc_id = H5Screate_simple(slice_ndims, image_slice_count, NULL);
    
    if( (result=H5Sselect_hyperslab(image_max_fspc_id, H5S_SELECT_SET, image_slice_start, NULL, image_slice_count, NULL))>=0 )
    {
      if((result=H5Dread(volume->imax_id, H5T_NATIVE_DOUBLE, scaling_mspc_id, image_max_fspc_id, H5P_DEFAULT,image_slice_max_buffer))<0)
      {
        MI_LOG_ERROR(MI2_MSG_HDF5,"H5Dread");
        goto cleanup;
      }
    } else {
      MI_LOG_ERROR(MI2_MSG_HDF5,"H5Sselect_hyperslab");
      goto cleanup;
    }
    
    if((result=H5Sselect_hyperslab(image_min_fspc_id, H5S_SELECT_SET, image_slice_start, NULL, image_slice_count, NULL))>=0 )
    {
      if((result=H5Dread(volume->imin_id, H5T_NATIVE_DOUBLE, scaling_mspc_id, image_min_fspc_id, H5P_DEFAULT,image_slice_min_buffer))<0)
      {
        MI_LOG_ERROR(MI2_MSG_HDF5,"H5Dread");
        goto cleanup;
      }
    } else {
      /*TODO: report read error somehow*/
      MI_LOG_ERROR(MI2_MSG_HDF5,"H5Sselect_hyperslab");
      goto cleanup;
    }
    H5Sclose(scaling_mspc_id);
    H5Sclose(image_max_fspc_id);
    H5Sclose(image_min_fspc_id);
  } else {
    slice_ndims=0;
    total_number_of_slices=1;
    image_slice_max_buffer=malloc(sizeof(double));
    image_slice_min_buffer=malloc(sizeof(double));
    miget_volume_range(volume,image_slice_max_buffer,image_slice_min_buffer);
    image_slice_length=1;
    /*it produces unity scaling*/
    scaling_needed=(*image_slice_max_buffer!=volume_valid_max) || (*image_slice_min_buffer!=volume_valid_min);
    for (i = 0; i < ndims; i++) {
      image_slice_length *= hdf_count[i];
    }
#ifdef _DEBUG    
    printf("mirw_hyperslab_icv:Real max:%f min:%f\n",*image_slice_max_buffer,*image_slice_min_buffer);
#endif    
  }
  
  //A hack to disable interslice scaling when it is not needed according to MINC1 specs
  if( volume->volume_type==MI_TYPE_FLOAT    || volume->volume_type==MI_TYPE_DOUBLE || 
      volume->volume_type==MI_TYPE_FCOMPLEX || volume->volume_type==MI_TYPE_DCOMPLEX )
  {
    scaling_needed=0;
  } 
#ifdef _DEBUG  
  printf("mirw_hyperslab_icv:Slice_ndim:%d total_number_of_slices:%d image_slice_length:%d scaling_needed:%d\n",slice_ndims,total_number_of_slices,image_slice_length,scaling_needed);
#endif

  if (opcode == MIRW_OP_READ) 
  {
    MI_CHECK_HDF_CALL(result = H5Dread(dset_id, buffer_type_id, mspc_id, fspc_id, H5P_DEFAULT, buffer),"H5Dread");
    if(result<0)
    {
      goto cleanup;
    }
    
    if(scaling_needed)
    {
      switch(buffer_data_type)
      {
        case MI_TYPE_FLOAT:
#ifdef _DEBUG  
          printf("Descaling  float\n");
#endif
          APPLY_DESCALING(float,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_DOUBLE:
#ifdef _DEBUG  
          printf("Descaling  double\n");
#endif
          APPLY_DESCALING(double,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_INT:
#ifdef _DEBUG  
          printf("Descaling  int\n");
#endif
          APPLY_DESCALING(int,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_UINT:
#ifdef _DEBUG  
          printf("Descaling  uint\n");
#endif
          APPLY_DESCALING(unsigned int,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_SHORT:
#ifdef _DEBUG  
          printf("Descaling  short\n");
#endif
          APPLY_DESCALING(short,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_USHORT:
#ifdef _DEBUG  
          printf("Descaling  ushort\n");
#endif
          APPLY_DESCALING(unsigned short,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_BYTE:
#ifdef _DEBUG  
          printf("Descaling  byte\n");
#endif
          APPLY_DESCALING(char,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        case MI_TYPE_UBYTE:
#ifdef _DEBUG  
          printf("Descaling  ubyte\n");
#endif
          APPLY_DESCALING(unsigned char,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
          break;
        default:
          /*TODO: report unsupported conversion*/
          result=MI_ERROR;
          goto cleanup;
      } 
    } else {
#ifdef _DEBUG  
      printf("Descaling  not needed!\n");
#endif
    }
    
    if (n_different != 0 ) {
      for (i = 0; i < ndims; i++) {
        icount[i] = count[i];
      }
      restructure_array(ndims, buffer, icount, H5Tget_size(buffer_type_id),volume->dim_indices, dir);
      /*TODO: check if we managed to restructure the array*/
      result=0;
    }
  } else { /*opcode != MIRW_OP_READ*/

    volume->is_dirty = TRUE; /* Mark as modified. */
    
    if (n_different != 0 ) {
      /* Invert before calling */
      for (i = 0; i < ndims; i++) {
        icount[volume->dim_indices[i]] = count[i];
        idir[volume->dim_indices[i]] = dir[i];
        /* this one was correct the original way*/
        imap[volume->dim_indices[i]] = i;

      }
    }
    if(scaling_needed || n_different != 0) 
    {
      /*create temporary copy, to be destroyed*/
      temp_buffer=malloc(buffer_size);
      if(!temp_buffer)
      {
        MI_LOG_ERROR(MI2_MSG_OUTOFMEM,buffer_size);
        result=MI_ERROR; /*TODO: error code?*/
        goto cleanup;
      }
      memcpy(temp_buffer,buffer,buffer_size);

      if (n_different != 0 )
        restructure_array(ndims, temp_buffer, icount, H5Tget_size(buffer_type_id), imap, idir);

      if(scaling_needed)
      {
        switch(buffer_data_type)
        {
          case MI_TYPE_FLOAT:
#ifdef _DEBUG  
            printf("scaling  float\n");
#endif
            APPLY_SCALING(float,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_DOUBLE:
#ifdef _DEBUG  
            printf("scaling  double\n");
#endif
            APPLY_SCALING(double,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_INT:
#ifdef _DEBUG  
            printf("scaling  int\n");
#endif
            APPLY_SCALING(int,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_UINT:
#ifdef _DEBUG  
            printf("scaling  unsigned int\n");
#endif
            APPLY_SCALING(unsigned int,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_SHORT:
#ifdef _DEBUG  
            printf("scaling  short\n");
#endif
            APPLY_SCALING(short,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_USHORT:
#ifdef _DEBUG  
            printf("scaling  unsigned short\n");
#endif
            APPLY_SCALING(unsigned short,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_BYTE:
#ifdef _DEBUG
            printf("scaling  char\n");
#endif
            APPLY_SCALING(char,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          case MI_TYPE_UBYTE:
#ifdef _DEBUG
            printf("scaling  unsigned char\n");
#endif
            APPLY_SCALING(unsigned char,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max);
            break;
          default:
            /*TODO: report unsupported conversion*/
            result=MI_ERROR;
            goto cleanup;
        }
      }
      MI_CHECK_HDF_CALL(result = H5Dwrite(dset_id, buffer_type_id, mspc_id, fspc_id, H5P_DEFAULT, temp_buffer),"H5Dwrite");
    } else {
      MI_CHECK_HDF_CALL(result = H5Dwrite(dset_id, buffer_type_id, mspc_id, fspc_id, H5P_DEFAULT, buffer),"H5Dwrite");
    }
    
    if(result<0)
    {
      goto cleanup;
    }
  }
      
cleanup:

  if (buffer_type_id >= 0) {
    H5Tclose(buffer_type_id);
  }
  if (mspc_id >= 0) {
    H5Sclose(mspc_id);
  }
  if (fspc_id >= 0) {
    H5Sclose(fspc_id);
  }
  if ( dset_id >=0 ) {
    H5Dclose(dset_id);
  }
  
  if(temp_buffer!=NULL)
  {
    free(temp_buffer);
  }
  if(image_slice_min_buffer!=NULL)
  {
    free(image_slice_min_buffer);
  }
  if(image_slice_max_buffer!=NULL)
  {
    free(image_slice_max_buffer);
  }
  return (result);
}

#define APPLY_DESCALING_NORM(type_out,buffer_in,buffer_out,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,voxel_min,voxel_max,data_min,data_max,norm_min,norm_max) \
  { \
    hsize_t _i,_j;\
    double voxel_offset=voxel_min;\
    double voxel_range=voxel_max-voxel_min;\
    double norm_offset=(double)norm_min;\
    double norm_range=(double)norm_max-(double)norm_min;\
    double data_offset=data_min;\
    double data_range=(double)data_max-(double)data_min;\
    type_out *_buffer_out=(type_out *)buffer_out;\
    double *_buffer_in=(double *)buffer_in; \
    for(_i=0;_i<total_number_of_slices;_i++)\
      for(_j=0;_j<image_slice_length;_j++)\
      {\
        double _temp=(( (*_buffer_in) - voxel_offset) / voxel_range)*(image_slice_max_buffer[_i]-image_slice_min_buffer[_i]) + image_slice_min_buffer[_i] ;\
        _temp=(_temp-data_offset)/data_range;\
        _temp=(_temp<0.0)?norm_min:(_temp>=1.0)?norm_max:(rint(_temp*norm_range)+norm_offset); \
        *_buffer_out=(type_out)(_temp);\
        _buffer_in++;\
        _buffer_out++;\
      }\
  }


#define APPLY_SCALING_NORM(type_in,buffer_in,buffer_out,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,voxel_min,voxel_max,data_min,data_max,norm_min,norm_max) \
  { \
    hsize_t _i,_j;\
    double voxel_range=voxel_max-voxel_min;\
    double voxel_offset=voxel_min;\
    double norm_offset=norm_min;\
    double norm_range=(double)norm_max-(double)norm_min;\
    double data_offset=data_min;\
    double data_range=(double)data_max-(double)data_min;\
    type_in *_buffer_in=(type_in *)buffer_in;\
    double *_buffer_out=buffer_out;\
    for(_i=0;_i<total_number_of_slices;_i++)\
      for(_j=0;_j<image_slice_length;_j++)\
      {\
        double _temp=((double)(*_buffer_in)-norm_offset)/norm_range;\
        _temp=(_temp*data_range)+data_offset;\
        _temp=(((_temp - image_slice_min_buffer[_i])/(image_slice_max_buffer[_i]-image_slice_min_buffer[_i]))*voxel_range + voxel_offset);\
        *_buffer_out=round(_temp);\
        _buffer_out++;\
        _buffer_in++;\
      }\
  }



/** Read/write a hyperslab of data, performing dimension remapping
 * and data rescaling as needed. Data in the range (min-max) will map to the appropriate full range of buffer_data_type
 */
static int mirw_hyperslab_normalized(int opcode,
                              mihandle_t volume,
                              mitype_t buffer_data_type,
                              const misize_t start[],
                              const misize_t count[],
                              double data_min,
                              double data_max,
                              void *buffer)
{
  hid_t dset_id = -1;
  hid_t mspc_id = -1;
  hid_t fspc_id = -1;
  hid_t volume_type_id = -1;
  hid_t buffer_type_id = -1;
  int result = MI_ERROR;
  hsize_t hdf_start[MI2_MAX_VAR_DIMS];
  hsize_t hdf_count[MI2_MAX_VAR_DIMS];
  int dir[MI2_MAX_VAR_DIMS];  /* Direction vector in file order */
  hsize_t ndims;
  int slice_ndims;
  int n_different = 0;
  double volume_valid_min, volume_valid_max;
  misize_t buffer_size;
  misize_t input_buffer_size;
  double *temp_buffer=NULL;
  size_t icount[MI2_MAX_VAR_DIMS];
  int idir[MI2_MAX_VAR_DIMS];
  int imap[MI2_MAX_VAR_DIMS];
  double *image_slice_max_buffer=NULL;
  double *image_slice_min_buffer=NULL;

  char path[MI2_MAX_PATH];
  
  hsize_t image_slice_start[MI2_MAX_VAR_DIMS];
  hsize_t image_slice_count[MI2_MAX_VAR_DIMS];
  hsize_t image_slice_length=0;
  hsize_t total_number_of_slices=0;
  hsize_t i;
  int j;


  /* Disallow write operations to anything but the highest resolution.
   */
  if (opcode == MIRW_OP_WRITE && volume->selected_resolution != 0) {
    /*TODO: report error that we are not dealing with the rihgt image here*/
    return (MI_ERROR);
  }
  
  sprintf(path, MI_ROOT_PATH "/image/%d/image", volume->selected_resolution);

  /* Open the dataset with the specified path
  */
  MI_CHECK_HDF_CALL(dset_id = H5Dopen1(volume->hdf_id, path),"H5Dopen1");
  if (dset_id < 0) {
    return (MI_ERROR);
  }

  MI_CHECK_HDF_CALL(fspc_id = H5Dget_space(dset_id),"H5Dget_space");
  if (fspc_id < 0) {
    /*TODO: report can't get dataset*/
    goto cleanup;
  }
  buffer_type_id = mitype_to_hdftype(buffer_data_type,TRUE);
  if(buffer_type_id<0)
  {
    goto cleanup;
  }
  
  MI_CHECK_HDF_CALL(volume_type_id = H5Tcopy ( H5T_NATIVE_DOUBLE ),"H5Tcopy");
  if(volume_type_id<0)
  {
    fprintf(stderr,"H5Tcopy: Fail %s:%d\n",__FILE__,__LINE__);
    goto cleanup;
  }
  
  ndims = volume->number_of_dims;
  
  if (ndims == 0) {
    /* A scalar volume is possible but extremely unlikely, not to
     * mention useless!
     */
    mspc_id = H5Screate(H5S_SCALAR);
  } else {

    n_different = mitranslate_hyperslab_origin(volume,start,count, hdf_start,hdf_count,dir);

    MI_CHECK_HDF_CALL(mspc_id = H5Screate_simple(ndims, hdf_count, NULL),"H5Screate_simple");
    
    if (mspc_id < 0) {
      goto cleanup;
    }
  }
  
  miget_hyperslab_size_hdf(volume_type_id,ndims,hdf_count,&buffer_size);
  miget_hyperslab_size_hdf(buffer_type_id,ndims,hdf_count,&input_buffer_size);

  MI_CHECK_HDF_CALL(result = H5Sselect_hyperslab(fspc_id, H5S_SELECT_SET, hdf_start, NULL,
                               hdf_count, NULL),"H5Sselect_hyperslab");
  if (result < 0) {
    goto cleanup;
  }

  miget_volume_valid_range( volume, &volume_valid_max, &volume_valid_min);

#ifdef _DEBUG
  printf("mirw_hyperslab_normalized:Volume:%x valid_max:%f valid_min:%f scaling:%d\n",volume,volume_valid_max,volume_valid_min,volume->has_slice_scaling);
#endif  
  
  if(volume->has_slice_scaling && 
    !(volume->volume_type==MI_TYPE_FLOAT    || volume->volume_type==MI_TYPE_DOUBLE || 
      volume->volume_type==MI_TYPE_FCOMPLEX || volume->volume_type==MI_TYPE_DCOMPLEX) )
  {
    hid_t image_max_fspc_id;
    hid_t image_min_fspc_id;
    hid_t scaling_mspc_id;
    total_number_of_slices=1;
    image_slice_length=1;

    MI_CHECK_HDF_CALL(image_max_fspc_id=H5Dget_space(volume->imax_id),"H5Dget_space");
    MI_CHECK_HDF_CALL(image_min_fspc_id=H5Dget_space(volume->imin_id),"H5Dget_space");

    if ( image_max_fspc_id < 0 || image_min_fspc_id<0 ) {
      result=MI_ERROR;
      goto cleanup;
    }

    MI_CHECK_HDF_CALL(slice_ndims = H5Sget_simple_extent_ndims ( image_max_fspc_id ),"H5Sget_simple_extent_ndims");
    if(slice_ndims<0)
    {
      goto cleanup;
    }

    if ( (hsize_t)slice_ndims > ndims ) { /*Can this really happen?*/
      slice_ndims = ndims;
    }

    for ( j = 0; j < slice_ndims; j++ ) {
      image_slice_count[j] = hdf_count[j];
      image_slice_start[j] = hdf_start[j];
      
      if(hdf_count[j]>1) /*avoid zero sized dimensions?*/
        total_number_of_slices*=hdf_count[j];
    }
    
    for (i = slice_ndims; i < ndims; i++ ) {
      if(hdf_count[i]>1) /*avoid zero sized dimensions?*/
        image_slice_length*=hdf_count[i];
      
      image_slice_count[i] = 0;
      image_slice_start[i] = 0;
    }
    
    image_slice_max_buffer=malloc(total_number_of_slices*sizeof(double));
    image_slice_min_buffer=malloc(total_number_of_slices*sizeof(double));
    /*TODO check for allocation failure ?*/
    
    MI_CHECK_HDF_CALL(scaling_mspc_id = H5Screate_simple(slice_ndims, image_slice_count, NULL),"H5Screate_simple");
    
    if( (result=H5Sselect_hyperslab(image_max_fspc_id, H5S_SELECT_SET, image_slice_start, NULL, image_slice_count, NULL))>=0 )
    {
      if( ( result=H5Dread(volume->imax_id, H5T_NATIVE_DOUBLE, scaling_mspc_id, image_max_fspc_id, H5P_DEFAULT,image_slice_max_buffer))<0)
      {
        MI_LOG_ERROR(MI2_MSG_HDF5,"H5Dread");
        goto cleanup;
      }
    } else {
      MI_LOG_ERROR(MI2_MSG_HDF5,"H5Sselect_hyperslab");
      goto cleanup;
    }
    
    if( (result=H5Sselect_hyperslab(image_min_fspc_id, H5S_SELECT_SET, image_slice_start, NULL, image_slice_count, NULL))>=0 )
    {
      if( (result=H5Dread(volume->imin_id, H5T_NATIVE_DOUBLE, scaling_mspc_id, image_min_fspc_id, H5P_DEFAULT,image_slice_min_buffer))<0)
      {
        MI_LOG_ERROR(MI2_MSG_HDF5,"H5Dread");
        goto cleanup;
      }
    } else {
      MI_LOG_ERROR(MI2_MSG_HDF5,"H5Sselect_hyperslab");
      goto cleanup;
    }
    H5Sclose(scaling_mspc_id);
    H5Sclose(image_max_fspc_id);
    
  } else {
    slice_ndims=0;
    total_number_of_slices=1;
    image_slice_max_buffer=malloc(sizeof(double));
    image_slice_min_buffer=malloc(sizeof(double));
    miget_volume_range( volume,image_slice_max_buffer,image_slice_min_buffer );
    image_slice_length=1;
    for (i = 0; i < ndims; i++) {
      image_slice_length *= hdf_count[i];
    }
#ifdef _DEBUG
    printf("mirw_hyperslab_normalized:Real max:%f min:%f\n",*image_slice_max_buffer,*image_slice_min_buffer);
#endif
  }

#ifdef _DEBUG  
  printf("mirw_hyperslab_normalized:Slice_ndim:%d total_number_of_slices:%d image_slice_length:%d\n",slice_ndims,total_number_of_slices,image_slice_length);
  printf("mirw_hyperslab_normalized:data min:%f data max:%f buffer_data_type:%d\n",data_min,data_max,buffer_data_type);
#endif

  /*Allocate temporary Buffer*/
  temp_buffer=(double*)malloc(buffer_size);
  if(!temp_buffer)
  {
    MI_LOG_ERROR(MI2_MSG_OUTOFMEM,buffer_size);
    result=MI_ERROR;
    goto cleanup;
  }
  
  if (opcode == MIRW_OP_READ) 
  {
    MI_CHECK_HDF_CALL(result = H5Dread(dset_id, volume_type_id, mspc_id, fspc_id, H5P_DEFAULT, temp_buffer),"H5Dread");
    if(result<0)
    {
      goto cleanup;
    }
    
    /*WARNING: floating point types will be normalized between 0.0 and 1.0*/
    switch(buffer_data_type)
    {
      case MI_TYPE_FLOAT:
        APPLY_DESCALING_NORM(float,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0.0f,1.0f);
        break;
      case MI_TYPE_DOUBLE:
        APPLY_DESCALING_NORM(double,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0.0,1.0);
        break;
      case MI_TYPE_INT:
        APPLY_DESCALING_NORM(int,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,INT_MIN,INT_MAX);
        break;
      case MI_TYPE_UINT:
        APPLY_DESCALING_NORM(unsigned int,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0,UINT_MAX);
        break;
      case MI_TYPE_SHORT:
        APPLY_DESCALING_NORM(short,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,SHRT_MIN,SHRT_MAX);
        break;
      case MI_TYPE_USHORT:
        APPLY_DESCALING_NORM(unsigned short,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0,USHRT_MAX);
        break;
      case MI_TYPE_BYTE:
        APPLY_DESCALING_NORM(char,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,SCHAR_MIN,SCHAR_MAX);
        break;
      case MI_TYPE_UBYTE:
        APPLY_DESCALING_NORM(unsigned char,temp_buffer,buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0,UCHAR_MAX);
        break;
      default:
        /*TODO: report unsupported conversion*/
        result=MI_ERROR;
        goto cleanup;
    }
    
    if (n_different != 0 ) {
      for (i = 0; i < ndims; i++) {
         icount[i] = count[i];
      }
      restructure_array(ndims, buffer, icount, H5Tget_size(buffer_type_id),volume->dim_indices, dir);
      /*TODO: check if we managed to restructure the array*/
      result=0;
    }
  } else { /*opcode != MIRW_OP_READ*/
    void *temp_buffer2;
    volume->is_dirty = TRUE; /* Mark as modified. */
    
    if (n_different != 0 ) {
      /* Invert before calling */
      for (i = 0; i < ndims; i++) {
        icount[volume->dim_indices[i]] = count[i];
        idir[volume->dim_indices[i]] = dir[i];
        /* this one was correct the original way*/
        imap[volume->dim_indices[i]] = i;

      }
    }
    
    /*create temporary copy, to be destroyed*/
    temp_buffer2=malloc(input_buffer_size);
    if(!temp_buffer2)
    {
      MI_LOG_ERROR(MI2_MSG_OUTOFMEM,input_buffer_size);
      result=MI_ERROR; /*TODO: error code?*/
      goto cleanup;
    }
    memcpy(temp_buffer2,buffer,input_buffer_size);
    
    if (n_different != 0 ) 
      restructure_array(ndims, temp_buffer2, icount, H5Tget_size(buffer_type_id), imap, idir);
    
    switch(buffer_data_type)
    {
      case MI_TYPE_FLOAT:
        APPLY_SCALING_NORM(float,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0.0,1.0);
        break;
      case MI_TYPE_DOUBLE:
        APPLY_SCALING_NORM(double,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0.0,1.0);
        break;
      case MI_TYPE_INT:
        APPLY_SCALING_NORM(int,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,(double)INT_MIN,(double)INT_MAX);
        break;
      case MI_TYPE_UINT:
        APPLY_SCALING_NORM(unsigned int,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0,UINT_MAX);
        break;
      case MI_TYPE_SHORT:
        APPLY_SCALING_NORM(short,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,SHRT_MIN,SHRT_MAX);
        break;
      case MI_TYPE_USHORT:
        APPLY_SCALING_NORM(unsigned short,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0,USHRT_MAX);
        break;
      case MI_TYPE_BYTE:
        APPLY_SCALING_NORM(char,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,SCHAR_MIN,SCHAR_MAX);
        break;
      case MI_TYPE_UBYTE:
        APPLY_SCALING_NORM(unsigned char,temp_buffer2,temp_buffer,image_slice_length,total_number_of_slices,image_slice_min_buffer,image_slice_max_buffer,volume_valid_min,volume_valid_max,data_min,data_max,0,UCHAR_MAX);
        break;
      default:
        /*TODO: report unsupported conversion*/
        free(temp_buffer2);
        result=MI_ERROR;
        goto cleanup;
    }
    free(temp_buffer2);
    
    MI_CHECK_HDF_CALL(result = H5Dwrite(dset_id, volume_type_id, mspc_id, fspc_id, H5P_DEFAULT, temp_buffer),"H5Dwrite");
    if(result<0)
    {
      goto cleanup;
    }
  }
      
cleanup:

  if (volume_type_id >= 0) {
    H5Tclose(volume_type_id);
  }
  if (buffer_type_id >= 0) {
    H5Tclose(buffer_type_id);
  }
  if (mspc_id >= 0) {
    H5Sclose(mspc_id);
  }
  if (fspc_id >= 0) {
    H5Sclose(fspc_id);
  }
  if ( dset_id >=0 ) {
    H5Dclose(dset_id);
  }
  if(temp_buffer!=NULL)
  {
    free(temp_buffer);
  }
  if(image_slice_min_buffer!=NULL)
  {
    free(image_slice_min_buffer);
  }
  if(image_slice_max_buffer!=NULL)
  {
    free(image_slice_max_buffer);
  }
  return (result);
}


/** Reads the real values in the volume from the interval min through
 *  max, mapped to the maximum representable range for the requested
 *  data type. Float types is mapped to 0.0 1.0
 */
int miget_hyperslab_normalized(mihandle_t volume,
                               mitype_t buffer_data_type,
                               const misize_t start[],
                               const misize_t count[],
                               double data_min,
                               double data_max,
                               void *buffer)
{

    return mirw_hyperslab_normalized(MIRW_OP_READ, volume, buffer_data_type, 
                                     start, count, data_min, data_max, buffer);
}

/** Writes the real values in the volume from the interval min through
 *  max, mapped to the maximum representable range for the requested
 *  data type. Float types is mapped to 0.0 1.0
 */
int miset_hyperslab_normalized(mihandle_t volume,
                               mitype_t buffer_data_type,
                               const misize_t start[],
                               const misize_t count[],
                               double data_min,
                               double data_max,
                               void *buffer)
{
    return mirw_hyperslab_normalized(MIRW_OP_WRITE, volume, buffer_data_type, 
                                     start, count, data_min, data_max, buffer);
}


/** Get a hyperslab from the file, 
 * converting voxel values into real values
 */
int miget_hyperslab_with_icv(mihandle_t volume,           /**< A MINC 2.0 volume handle */
                             mitype_t buffer_data_type,   /**< Output datatype */
                             const misize_t start[], /**< Start coordinates  */
                             const misize_t count[], /**< Lengths of edges  */
                             void *buffer)                /**< Output memory buffer */
{
  return mirw_hyperslab_icv(MIRW_OP_READ, volume, buffer_data_type, start, count,buffer);
}

/** Write a hyperslab to the file, converting real values into voxel values
 */
int miset_hyperslab_with_icv(mihandle_t volume,        /**< A MINC 2.0 volume handle */
                         mitype_t buffer_data_type,    /**< Output datatype */
                         const misize_t start[],       /**< Start coordinates  */
                         const misize_t count[],       /**< Lengths of edges  */
                         void *buffer)                 /**< Output memory buffer */
{
  return  mirw_hyperslab_icv(MIRW_OP_WRITE,volume,buffer_data_type,start,count,buffer);
}

/** Read a hyperslab from the file into the preallocated buffer,
 *  converting from the stored "voxel" data range to the desired
 * "real" (float or double) data range.
 */
int miget_real_value_hyperslab(mihandle_t volume,       /**< A MINC 2.0 volume handle */
                           mitype_t buffer_data_type,   /**< Output datatype    */
                           const misize_t start[], /**< Start coordinates  */
                           const misize_t count[], /**< Lengths of edges   */
                           void *buffer)                /**< Output memory buffer */ 
{

  return mirw_hyperslab_icv(MIRW_OP_READ,
                              volume,
                              buffer_data_type,
                              start,
                              count,
                              (void *) buffer);
}

/** Write a hyperslab to the file from the preallocated buffer,
 *  converting from the stored "voxel" data range to the desired
 * "real" (float or double) data range.
 */
int miset_real_value_hyperslab(mihandle_t volume,
                           mitype_t buffer_data_type,
                           const misize_t start[],
                           const misize_t count[],
                           void *buffer)
{
  return mirw_hyperslab_icv(MIRW_OP_WRITE,
                                volume,
                                buffer_data_type,
                                start,
                                count,
                                (void *) buffer);
}

/** Read a hyperslab from the file into the preallocated buffer,
 * with no range conversions or normalization.  Type conversions will
 * be performed if necessary.
 */
int miget_voxel_value_hyperslab(mihandle_t volume,
                            mitype_t buffer_data_type,
                            const misize_t start[],
                            const misize_t count[],
                            void *buffer)
{
  return mirw_hyperslab_raw(MIRW_OP_READ, volume, buffer_data_type,
                            start, count, buffer);
}

/** Write a hyperslab to the file from the preallocated buffer,
 * with no range conversions or normalization.  Type conversions will
 * be performed if necessary.
 */
int miset_voxel_value_hyperslab(mihandle_t volume,
                            mitype_t buffer_data_type,
                            const misize_t start[],
                            const misize_t count[],
                            void *buffer)
{
  return mirw_hyperslab_raw(MIRW_OP_WRITE, volume, buffer_data_type,
                            start, count, (void *) buffer);
}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
