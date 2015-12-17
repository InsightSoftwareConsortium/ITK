/**
 * \file Reader for NIfTI-1 format files.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "input_nifti.h"

#include "nifti1.h"
#include "nifti1_io.h"

#define NUM_BYTE_VALUES (UCHAR_MAX + 1)

/**
 * Scan through the file to determine the range of the data.
 */

#define CHUNK_SIZE 10240

static void
nifti_find_data_range(nifti_image *nii_ptr,
                      znzFile zfp,
                      VIO_Real *min_value_ptr,
                      VIO_Real *max_value_ptr)
{
  size_t i, j;
  double slope;
  double inter;
  long int initial_offset = znztell(zfp);
  double data[CHUNK_SIZE / sizeof(double)];
  size_t n_voxels_per_chunk;

  if (initial_offset < 0)       /* Did znztell() give an error? */
  {
    print_error("nifti_find_data_range: Tell failed.\n");
    return;
  }

  *min_value_ptr = FLT_MAX;
  *max_value_ptr = -FLT_MAX;

  if (nii_ptr->scl_slope <= 0.0)
  {
    slope = 1.0;
    inter = 0.0;
  }
  else
  {
    slope = nii_ptr->scl_slope;
    inter = nii_ptr->scl_inter;
  }

  n_voxels_per_chunk = CHUNK_SIZE / nii_ptr->nbyper;

  for (i = 0; i < nii_ptr->nvox; i += n_voxels_per_chunk)
  {
    double tmp = 0.0;
    size_t n_bytes_to_read;
    if (i + n_voxels_per_chunk > nii_ptr->nvox)
    {
      n_bytes_to_read = (nii_ptr->nvox - i) * nii_ptr->nbyper;
    }
    else
    {
      n_bytes_to_read = CHUNK_SIZE;
    }

    if (nifti_read_buffer(zfp, data, n_bytes_to_read, nii_ptr) != 
        n_bytes_to_read)
    {
      print_error("nifti_find_data_range: Read error.\n");
      return;
    }

    for (j = 0; j < n_voxels_per_chunk; j++)
    {
      switch (nii_ptr->datatype)
      {
      case DT_INT8:
        tmp = (double) ((char *)data)[j];
        break;
      case DT_UINT8:
        tmp = (double) ((unsigned char *)data)[j];
        break;
      case DT_INT16:
        tmp = (double) ((short *)data)[j];
        break;
      case DT_UINT16:
        tmp = (double) ((unsigned short *)data)[j];
        break;
      case DT_INT32:
        tmp = (double) ((int *)data)[j];
        break;
      case DT_UINT32:
        tmp = (double) ((unsigned int *)data)[j];
        break;
      case DT_FLOAT32:
        tmp = (double) ((float *)data)[j];
        break;
      case DT_FLOAT64:
        tmp = (double) ((double *)data)[j];
        break;
      default:
        fprintf(stderr, "NIfTI-1 data type %d not handled\n", 
                nii_ptr->datatype);
        break;
      }
      if (tmp < *min_value_ptr)
      {
        *min_value_ptr = tmp;
      }
      if (tmp > *max_value_ptr)
      {
        *max_value_ptr = tmp;
      }
    }
  }

  *min_value_ptr = (*min_value_ptr * slope) + inter;
  *max_value_ptr = (*max_value_ptr * slope) + inter;

  znzseek(zfp, initial_offset, SEEK_SET);
}

/**
 * Converts the fields in a nifti_image to the appropriate MINC attributes.
 */
static void 
nifti_image_to_minc_attributes(nifti_image *nii_ptr, 
                               int mnc_index_from_file[],
                               VIO_Real mnc_starts[],
                               VIO_Real mnc_steps[], 
                               VIO_Real mnc_dircos[][VIO_N_DIMENSIONS])
{
  size_t i, j;
  VIO_Transform mnc_xform;
  VIO_General_transform mnc_linear_xform;

  make_identity_transform(&mnc_xform);

  if (nii_ptr->nifti_type == NIFTI_FTYPE_ANALYZE ||
      (nii_ptr->sform_code == NIFTI_XFORM_UNKNOWN &&
       nii_ptr->qform_code == NIFTI_XFORM_UNKNOWN))
  {

    print_error("No transform found in header, guessing.\n");

    for (i = 0; i < VIO_MAX_DIMENSIONS; i++)
    {
      mnc_index_from_file[i] = i;
    }

    Transform_elem(mnc_xform, 0, 0) *= nii_ptr->dx;
    Transform_elem(mnc_xform, 1, 1) *= nii_ptr->dy;
    Transform_elem(mnc_xform, 2, 2) *= nii_ptr->dz;
    Transform_elem(mnc_xform, 0, 3) = -(nii_ptr->dx * nii_ptr->nx) / 2;
    Transform_elem(mnc_xform, 1, 3) = -(nii_ptr->dy * nii_ptr->ny) / 2;
    Transform_elem(mnc_xform, 2, 3) = -(nii_ptr->dz * nii_ptr->nz) / 2;

    create_linear_transform(&mnc_linear_xform, &mnc_xform);

    convert_transform_to_starts_and_steps(&mnc_linear_xform,
                                          VIO_N_DIMENSIONS,
                                          NULL,
                                          mnc_index_from_file,
                                          mnc_starts,
                                          mnc_steps,
                                          mnc_dircos);

  }
  else
  {
    if (nii_ptr->sform_code != NIFTI_XFORM_UNKNOWN)
    {
      for (i = 0; i < 4; i++)
      {
        for (j = 0; j < 4; j++)
        {
          Transform_elem(mnc_xform, i, j) = nii_ptr->sto_xyz.m[i][j];
        }
      }
    } 
    else
    {
      for (i = 0; i < 4; i++)
      {
        for (j = 0; j < 4; j++)
        {
          Transform_elem(mnc_xform, i, j) = nii_ptr->qto_xyz.m[i][j];
        }
      }
    }

    create_linear_transform(&mnc_linear_xform, &mnc_xform);
  
    for_less (i, 0, 6)
    {
      switch (i)
      {
      case 0:
        mnc_index_from_file[0] = VIO_X;
        mnc_index_from_file[1] = VIO_Y;
        mnc_index_from_file[2] = VIO_Z;
        break;
      case 1:
        mnc_index_from_file[0] = VIO_X;
        mnc_index_from_file[1] = VIO_Z;
        mnc_index_from_file[2] = VIO_Y;
        break;
      case 2:
        mnc_index_from_file[0] = VIO_Y;
        mnc_index_from_file[1] = VIO_X;
        mnc_index_from_file[2] = VIO_Z;
        break;
      case 3:
        mnc_index_from_file[0] = VIO_Y;
        mnc_index_from_file[1] = VIO_Z;
        mnc_index_from_file[2] = VIO_X;
        break;
      case 4:
        mnc_index_from_file[0] = VIO_Z;
        mnc_index_from_file[1] = VIO_X;
        mnc_index_from_file[2] = VIO_Y;
        break;
      case 5:
        mnc_index_from_file[0] = VIO_Z;
        mnc_index_from_file[1] = VIO_Y;
        mnc_index_from_file[2] = VIO_X;
        break;
      }
      /* For the time axis, if present. */
      mnc_index_from_file[3] = 3;

      convert_transform_to_starts_and_steps(&mnc_linear_xform,
                                            VIO_N_DIMENSIONS,
                                            NULL,
                                            mnc_index_from_file,
                                            mnc_starts,
                                            mnc_steps,
                                            mnc_dircos);
      if( fabs( mnc_dircos[0][VIO_X] ) > fabs( mnc_dircos[0][VIO_Y] ) &&
          fabs( mnc_dircos[0][VIO_X] ) > fabs( mnc_dircos[0][VIO_Z] ) &&
          fabs( mnc_dircos[1][VIO_Y] ) > fabs( mnc_dircos[1][VIO_X] ) &&
          fabs( mnc_dircos[1][VIO_Y] ) > fabs( mnc_dircos[1][VIO_Z] ) &&
          fabs( mnc_dircos[2][VIO_Z] ) > fabs( mnc_dircos[2][VIO_X] ) &&
          fabs( mnc_dircos[2][VIO_Z] ) > fabs( mnc_dircos[2][VIO_Y] ) )
      {
        break;
      }
    }
  }

  /* Adjust start and step values if alternate units are specified.
   */
  switch (nii_ptr->xyz_units) {
  case NIFTI_UNITS_METER:
    for (i = 0; i < VIO_N_DIMENSIONS; i++)
    {
      mnc_starts[i] *= 1000.0;
      mnc_steps[i] *= 1000.0;
    }
    break;
  case NIFTI_UNITS_MICRON:
    for (i = 0; i < VIO_N_DIMENSIONS; i++)
    {
      mnc_starts[i] /= 1000.0;
      mnc_steps[i] /= 1000.0;
    }
    break;
  default:
    break;
  }

  /* Store the start and step values for the time dimension, adjusting
   * the units as needed.
   */
  switch (nii_ptr->time_units) {
  case NIFTI_UNITS_MSEC:
    mnc_starts[3] = nii_ptr->toffset / 1000.0;
    mnc_steps[3] = nii_ptr->dt / 1000.0;
    break;
  case NIFTI_UNITS_USEC:
    mnc_starts[3] = nii_ptr->toffset / 1000000.0;
    mnc_steps[3] = nii_ptr->dt / 1000000.0;
    break;
  default:                      /* Either seconds or unknown. */
    mnc_starts[3] = nii_ptr->toffset;
    mnc_steps[3] = nii_ptr->dt;
    break;
  }
}

/**
 * nifti_image_open() gives us a file pointer to the image data, but
 * it unfortunately does not skip past the header if present.  So we
 * have to figure out the right offset to skip. This is a bit tricky,
 * and there is no public function in the library to do it, so I've
 * essentially copied the procedure here.
 */
static int
nifti_skip_header(znzFile zfp, nifti_image *nii_ptr)
{
  size_t ioff;
  size_t volsize = nifti_get_volsize(nii_ptr); // total bytes to read.

  // A negative offset means that it is relative to the end-of-file.
  if (nii_ptr->iname_offset < 0)
  {
    size_t filesize = nifti_get_filesize(nii_ptr->iname);
    if ( filesize <= 0 )            // Empty image file?
    {
      return VIO_ERROR;
    }
    ioff = (filesize > volsize) ? filesize - volsize : 0;
  }
  else
  {
    ioff = nii_ptr->iname_offset;
  }
  znzseek(zfp, ioff, SEEK_SET);
  return VIO_OK;
}

VIOAPI  VIO_Status
initialize_nifti_format_input(VIO_STR             filename,
                              VIO_Volume          volume,
                              volume_input_struct *in_ptr)
{
  int               sizes[VIO_MAX_DIMENSIONS];
  long              n_voxels_in_slice;
  nc_type           desired_nc_type;
  int               axis;
  nifti_image       *nii_ptr;
  VIO_Real          mnc_dircos[VIO_N_DIMENSIONS][VIO_N_DIMENSIONS];
  VIO_Real          mnc_steps[VIO_MAX_DIMENSIONS];
  VIO_Real          mnc_starts[VIO_MAX_DIMENSIONS];
  int               n_dimensions;
  znzFile           zfp;
  nc_type           file_nc_type;
  VIO_BOOL          signed_flag;

  /* Read in the NIfTI file header and get a znzFile handle to the data.
   */
  zfp = nifti_image_open(filename, "rb", &nii_ptr);
  if (znz_isnull(zfp))
  {
    nifti_image_free(nii_ptr);
    return VIO_ERROR;
  }
  else
  {
    nifti_skip_header(zfp, nii_ptr);
  }

  /* Translate from NIfTI to VIO types.
   */
  switch (nii_ptr->datatype)
  {
  case DT_UINT8:
    in_ptr->file_data_type = VIO_UNSIGNED_BYTE;
    file_nc_type = NC_BYTE;
    signed_flag = FALSE;
    break;
  case DT_INT8:
    in_ptr->file_data_type = VIO_SIGNED_BYTE;
    file_nc_type = NC_BYTE;
    signed_flag = TRUE;
    break;
  case DT_UINT16:
    in_ptr->file_data_type = VIO_UNSIGNED_SHORT;
    file_nc_type = NC_SHORT;
    signed_flag = FALSE;
    break;
  case DT_INT16:
    in_ptr->file_data_type = VIO_SIGNED_SHORT;
    file_nc_type = NC_SHORT;
    signed_flag = TRUE;
    break;
  case DT_UINT32:
    in_ptr->file_data_type = VIO_UNSIGNED_INT;
    file_nc_type = NC_INT;
    signed_flag = FALSE;
    break;
  case DT_INT32:
    in_ptr->file_data_type = VIO_SIGNED_INT;
    file_nc_type = NC_INT;
    signed_flag = TRUE;
    break;
  case DT_FLOAT32:
    in_ptr->file_data_type = VIO_FLOAT;
    file_nc_type = NC_FLOAT;
    signed_flag = TRUE;
    break;
  case DT_FLOAT64:
    in_ptr->file_data_type = VIO_DOUBLE;
    file_nc_type = NC_DOUBLE;
    signed_flag = TRUE;
    break;
  default:
    print_error("Unknown NIfTI-1 data type.\n");
    nifti_image_free(nii_ptr);
    znzclose(zfp);
    return VIO_ERROR;
  }

  n_dimensions = nii_ptr->dim[0];

  /* Treat 1D or 2D files as 3D */
  if (n_dimensions < VIO_N_DIMENSIONS)
  {
    n_dimensions = VIO_N_DIMENSIONS;
  }

  /* Ignore trailing dimensions of length 1 or less. The library
   * does not set the "ndim" field correctly in all cases.
   */
  while (n_dimensions > VIO_N_DIMENSIONS && nii_ptr->dim[n_dimensions] <= 1)
  {
    n_dimensions--;
  }

  for (axis = 0; axis < n_dimensions; axis++)
  {
    in_ptr->sizes_in_file[axis] = nii_ptr->dim[axis + 1];
    if (in_ptr->sizes_in_file[axis] <= 0)
      in_ptr->sizes_in_file[axis] = 1;
  }

  /* Decide how to store data in memory. */

  if ( get_volume_data_type(volume) == VIO_NO_DATA_TYPE )
  {
    desired_nc_type = file_nc_type;
  }
  else
  {
    desired_nc_type = get_volume_nc_data_type(volume, &signed_flag);
  }

  if( volume->spatial_axes[VIO_X] < 0 ||
      volume->spatial_axes[VIO_Y] < 0 ||
      volume->spatial_axes[VIO_Z] < 0 )
  {
    print_error("warning: setting NIfTI-1 spatial axes to XYZ.\n");
    volume->spatial_axes[VIO_X] = 0;
    volume->spatial_axes[VIO_Y] = 1;
    volume->spatial_axes[VIO_Z] = 2;
  }

  if (!set_volume_n_dimensions(volume, n_dimensions))
  {
    print_error("Problem setting number of dimensions.\n");
    nifti_image_free(nii_ptr);
    znzclose(zfp);
    return VIO_ERROR;
  }

  nifti_image_to_minc_attributes(nii_ptr, in_ptr->axis_index_from_file,
                                 mnc_starts, mnc_steps, mnc_dircos);

  for_less( axis, 0, n_dimensions)
  {
    int volume_axis = in_ptr->axis_index_from_file[axis];

    sizes[volume_axis] = in_ptr->sizes_in_file[axis];
    if (axis < 3)
    {
      /* DEBUG */
      printf("%d %d size:%4d step:%6.3f start:%9.4f dc:[%7.4f %7.4f %7.4f]\n",
             axis,
             volume_axis,
             sizes[volume_axis],
             mnc_steps[volume_axis],
             mnc_starts[volume_axis],
             mnc_dircos[volume_axis][0], 
             mnc_dircos[volume_axis][1], 
             mnc_dircos[volume_axis][2]);

      set_volume_direction_cosine(volume, volume_axis, mnc_dircos[axis]);
    }
    else
    {
      /* DEBUG */
      printf("%d %d size:%4d step:%6.3f start:%9.4f\n",
             axis,
             volume_axis,
             sizes[volume_axis],
             mnc_steps[volume_axis],
             mnc_starts[volume_axis]);
    }
  }

  set_volume_separations( volume, mnc_steps );
  set_volume_starts( volume, mnc_starts );

  set_volume_type( volume, desired_nc_type, signed_flag, 0.0, 0.0 );
  set_volume_sizes( volume, sizes );

  n_voxels_in_slice = (in_ptr->sizes_in_file[0] * in_ptr->sizes_in_file[1]);

  /* If the data must be converted to byte, read the entire image file simply
   * to find the max and min values. This allows us to set the value_scale and
   * value_translation properly when we read the file.
   */
  if (get_volume_data_type(volume) != in_ptr->file_data_type )
  {
    VIO_Real original_min_value, original_max_value;

    nifti_find_data_range(nii_ptr, zfp,
                          &original_min_value,
                          &original_max_value);
    set_volume_voxel_range(volume, original_min_value, original_max_value);
  }

  in_ptr->min_value = FLT_MAX;
  in_ptr->max_value = -FLT_MAX;
  in_ptr->slice_index = 0;
  in_ptr->volume_file = (FILE *) zfp;
  in_ptr->header_info = nii_ptr;
  in_ptr->generic_slice_buffer = malloc(n_voxels_in_slice * nii_ptr->nbyper);
  if (in_ptr->generic_slice_buffer == NULL)
  {
    return VIO_ERROR;
  }
  return VIO_OK;
}

VIOAPI void
delete_nifti_format_input(
                          volume_input_struct   *in_ptr
                          )
{
  nifti_image *nii_ptr = (nifti_image *) in_ptr->header_info;
  znzFile zfp = (znzFile) in_ptr->volume_file;
  nifti_image_free(nii_ptr);
  znzclose(zfp);
  free(in_ptr->generic_slice_buffer);
}

VIOAPI  VIO_BOOL
input_more_nifti_format_file(
                             VIO_Volume          volume,
                             volume_input_struct *in_ptr,
                             VIO_Real            *fraction_done
                             )
{
  nifti_image    *nii_ptr = (nifti_image *) in_ptr->header_info;
  znzFile        zfp = (znzFile) in_ptr->volume_file;
  void           *data_ptr = in_ptr->generic_slice_buffer;
  int            data_ind = 0;
  double         value = 0;
  double         value_offset, value_scale;
  int            *inner_index;
  int            indices[VIO_MAX_DIMENSIONS];
  VIO_Real       original_min_value, original_max_value;
  int            i;
  int            total_slices;

  total_slices = in_ptr->sizes_in_file[2];
  if (get_volume_n_dimensions(volume) > 3)
  {
    /* If there is a time dimension, incorporate that into our slice
     * count.
     */
    total_slices *= in_ptr->sizes_in_file[3];
  }
  
  if ( in_ptr->slice_index < total_slices )
  {
    size_t     n_bytes_per_slice;
    size_t     n_bytes_read;

    n_bytes_per_slice = (in_ptr->sizes_in_file[0] *
                         in_ptr->sizes_in_file[1] *
                         nii_ptr->nbyper);

    /* If the memory for the volume has not been allocated yet,
     * initialize that memory now.
     */
    if (!volume_is_alloced(volume))
    {
      alloc_volume_data(volume);
      if (!volume_is_alloced(volume))
      {
        print_error("Failed to allocate volume.\n");
        return FALSE;
      }
    }

    n_bytes_read = nifti_read_buffer(zfp, data_ptr, n_bytes_per_slice, nii_ptr);
    if (n_bytes_read < n_bytes_per_slice)
    {
      return FALSE;
    }

    /* See if we need to apply scaling to this slice. This is only
     * needed if the volume voxel type is not the same as the file
     * voxel type. THIS IS ONLY REALLY LEGAL FOR BYTE VOLUME TYPES.
     */
    if (get_volume_data_type(volume) != in_ptr->file_data_type)
    {
      get_volume_voxel_range(volume, &original_min_value, &original_max_value);

      value_offset = original_min_value;
      value_scale = (original_max_value - original_min_value) /
        (VIO_Real) (NUM_BYTE_VALUES - 1);
    }
    else
    {
      /* Just do trivial scaling. */
      value_offset = 0.0;
      value_scale = 1.0;
    }

    /* Set up the indices.
     */
    inner_index = &indices[in_ptr->axis_index_from_file[0]];

    if (get_volume_n_dimensions(volume) > 3)
    {
      /* If a time dimension is present, convert the slice index into
       * both a time and slice coordinate using the number of slices.
       */
      indices[in_ptr->axis_index_from_file[3]] = in_ptr->slice_index / in_ptr->sizes_in_file[2];
      indices[in_ptr->axis_index_from_file[2]] = in_ptr->slice_index % in_ptr->sizes_in_file[2];
    }
    else
    {
      indices[in_ptr->axis_index_from_file[2]] = in_ptr->slice_index;
    }

    for_less( i, 0, in_ptr->sizes_in_file[1] )
    {
      indices[in_ptr->axis_index_from_file[1]] = i;
      for_less( *inner_index, 0, in_ptr->sizes_in_file[0] )
      {
        switch ( in_ptr->file_data_type )
        {
        case VIO_UNSIGNED_BYTE:
          value = ((unsigned char *) data_ptr)[data_ind++];
          break;
        case VIO_SIGNED_BYTE:
          value = ((char *) data_ptr)[data_ind++];
          break;
        case VIO_UNSIGNED_SHORT:
          value = ((unsigned short *) data_ptr)[data_ind++];
          break;
        case VIO_SIGNED_SHORT:
          value = ((short *) data_ptr)[data_ind++];
          break;
        case VIO_UNSIGNED_INT:
          value = ((unsigned int *) data_ptr)[data_ind++];
          break;
        case VIO_SIGNED_INT:
          value = ((int *) data_ptr)[data_ind++];
          break;
        case VIO_FLOAT:
          value = ((float *) data_ptr)[data_ind++];
          break;
        case VIO_DOUBLE:
          value = ((double *)data_ptr)[data_ind++];
          break;
        default:
          handle_internal_error( "input_more_nifti_format_file" );
          break;
        }
        
        value = (value - value_offset) / value_scale;

        switch (get_volume_data_type(volume))
        {
        case VIO_UNSIGNED_BYTE:
          if (value < 0 || value > UCHAR_MAX)
          {
            print_error("clipping uint8 value\n");
          }
          break;
        case VIO_SIGNED_BYTE:
          if (value < SCHAR_MIN || value > SCHAR_MAX)
          {
            print_error("clipping int8 value\n");
          }
          break;
        case VIO_UNSIGNED_SHORT:
          if (value < 0 || value > USHRT_MAX)
          {
            print_error("clipping uint16 value\n");
          }
          break;
        case VIO_SIGNED_SHORT:
          if (value < SHRT_MIN || value > SHRT_MAX)
          {
            print_error("clipping int16 value\n");
          }
          break;
        case VIO_UNSIGNED_INT:
          if (value < 0 || value > UINT_MAX)
          {
            print_error("clipping uint32 value\n");
          }
          break;
        case VIO_SIGNED_INT:
          if (value < INT_MIN || value > INT_MAX)
          {
            print_error("clipping int32 value\n");
          }
          break;
        case VIO_NO_DATA_TYPE:
        case VIO_FLOAT:
        case VIO_DOUBLE:
        case VIO_MAX_DATA_TYPE:
          break;
        }
        if (value > in_ptr->max_value)
        {
          in_ptr->max_value = value;
        }
        if (value < in_ptr->min_value)
        {
          in_ptr->min_value = value;
        }
        set_volume_voxel_value( volume,
                                indices[VIO_X],
                                indices[VIO_Y],
                                indices[VIO_Z],
                                indices[3],
                                indices[4],
                                value);

      }
    }

    in_ptr->slice_index++;      /* Advance to the next slice. */
  }

  *fraction_done = (VIO_Real) in_ptr->slice_index / total_slices;

  if (in_ptr->slice_index >= total_slices)
  {
    set_volume_voxel_range( volume, in_ptr->min_value, in_ptr->max_value );

    /* Make sure we scale the data up to the original real range,
     * if appropriate.
     */
    if (get_volume_data_type(volume) != in_ptr->file_data_type)
    {
      set_volume_real_range(volume, original_min_value, original_max_value);
    }

    return FALSE;
  }
  else
  {
    return TRUE;
  }
}
