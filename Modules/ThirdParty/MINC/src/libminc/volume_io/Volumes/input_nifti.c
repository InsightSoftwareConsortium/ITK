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
  long int initial_offset = znztell(zfp);
  double data[CHUNK_SIZE / sizeof(double)];
  size_t n_voxels_per_chunk;

  if (initial_offset < 0)       /* Did znztell() give an error? */
  {
    print_error("nifti_find_data_range: Tell failed.\n");
    return;
  }

  *min_value_ptr = DBL_MAX;
  *max_value_ptr = -DBL_MAX;

  n_voxels_per_chunk = CHUNK_SIZE / nii_ptr->nbyper;

  if ( nii_ptr->datatype == DT_RGB24 )
  {
    *min_value_ptr = 0;
    *max_value_ptr = 0xffffffff;
    return;
  }

  for (i = 0; i < nii_ptr->nvox; i += n_voxels_per_chunk)
  {
    double tmp = 0.0;
    size_t n_bytes_to_read;
    if (i + n_voxels_per_chunk > nii_ptr->nvox)
    {
      /* Final partial chunk. Adjust the counters to read only
       * the bytes and voxels actually loaded.
       */
      n_voxels_per_chunk = nii_ptr->nvox - i;
      n_bytes_to_read = n_voxels_per_chunk * nii_ptr->nbyper;
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
        return;
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
  int i, j;
  VIO_Transform mnc_xform;
  VIO_General_transform mnc_linear_xform;

  make_identity_transform(&mnc_xform);

  /* Initialize with default indices. */
  for (i = 0; i < VIO_MAX_DIMENSIONS; i++)
  {
    mnc_index_from_file[i] = i;
    mnc_starts[i] = 0;
    mnc_steps[i] = 1;
  }

  if (nii_ptr->nifti_type == NIFTI_FTYPE_ANALYZE ||
      (nii_ptr->sform_code == NIFTI_XFORM_UNKNOWN &&
       nii_ptr->qform_code == NIFTI_XFORM_UNKNOWN))
  {

    print_error("No transform found in header, guessing.\n");

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
    mat44 nii_xfm;
    if (nii_ptr->sform_code != NIFTI_XFORM_UNKNOWN)
    {
      nii_xfm = nii_ptr->sto_xyz;
    }
    else
    {
      nii_xfm = nii_ptr->qto_xyz;
    }

    for_less( i, 0, VIO_N_DIMENSIONS )
    {
      int spatial_axis = VIO_X;
      float c_x = fabsf(nii_xfm.m[VIO_X][i]);
      float c_y = fabsf(nii_xfm.m[VIO_Y][i]);
      float c_z = fabsf(nii_xfm.m[VIO_Z][i]);
      if (c_y > c_x && c_y > c_z)
      {
        spatial_axis = VIO_Y;
      }
      if (c_z > c_x && c_z > c_y)
      {
        spatial_axis = VIO_Z;
      }
      mnc_index_from_file[i] = spatial_axis;
    }

    for (i = 0; i < VIO_N_DIMENSIONS; i++)
    {
      for (j = 0; j < 4; j++)
      {
        int volume_axis = (j < VIO_N_DIMENSIONS) ? mnc_index_from_file[j] : j;
        Transform_elem(mnc_xform, i, volume_axis) = nii_xfm.m[i][j];
      }
    }

    create_linear_transform(&mnc_linear_xform, &mnc_xform);

    convert_transform_to_starts_and_steps(&mnc_linear_xform,
                                          VIO_N_DIMENSIONS,
                                          NULL,
                                          mnc_index_from_file,
                                          mnc_starts,
                                          mnc_steps,
                                          mnc_dircos);
  }

  /* Adjust start and step values if alternate units are specified.
   */
  switch (nii_ptr->xyz_units)
  {
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
  switch (nii_ptr->time_units)
  {
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

  delete_general_transform(&mnc_linear_xform);
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
  VIO_Real          steps[VIO_MAX_DIMENSIONS];
  VIO_Real          starts[VIO_MAX_DIMENSIONS];
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
  VIO_Real          min_voxel, max_voxel;
  VIO_Real          min_real, max_real;

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
  case DT_RGB24:
    in_ptr->file_data_type = VIO_UNSIGNED_INT;
    file_nc_type = NC_INT;
    signed_flag = FALSE;
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

#if DEBUG
  nifti_image_infodump(nii_ptr);
#endif

  nifti_image_to_minc_attributes(nii_ptr, in_ptr->axis_index_from_file,
                                 mnc_starts, mnc_steps, mnc_dircos);

  /* Put the various arrays in the correct order. */
  for_less( axis, 0, n_dimensions)
  {
    if (axis < 3)
    {
      int volume_axis = in_ptr->axis_index_from_file[axis];
      sizes[volume_axis] = in_ptr->sizes_in_file[axis];
      steps[axis] = mnc_steps[volume_axis];
      starts[axis] = mnc_starts[volume_axis];

      set_volume_direction_cosine(volume, axis, mnc_dircos[volume_axis]);
    }
    else
    {
      sizes[axis] = in_ptr->sizes_in_file[axis];
      steps[axis] = mnc_steps[axis];
      starts[axis] = mnc_starts[axis];
    }
  }

#if DEBUG
  for_less( axis, 0, n_dimensions )
  {
    if (axis < 3)
    {
      int volume_axis = in_ptr->axis_index_from_file[axis];

      printf("%d %d size:%4d step:%6.3f start:%9.4f dc:[%7.4f %7.4f %7.4f]\n",
             axis,
             volume_axis,
             sizes[axis],
             mnc_steps[volume_axis],
             mnc_starts[volume_axis],
             mnc_dircos[volume_axis][0],
             mnc_dircos[volume_axis][1],
             mnc_dircos[volume_axis][2]);

    }
    else
    {
      printf("%d %d size:%4d step:%6.3f start:%9.4f\n",
             axis,
             axis,
             sizes[axis],
             mnc_steps[axis],
             mnc_starts[axis]);
    }
  }
#endif /* DEBUG */

  set_volume_separations( volume, steps );
  set_volume_starts( volume, starts );

  set_volume_type( volume, desired_nc_type, signed_flag, 0.0, 0.0 );
  set_volume_sizes( volume, sizes );

  n_voxels_in_slice = (in_ptr->sizes_in_file[0] * in_ptr->sizes_in_file[1]);

  set_rgb_volume_flag( volume, nii_ptr->datatype == DT_RGB24 );

  /* Determine the voxel range of NIfTI data. */
  /* TODO: Is this really needed? Could we just assume the file uses
   * the full available range of the datatype without any significant
   * consequences?
   */
  nifti_find_data_range(nii_ptr, zfp, &min_voxel, &max_voxel);

  /* Calculate the real range of the data, using the NIfTI slope and
   * scale, if appropriate.
   */
  if (nii_ptr->scl_slope > 0)
  {
    min_real = (min_voxel * nii_ptr->scl_slope) + nii_ptr->scl_inter;
    max_real = (max_voxel * nii_ptr->scl_slope) + nii_ptr->scl_inter;
  }
  else
  {
    min_real = min_voxel;
    max_real = max_voxel;
  }

  /* As a special case, if we are converting the file to byte,
   * we need to prepare to scale the voxels into the final range.
   */
  if (get_volume_data_type(volume) == VIO_UNSIGNED_BYTE &&
      in_ptr->file_data_type != VIO_UNSIGNED_BYTE)
  {
    /* Set up the scaling for when we actually read the data.
     */
    in_ptr->min_value = min_voxel;
    in_ptr->max_value = max_voxel;

    min_voxel = 0;
    max_voxel = UCHAR_MAX;
  }
  else
  {
    in_ptr->min_value = 0.0;
    in_ptr->max_value = 1.0;
  }

  set_volume_voxel_range(volume, min_voxel, max_voxel);
  set_volume_real_range(volume, min_real, max_real);

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
  int            indices[VIO_MAX_DIMENSIONS];
  int            i;
  int            total_slices;
  int            n_dimensions = get_volume_n_dimensions( volume );
  int            vio_data_type = get_volume_data_type( volume );

  for_less(i, 0, VIO_MAX_DIMENSIONS)
    indices[i] = 0;

  total_slices = 1;
  for_less( i, 2, n_dimensions )
    total_slices *= in_ptr->sizes_in_file[i];

  if ( in_ptr->slice_index < total_slices )
  {
    size_t     n_bytes_per_slice;
    size_t     n_bytes_read;
    int        sizes[VIO_MAX_DIMENSIONS] = {1, 1, 1, 1, 1};

    sizes[in_ptr->axis_index_from_file[0]] = in_ptr->sizes_in_file[0];
    sizes[in_ptr->axis_index_from_file[1]] = 1;

    n_bytes_per_slice = (in_ptr->sizes_in_file[0] *
                         in_ptr->sizes_in_file[1] *
                         nii_ptr->nbyper);

    VIO_Real *temp_buffer = malloc(in_ptr->sizes_in_file[0] *
                                   sizeof(VIO_Real));

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

    if (vio_data_type == VIO_UNSIGNED_BYTE &&
        in_ptr->file_data_type != VIO_UNSIGNED_BYTE)
    {
      value_offset = in_ptr->min_value;
      value_scale = (in_ptr->max_value - in_ptr->min_value) /
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
    i = in_ptr->slice_index;
    switch ( n_dimensions )
    {
    case 5:
      /* If a vector dimension is present, convert the slice index into
       * a vector, time, and slice coordinate.
       */
      indices[in_ptr->axis_index_from_file[4]] = i / (in_ptr->sizes_in_file[3] * in_ptr->sizes_in_file[2]);
      i %= (in_ptr->sizes_in_file[3] * in_ptr->sizes_in_file[2]);
      /* fall through */
    case 4:
      /* If a time dimension is present, convert the slice index into
       * both a time and slice coordinate using the number of slices.
       */
      indices[in_ptr->axis_index_from_file[3]] = i / in_ptr->sizes_in_file[2];
      i %= in_ptr->sizes_in_file[2];
      /* fall through */
    default:
      indices[in_ptr->axis_index_from_file[2]] = i;
      break;
    }

    for_less( i, 0, in_ptr->sizes_in_file[1] )
    {
      int temp_ind;

      indices[in_ptr->axis_index_from_file[1]] = i;
      indices[in_ptr->axis_index_from_file[0]] = 0;

      switch ( nii_ptr->datatype ) {
      case DT_UINT8:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((unsigned char *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_INT8:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((signed char *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_UINT16:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((unsigned short *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_INT16:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((short *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_UINT32:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((unsigned int *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_INT32:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((int *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_FLOAT32:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((float *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_FLOAT64:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          value = ((double *) data_ptr)[data_ind++];
          value = (value - value_offset) / value_scale;
          temp_buffer[temp_ind] = value;
        }
        break;
      case DT_RGB24:
        for_less( temp_ind, 0, in_ptr->sizes_in_file[0] )
        {
          /* The only RGB-valued NIfTI files I have found to date organize
           * the data in an unexpected manner. Each "slice" consists of three
           * adjacent slices, each consisting of all of the of R, G, and B
           * values for the slice. This is different from what I expected,
           * which was to find the RGB triple in three adjacent bytes.
           *
           * Some comments I have seen suggest that the "adjacent"
           * byte format is also possible, so there may be a need to
           * allow the user to choose how RGB data is loaded (since there
           * is probably no easy way to tell what one has, short of
           * some complex calculation).
           *
           * One guess is that if the intent code is set to 'none',
           * the most common case, we're dealing with one of these
           * oddly-organized files.
           */
          if (nii_ptr->intent_code == NIFTI_INTENT_NONE)
          {
            int n = n_bytes_per_slice / 3;
            unsigned char r = ((unsigned char *)data_ptr)[data_ind + (n * 0)];
            unsigned char g = ((unsigned char *)data_ptr)[data_ind + (n * 1)];
            unsigned char b = ((unsigned char *)data_ptr)[data_ind + (n * 2)];
            value = (double) make_Colour( r, g, b );
            data_ind += 1;
          }
          else
          {
            unsigned char r = ((unsigned char *)data_ptr)[data_ind + 0];
            unsigned char g = ((unsigned char *)data_ptr)[data_ind + 1];
            unsigned char b = ((unsigned char *)data_ptr)[data_ind + 2];
            value = (double) make_Colour( r, g, b );
            data_ind += 3;
          }
          temp_buffer[temp_ind] = value;
        }
        break;
      default:
        handle_internal_error( "input_more_nifti_format_file" );
        break;
      }

      set_volume_voxel_hyperslab( volume,
                                  indices[VIO_X],
                                  indices[VIO_Y],
                                  indices[VIO_Z],
                                  indices[3],
                                  indices[4],
                                  sizes[VIO_X],
                                  sizes[VIO_Y],
                                  sizes[VIO_Z],
                                  1,
                                  1,
                                  temp_buffer );
    }

    in_ptr->slice_index++;      /* Advance to the next slice. */

    free(temp_buffer);
  }

  *fraction_done = (VIO_Real) in_ptr->slice_index / total_slices;

  return (in_ptr->slice_index < total_slices);
}
