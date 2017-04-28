/** \file convert.c
 * \brief MINC 2.0 Coordinate and Voxel Conversion Functions
 * \author Bert Vincent
 *
 * Functions to convert "real" valued data to and from "voxel" valued
 * data, and to convert coordinates between "voxel" and "world" systems.
 */

/**
 * \defgroup mi2Cvt MINC 2.0 Coordinate and Voxel Conversion Functions
 */
#include <stdlib.h>
#include <hdf5.h>
#include <math.h>
#include <float.h>
#include "minc2.h"
#include "minc2_private.h"


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
                        double *voxel_value_ptr
                        )
{
    int result = MI_NOERROR;
    double valid_min, valid_max;
    double slice_min, slice_max;
    double voxel_range, voxel_offset;
    double real_range, real_offset;

    /* get valid min/max, image min/max 
     */
    miget_volume_valid_range(volume, &valid_max, &valid_min);
    
    /* get image min/max 
     */
    miget_slice_range(volume, coords, ncoords, &slice_max, &slice_min);
    
    /* Calculate the actual conversion.
     */
    voxel_offset = valid_min;
    real_offset = slice_min;
    voxel_range = valid_max - valid_min;
    real_range = slice_max - slice_min;
    
    real_value = (real_value - real_offset) / real_range;
    *voxel_value_ptr = (real_value * voxel_range) + voxel_offset;

    return (result);
}


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
                        double *real_value_ptr)
{
    int result = MI_NOERROR;
    double valid_min, valid_max;
    double slice_min, slice_max;
    double voxel_range, voxel_offset;
    double real_range, real_offset;

    /* get valid min/max, image min/max 
     */
    miget_volume_valid_range(volume, &valid_max, &valid_min);
    
    /* get image min/max 
     */
    miget_slice_range(volume, coords, ncoords, &slice_max, &slice_min);

    /* Calculate the actual conversion.
     */
    voxel_offset = valid_min;
    real_offset = slice_min;
    voxel_range = valid_max - valid_min;
    real_range = slice_max - slice_min;
    
    voxel_value = (voxel_value - voxel_offset) / voxel_range;
    *real_value_ptr = (voxel_value * real_range) + real_offset;
    return (result);
}

/** \internal
 */
static void
mireorder_voxel_to_xyz(mihandle_t volume, 
                       const double voxel[], 
                       double xyz[MI2_3D], 
                       midimclass_t dimclass)
{
    int i, axis;

    for (i = 0; i < volume->number_of_dims; i++) {
        midimhandle_t hdim = volume->dim_handles[i];
        axis = hdim->world_index;
        if ( axis >= 0 && dimclass == hdim->dim_class) {
            xyz[axis] = voxel[i];
        }
    }
}

/** \internal
 */
static void
mireorder_xyz_to_voxel(mihandle_t volume, 
                       const double xyz[MI2_3D], 
                       double voxel[],
                       midimclass_t dimclass)
{
    int i, axis;

    for (i = 0; i < volume->number_of_dims; i++) {
        midimhandle_t hdim = volume->dim_handles[i];
        axis = hdim->world_index;
        if ( axis >= 0 && dimclass == hdim->dim_class) {
            voxel[i] = xyz[axis];
        }
    }
}

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
                         double world[MI2_3D])
{
    double temp[MI2_3D];

    mireorder_voxel_to_xyz(volume, voxel, temp, MI_DIMCLASS_SPATIAL);
    mitransform_coord(world, volume->v2w_transform, temp);
    return (MI_NOERROR);
}

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
                         const double world[MI2_3D],
                         double voxel[])
{
    double temp[MI2_3D];
    int i;

    for (i = 0; i < volume->number_of_dims; i++) {
        voxel[i] = 0.0;
    }

    mitransform_coord(temp, volume->w2v_transform, world);
    mireorder_xyz_to_voxel(volume, temp, voxel, MI_DIMCLASS_SPATIAL);
    return (MI_NOERROR);
}

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
                 double *value_ptr)
{
    double voxel;
    int result;

    result = miget_voxel_value(volume, coords, ndims, &voxel);
    if (result != MI_NOERROR) {
        return (result);
    }
    miconvert_voxel_to_real(volume, coords, ndims, voxel, value_ptr);
    return (MI_NOERROR);
}

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
                 double value)
{
    double voxel;

    if ((volume->mode & MI2_OPEN_RDWR) == 0) {
        //TODO: report that file is not open properly
        return (MI_ERROR);
    }

    miconvert_real_to_voxel(volume, coords, ndims, value, &voxel);
    return (miset_voxel_value(volume, coords, ndims, voxel));
}


/**
 * This function calculates the start values for the volume dimensions,
 * assuming that the spatial origin is relocated to the given world
 * coordinate.
 *
 * \ingroup mi2Cvt
 */
int
miconvert_world_origin_to_start( mihandle_t volume,
                                 double world[3],
                                 double starts[3])
{
    fprintf(stderr, "miconvert_world_origin_to_start: Not implemented.\n");
    return (MI_NOERROR);
}

/**
 * This function calculates the start values for the volume dimensions,
 * assuming that the spatial origin is relocated to the given world
 * coordinate.
 *
 * \ingroup mi2Cvt
 */
int
miconvert_spatial_frequency_origin_to_start( mihandle_t volume,
                                             double world[3],
                                             double starts[3])
{
    fprintf(stderr, "miconvert_spatial_frequency_origin_to_start: Not implemented.\n");
    return (MI_NOERROR);
}

static double 
dot_vectors(int n, double v1[], double v2[])
{
    int i;
    double d;

    d = 0.0;
    for ( i = 0; i < n; i++ ) {
        d += v1[i] * v2[i];
    }
    return ( d );
}

static int
solve_linear_system( int n, double **coefs, double *values, double *solution)
{
    int i;

    for ( i = 0; i < n; i++ ) {
        solution[i] = values[i];
    }

    return (scaled_maximal_pivoting_gaussian_elimination_real(n, coefs, 1,
                                                              &solution ));
}

static void
convert_transform_origin_to_starts(mihandle_t hvol,
                                   double origin[],
                                   double starts[] )
{
    int axis;
    int which[MI2_3D];
    int n_axes, i, j;
    double o_dot_c, c_dot_c;
    double x_dot_x, x_dot_y, x_dot_v, y_dot_y, y_dot_v, bottom;
    double **matrix, solution[MI2_3D];

    for ( i = 0; i < hvol->number_of_dims; i++) {
        starts[i] = 0.0;
    }

    /*--- get the list of valid axes (which) */

    n_axes = 0;
    for ( i = 0; i < hvol->number_of_dims; i++ ) {
        axis = hvol->dim_handles[i]->world_index;
        if ( axis >= 0 ) {
            which[axis] = i;
            ++n_axes;
        }
    }

    /*--- get the starts: computed differently for 1, 2, or 3 axes */

    switch (n_axes) {
    case 1:
        o_dot_c = dot_vectors(MI2_3D, 
                              origin, 
                              hvol->dim_handles[which[0]]->direction_cosines);
        c_dot_c = dot_vectors(MI2_3D, 
                              hvol->dim_handles[which[0]]->direction_cosines,
                              hvol->dim_handles[which[0]]->direction_cosines);
        if ( c_dot_c != 0.0 ) {
            starts[which[0]] = o_dot_c / c_dot_c;
        }
        break;

    case 2:
        x_dot_x = dot_vectors(MI2_3D, 
                              hvol->dim_handles[which[0]]->direction_cosines,
                              hvol->dim_handles[which[0]]->direction_cosines );
        x_dot_v = dot_vectors(MI2_3D, 
                              hvol->dim_handles[which[0]]->direction_cosines,
                              origin );
        x_dot_y = dot_vectors(MI2_3D, 
                              hvol->dim_handles[which[0]]->direction_cosines,
                              hvol->dim_handles[which[1]]->direction_cosines );
        y_dot_y = dot_vectors(MI2_3D, 
                              hvol->dim_handles[which[1]]->direction_cosines,
                              hvol->dim_handles[which[1]]->direction_cosines );
        y_dot_v = dot_vectors(MI2_3D, 
                              hvol->dim_handles[which[1]]->direction_cosines, 
                              origin );

        bottom = x_dot_x * y_dot_y - x_dot_y * x_dot_y;

        if ( bottom != 0.0 ) {
            starts[which[0]] = (x_dot_v * y_dot_y - x_dot_y * y_dot_v) / bottom;
            starts[which[1]] = (y_dot_v * x_dot_x - x_dot_y * x_dot_v) / bottom;
        }
        break;

    case 3:
        /*--- this is the usual case, solve the equations to find what
              starts give the desired origin */

        matrix = alloc2d(MI2_3D, MI2_3D);

        for ( i = 0; i < MI2_3D; i++) {
            for ( j = 0; j < hvol->number_of_dims; j++ ) {
                matrix[i][j] = hvol->dim_handles[j]->direction_cosines[i];
            }
        }

        if ( solve_linear_system( MI2_3D, matrix, origin, solution ) ) {
            starts[which[0]] = solution[0];
            starts[which[1]] = solution[1];
            starts[which[2]] = solution[2];
        }

        free2d(MI2_3D, matrix);

        break;
    }
}

/**
 * This function sets the world coordinates of the point (0,0,0) in voxel
 * coordinates.  This changes the constant offset of the two coordinate
 * systems.
 *
 * \ingroup mi2Cvt
 */
int miset_world_origin(mihandle_t volume, /**< A volume handle */
                   double world[MI2_3D]) /**< The world coordinates of voxel origin  */
{
    double starts[MI2_3D];
    int i;

    convert_transform_origin_to_starts(volume, world, starts);
    for (i = 0; i < volume->number_of_dims; i++) {
        midimhandle_t hdim = volume->dim_handles[i];
        if (hdim->dim_class == MI_DIMCLASS_SPATIAL || 
            hdim->dim_class == MI_DIMCLASS_SFREQUENCY) {
            hdim->start = starts[hdim->world_index];
        }
    }

    /* Get the voxel to world transform for the volume
     */
    miget_voxel_to_world(volume, volume->v2w_transform);

    /* Calculate the inverse transform */
    miinvert_transform(volume->v2w_transform, volume->w2v_transform);

    return (MI_NOERROR);
}

/**
 * This function sets the world coordinates of the point (0,0,0) in voxel
 * coordinates.  This changes the constant offset of the two coordinate
 * systems.
 *
 * \ingroup mi2Cvt
 */
int
miset_spatial_frequency_origin(mihandle_t volume,
                               double world[3])
{
    if ((volume->mode & MI2_OPEN_RDWR) == 0) {
        return (MI_ERROR);
    }

    fprintf(stderr, "miset_spatial_frequency_origin: Not implemented.\n");
    return (MI_NOERROR);
}

/** This function retrieves the voxel values of a position in the
 * MINC volume. The voxel value is the unscaled value, and corresponds
 * to the value actually stored in the file.
 *
 * \ingroup mi2Cvt
 */
int miget_voxel_value(mihandle_t volume,
                  const misize_t coords[],
                  int ndims,
                  double *voxel_ptr)
{
    int result;
    misize_t count[MI2_MAX_VAR_DIMS];
    int i;

    for (i = 0; i < volume->number_of_dims; i++) {
        count[i] = 1;
    }
    result = miget_voxel_value_hyperslab(volume, MI_TYPE_DOUBLE, 
                                         coords, count, voxel_ptr);
    return (result);
}

/** This function sets the voxel value of a position in the MINC
 * volume.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file.
 *
 * \ingroup mi2Cvt
 */
int miset_voxel_value(mihandle_t volume,
                  const misize_t coords[],
                  int ndims,
                  double voxel)
{
    int result;
    misize_t count[MI2_MAX_VAR_DIMS];
    int i;

    if ((volume->mode & MI2_OPEN_RDWR) == 0) {
        return (MI_ERROR);
    }

    for (i = 0; i < ndims; i++) {
        count[i] = 1;
    }
    
    result = miset_voxel_value_hyperslab(volume, MI_TYPE_DOUBLE, 
                                         coords, count, &voxel);
    return (result);
}


int miget_volume_real_range(mihandle_t volume, double real_range[])
{
    hid_t spc_id;
    int n;
    double *buffer;
    int i;

    /* First find the real minimum.
     */
    spc_id = H5Dget_space(volume->imin_id);

    n = (int) H5Sget_simple_extent_npoints(spc_id);

    H5Sclose(spc_id);

    buffer = malloc(n * sizeof(double));
    if (buffer == NULL) {
        return (MI_ERROR);
    }

    H5Dread(volume->imin_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            buffer);

    real_range[0] = DBL_MAX;

    for (i = 0; i < n; i++) {
        if (buffer[i] < real_range[0]) {
            real_range[0] = buffer[i];
        }
    }
    
    free(buffer);

    /* Now find the maximum.
     */
    spc_id = H5Dget_space(volume->imax_id);

    n = (int) H5Sget_simple_extent_npoints(spc_id);

    H5Sclose(spc_id);

    buffer = malloc(n * sizeof(double));
    if (buffer == NULL) {
        return (MI_ERROR);
    }

    H5Dread(volume->imax_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
            buffer);

    real_range[1] = -DBL_MAX;

    for (i = 0; i < n; i++) {
        if (buffer[i] > real_range[1]) {
            real_range[1] = buffer[i];
        }
    }
    
    free(buffer);

    return (MI_NOERROR);
}

/* kate: indent-mode cstyle; indent-width 2; replace-tabs on; */
