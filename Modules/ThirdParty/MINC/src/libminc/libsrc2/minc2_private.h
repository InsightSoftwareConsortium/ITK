/** \internal
 * \file minc2_private.h
 * \brief MINC 2.0 private constants, types, and functions.
 */
#ifndef MINC2_PRIVATE_H
#define MINC2_PRIVATE_H

#include "config.h"
#include <string.h>
#include "minc2_structs.h"

/** The root of all MINC 2.0 objects in the HDF5 hierarchy.
 */
#define MI_ROOT_PATH "/minc-2.0"
#define MI_ROOT_COMMENT "Root of the MINC 2.0 data hierarchy"

#define MI_DIMAGE_PATH "image"


#define MI_INFO_NAME "info"
#define MI_INFO_COMMENT "Group holding directly accessible attributes"

#define MI_DIMENSIONS_PATH "dimensions"
#define MI_DIMS_COMMENT "Group holding dimension variables"

/** The fixed path to the full-resolution image data.
 */
#define MI_IMAGE_PATH MI_ROOT_PATH "/" MI_DIMAGE_PATH

/** The fixed path to the full-resolution image data.
 */
#define MI_FULLIMAGE_PATH MI_IMAGE_PATH "/0"

/** The fixed path to the dimension 
 */
#define MI_FULLDIMENSIONS_PATH MI_ROOT_PATH "/dimensions"


/** Size of a linear transform */
#define MI2_LIN_XFM_SIZE 4

/** Standard linear transform, a 4x4 matrix.
 */
typedef double mi_lin_xfm_t[MI2_LIN_XFM_SIZE][MI2_LIN_XFM_SIZE];

#ifdef _WIN32
typedef __int64 mi_i64_t;
#else //_WIN32
typedef long long mi_i64_t;
#endif //_WIN32

/** The fixed path to the dimension 
 */
#define MI_FULLDIMENSIONS_PATH MI_ROOT_PATH "/dimensions"

/** \internal
 * Volume properties  
 */
struct mivolprops {
    miboolean_t enable_flag;    /* enable multi-res */
    int depth;                  /* multi-res depth */
    micompression_t compression_type;
    int zlib_level; 
    int edge_count;             /* how many chunks */
    int *edge_lengths;          /* size of each chunk */
    int max_lengths;
    misize_t record_length;
    char *record_name;
    int  template_flag;
    int checksum;               /*FLETCHER32 checksum is enabled*/
}; 

/** \internal
 * Dimension handle  
 */
struct midimension {
  midimattr_t attr;             /* Dimension attributes */
  midimclass_t dim_class;       /* Dimension class */
  double direction_cosines[3];  /* Direction cosines */
  miflipping_t flipping_order;
  char *name;                   /* Dimension name */
  double *offsets;              /* Offsets (if irregular) */
  double step;                  /* Step size */
  misize_t length;              /* Length */
  double start;                 /* Start value */
  char *units;                  /* Units string */
  double width;                 /* Sample width (if regular) */
  double *widths;               /* Widths (if irregular) */
  char *comments;               /* Comment string */
  mihandle_t volume_handle;     /* Handle of associated volume */
  short world_index;            /* -1, MI2_X, MI2_Y, or MI2_Z */
  midimalign_t align;           /* MI_DIMALIGN_CENTRE, MI_DIMALIGN_START */
};

/** \internal
 * Volume handle  
 */
struct mivolume {
  hid_t hdf_id;
  miboolean_t has_slice_scaling;
  int number_of_dims;
  midimhandle_t *dim_handles;   /* file order of dimensions */
  int *dim_indices;             /* apparent order of dimensions */
  mitype_t volume_type;
  miclass_t volume_class;
  mivolumeprops_t create_props;
  double valid_min;             /* Volume-wide valid min */
  double valid_max;             /* Volume-wide valid max */
  mi_lin_xfm_t v2w_transform;   /* Voxel-to-world transform */
  mi_lin_xfm_t w2v_transform;   /* World-to-voxel transform (inverse) */
  int selected_resolution;      /* The current resolution (0-N) */
  int mode;                     /* Open mode */
  hid_t ftype_id;               /* File type ID of image. */
  hid_t mtype_id;               /* Memory type ID of image. */
  hid_t plist_id;               /* Image property list */
  hid_t image_id;               /* Dataset for image */
  hid_t imax_id;                /* Dataset for image-max */
  hid_t imin_id;                /* Dataset for image-min */
  double scale_min;             /* Global minimum */
  double scale_max;             /* Global maximum */
  miboolean_t is_dirty;         /* TRUE if data has been modified. */
};

/**
 * \internal
 * "semi-private" functions.
 ****************************************************************************/
/* From m2util.c */
hid_t midescend_path(hid_t file_id, const char *path);
hid_t mitype_to_hdftype(mitype_t, int);
int mitype_len ( mitype_t mitype );
const char * mitype_sign ( mitype_t mitype );

int mitype_to_nctype(mitype_t, int *is_signed);

int miget_attribute(mihandle_t volume, const char *varpath, 
                           const char *attname, mitype_t data_type, 
                           size_t maxvals, void *values);

int miset_attr_at_loc(hid_t hdf_loc, const char *attname, 
                             mitype_t data_type, 
                             size_t maxvals, const void *values);

int miset_attribute(mihandle_t volume, const char *varpath, 
                           const char *attname, mitype_t data_type, 
                           size_t maxvals, const void *values);

/*void mifind_spatial_dims(int mincid, int space_to_dim[], int dim_to_space[]);*/

void miget_voxel_to_world(mihandle_t volume, mi_lin_xfm_t voxel_to_world);

void minormalize_vector(double vector[]);

void mitransform_coord(double out_coord[],
                              mi_lin_xfm_t transform,
                              const double in_coord[]);

int  miinvert_transform(mi_lin_xfm_t transform, mi_lin_xfm_t inverse);

void miinit(void);

void miinit_enum(hid_t);

int miget_scalar(hid_t loc_id, hid_t type_id, const char *path, 
                        void *data);

int minc_create_thumbnail(mihandle_t volume, int grp);

int minc_update_thumbnail(mihandle_t volume, hid_t loc_id, int igrp, int ogrp);

int minc_update_thumbnails(mihandle_t volume);

int scaled_maximal_pivoting_gaussian_elimination(int   n,
                                                  int   row[],
                                                  double **a,
                                                  int   n_values,
                                                  double **solution );

int scaled_maximal_pivoting_gaussian_elimination_real(int n,
                                                      double **coefs,
                                                      int n_values,
                                                      double **values );

double *alloc1d(int);
double **alloc2d(int, int);
void free2d(int, double **);

/* m2util : creation of minc2 comformant datasets*/
int create_dataset(hid_t hdf_file, const char *path);
int create_standard_dataset(hid_t hdf_file, const char *path);

int add_minimal_minc_attributes(hid_t hdf_file, hid_t dset_id);
int add_standard_minc_attributes(hid_t hdf_file, hid_t dset_id);


/* From hyper.c */
int mitranslate_hyperslab_origin(mihandle_t volume, 
                                const misize_t* start, 
                                const misize_t* count,
                                hsize_t* hdf_start,
                                hsize_t* hdf_count,
                                int* dir);
/* From volume.c */
void misave_valid_range(mihandle_t volume);

/* From valid.c*/
void miinit_default_range(mitype_t mitype, double *valid_max, double *valid_min);

#ifndef HAVE_RINT
double rint(double v);
#endif

#ifdef _MSC_VER
#define snprintf _snprintf 
#define vsnprintf _vsnprintf 
#define strcasecmp _stricmp 
#define strncasecmp _strnicmp 
#endif

#endif /*MINC2_PRIVATE_H*/
