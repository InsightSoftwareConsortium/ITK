/* minc_simple.h 
 *
 * Simplified interface for MINC files.
 */

#define MINC_STATUS_OK 0
#define MINC_STATUS_ERROR (-1)

#define MINC_TYPE_CHAR 10
#define MINC_TYPE_UCHAR 20
#define MINC_TYPE_SHORT 30
#define MINC_TYPE_USHORT 40
#define MINC_TYPE_INT 50
#define MINC_TYPE_UINT 60
#define MINC_TYPE_FLOAT 70
#define MINC_TYPE_DOUBLE 80

#define MINC_3D 3               /* Number of spatial dimensions */

/* Get information about a MINC file.
 */
MNCAPI int 
minc_file_size(char *path,      /* Path to the file */
               long *ct,        /* Total length of time axis, in voxels */
               long *cz,        /* Total length of Z axis, in voxels */
               long *cy,        /* Total length of Y axis, in voxels */
               long *cx,        /* Total length of X axis, in voxels */
               long *cvoxels,   /* Total number of voxels */
               long *cbytes);   /* Total number of bytes as native datatype */

/* Load data from a MINC file.
 */
MNCAPI int 
minc_load_data(char *path,      /* Path to the file */
               void *dataptr,   /* Buffer to store data */
               int datatype,    /* Type of data as read into memory */
               long *ct, long *cz, long *cy, long *cx,
               double *dt, double *dz, double *dy, double *dx,
               void **infoptr);

/* Define an output file.  Return value is a file handle, or 
 * MINC_STATUS_ERROR if a problem is detected.
 */
MNCAPI int
minc_save_start(char *path,     /* Path to the file */
                int filetype,   /* Date type as stored in the file */
                long ct,        /* Total length of time axis, in voxels */
                long cz,        /* Total length of Z axis, in voxels */
                long cy,        /* Total length of Y axis, in voxels */
                long cx,        /* Total length of X axis, in voxels */
                double dt,      /* Sample width along time axis, in seconds */
                double dz,      /* Sample width along Z axis, in mm */
                double dy,      /* Sample width along Y axis, in mm */
                double dx,      /* Sample width along X axis, in mm */
                void *infoptr,  /* Opaque file structure information */
                const char *history); /* New history information */

/* Write data to file.  Return value is MINC_STATUS_OK or MINC_STATUS_ERROR.
 */
MNCAPI int 
minc_save_data(int handle,    /* Handle returned by minc_save_start */
               void *dataptr,   /* Data to write */
               int datatype,    /* Type of data in memory */
               long st,         /* Start position of 4D hyperslab */
               long sz, 
               long sy, 
               long sx,
               long ct,         /* Size of 4D hyperslab */
               long cz, 
               long cy, 
               long cx);

/* Called when a particular file is complete.
 */
MNCAPI int 
minc_save_done(int handle);

/* Called to free memory associated with the infoptr.
 */                          
MNCAPI void 
minc_free_info(void *infoptr);

MNCAPI int 
minc_get_world_transform(int handle, double transform[4][4], int spatial_axes[3]);

MNCAPI void
minc_transform_to_world(const long voxel[], const int spatial_axes[3],
                        double transform[4][4], double world[3]);
