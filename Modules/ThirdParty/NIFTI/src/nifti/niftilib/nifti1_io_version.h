/* NOTE:  When changing version consider the impact on versions in
  nifti2_io_version.h nifti1_io_version.h nifticdf_version.h and znzlib.h
*/
#define NIFTI1_IO_VERSION_MAJOR 2
#define NIFTI1_IO_VERSION_MINOR 1
#define NIFTI1_IO_VERSION_PATCH 0

/* main string macros: NIFTI1_IO_VERSION and NIFTI1_IO_SOURCE_VERSION */
#define NIFTI1_IO_VERSION_TO_STRING(x) NIFTI1_IO_VERSION_TO_STRING0(x)
#define NIFTI1_IO_VERSION_TO_STRING0(x) #x
#define NIFTI1_IO_VERSION                                   \
   NIFTI1_IO_VERSION_TO_STRING(NIFTI1_IO_VERSION_MAJOR)     \
   "." NIFTI1_IO_VERSION_TO_STRING(NIFTI1_IO_VERSION_MINOR) \
   "." NIFTI1_IO_VERSION_TO_STRING(NIFTI1_IO_VERSION_PATCH)

#define NIFTI1_IO_SOURCE_VERSION "NIFTI1_IO version " NIFTI1_IO_VERSION
