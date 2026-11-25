/* NOTE:  When changing version consider the impact on versions in
  nifti2_io_version.h nifti1_io_version.h nifticdf_version.h and znzlib.h
*/
#define ZNZLIB_VERSION_MAJOR 3
#define ZNZLIB_VERSION_MINOR 0
#define ZNZLIB_VERSION_PATCH 0

/* main string macros: ZNZLIB_VERSION and ZNZLIB_SOURCE_VERSION */
#define ZNZLIB_VERSION_TO_STRING(x) ZNZLIB_VERSION_TO_STRING0(x)
#define ZNZLIB_VERSION_TO_STRING0(x) #x
#define ZNZLIB_VERSION                               \
  ZNZLIB_VERSION_TO_STRING(ZNZLIB_VERSION_MAJOR)     \
  "." ZNZLIB_VERSION_TO_STRING(ZNZLIB_VERSION_MINOR) \
  "." ZNZLIB_VERSION_TO_STRING(ZNZLIB_VERSION_PATCH)

#define ZNZLIB_SOURCE_VERSION "znz version " ZNZLIB_VERSION
