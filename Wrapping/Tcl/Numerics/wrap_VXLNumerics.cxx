#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE;
  const char* const package_version = ITK_WRAP_PACKAGE_VERSION;
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(vnl_matrix),
    ITK_WRAP_GROUP(vnl_vector),
    ITK_WRAP_GROUP(vnl_c_vector),
    ITK_WRAP_GROUP(vnl_diag_matrix),
    ITK_WRAP_GROUP(vnl_file_matrix),
    ITK_WRAP_GROUP(vnl_file_vector),
    ITK_WRAP_GROUP(vnl_fortran_copy)
  };
}
#endif
