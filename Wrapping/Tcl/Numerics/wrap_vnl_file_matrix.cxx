#include "vnl_file_matrix.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_file_matrix);
  namespace wrappers
  {
    typedef vnl_file_matrix<double> vnl_file_matrix_double;
    typedef vnl_file_matrix<float>  vnl_file_matrix_float;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_file_matrix_double);
  sizeof(vnl_file_matrix_float);
}

#endif
