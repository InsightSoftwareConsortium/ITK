#include "vnl_file_vector.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_file_vector);
  namespace wrappers
  {
    typedef vnl_file_vector<double>  vnl_file_vector_double;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_file_vector_double);
}

#endif
