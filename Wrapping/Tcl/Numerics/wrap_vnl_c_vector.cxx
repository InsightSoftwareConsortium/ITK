#include "vcl_complex.h"
#include "vnl_c_vector.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"
ITK_WRAP_VNL(vnl_c_vector);

#if 0
// Could add vnl_c_vector_bool, but it is disabled for ITK's
// VXLNumerics library.
namespace _cable_
{
  namespace wrappers
  {
    typedef vnl_c_vector<bool> vnl_c_vector_bool;
  }
}

void force_instantiate2()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_c_vector_bool);
}
#endif

#endif
