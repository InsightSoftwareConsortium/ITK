#include "vcl_complex.h"
#include "vnl/vnl_diag_matrix.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_VXLNumerics.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(vnl_diag_matrix);
  typedef vcl_complex<double> double_complex;
  typedef vcl_complex<float> float_complex;
  namespace wrappers
  {
    typedef vnl_diag_matrix<double>         vnl_diag_matrix_double;
    typedef vnl_diag_matrix<double_complex> vnl_diag_matrix_double_complex;
    typedef vnl_diag_matrix<float>          vnl_diag_matrix_float;
    typedef vnl_diag_matrix<float_complex>  vnl_diag_matrix_float_complex;
    typedef vnl_diag_matrix<int>            vnl_diag_matrix_int;
    typedef vnl_diag_matrix<long double>    vnl_diag_matrix_long_double;
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers;
  sizeof(vnl_diag_matrix_double);
  sizeof(vnl_diag_matrix_double_complex);
  sizeof(vnl_diag_matrix_float);
  sizeof(vnl_diag_matrix_float_complex);
  sizeof(vnl_diag_matrix_int);
  sizeof(vnl_diag_matrix_long_double);
}

#endif
