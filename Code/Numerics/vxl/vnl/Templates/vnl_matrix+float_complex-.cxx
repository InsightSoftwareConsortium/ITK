#include <vnl/vnl_matrix.txx>
#include <vnl/vnl_complex.h>
#ifdef VCL_GCC_27
inline
vnl_float_complex operator/(vnl_float_complex const &z, double d)
{
  return vnl_float_complex(z.real()/d, z.imag()/d);
}
#endif
VNL_MATRIX_INSTANTIATE(vnl_float_complex);
