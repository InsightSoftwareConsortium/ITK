#include <vnl/vnl_complex.h>
#include <vnl/vnl_determinant.txx>

// these could be instantiated for 'int', 'long' and 'rational'.
VNL_DETERMINANT_INSTANTIATE(float);
VNL_DETERMINANT_INSTANTIATE(double);
VNL_DETERMINANT_INSTANTIATE(vnl_float_complex);
VNL_DETERMINANT_INSTANTIATE(vnl_double_complex);
