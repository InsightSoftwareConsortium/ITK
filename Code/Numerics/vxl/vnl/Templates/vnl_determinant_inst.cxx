#include <vcl_complex.h>
#include <vnl/vnl_determinant.txx>

// these could be instantiated for 'int', 'long' and 'rational'.
VNL_DETERMINANT_INSTANTIATE(float);
VNL_DETERMINANT_INSTANTIATE(double);
VNL_DETERMINANT_INSTANTIATE(vcl_complex<float>);
VNL_DETERMINANT_INSTANTIATE(vcl_complex<double>);
