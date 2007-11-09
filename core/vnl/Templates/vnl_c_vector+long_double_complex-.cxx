#include <vnl/vnl_complex.h>
#include <vnl/vnl_c_vector.txx>

#ifndef __hppa // bug in HP assembler?
VNL_C_VECTOR_INSTANTIATE_unordered(vcl_complex<long double>);
#endif
