#include <vnl/vnl_complex.h>
#include <vnl/vnl_c_vector.hxx>

#ifndef __hppa // bug in HP assembler?
VNL_C_VECTOR_INSTANTIATE_unordered(std::complex<long double>);
#endif
