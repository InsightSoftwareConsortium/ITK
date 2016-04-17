#include <complex>
#include <vcl_compiler.h>
#include <vnl/vnl_det.hxx>

#ifndef __hppa // bug in HP assembler?
VNL_DET_INSTANTIATE(std::complex<long double>);
#endif
