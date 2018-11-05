#include <complex>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/vnl_det.hxx>

#ifndef __hppa // bug in HP assembler?
VNL_DET_INSTANTIATE(std::complex<long double>);
#endif
