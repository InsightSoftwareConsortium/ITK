#include <vcl_complex.h>
#include <vnl/vnl_det.hxx>

#ifndef __hppa // bug in HP assembler?
VNL_DET_INSTANTIATE(vcl_complex<long double>);
#endif
