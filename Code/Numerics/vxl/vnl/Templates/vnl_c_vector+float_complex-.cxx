#if 0
#include <vcl_complex.txx> // needed for __doadv() with EGCS -- PVr
//see vnl_c_vector+double_complex-.cxx
#endif
#include <vcl_complex.h>
#include <vnl/vnl_c_vector.txx>

VNL_C_VECTOR_INSTANTIATE_unordered(vcl_complex<float>);
