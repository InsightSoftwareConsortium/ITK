#include <vnl/vnl_complex.h>
#include <vcl_complex.txx> // needed for __doadv() with EGCS -- PVr
//see vnl_c_vector+double_complex-.cxx
#include <vnl/vnl_c_vector.txx>

VNL_C_VECTOR_INSTANTIATE_unordered(vnl_float_complex);
