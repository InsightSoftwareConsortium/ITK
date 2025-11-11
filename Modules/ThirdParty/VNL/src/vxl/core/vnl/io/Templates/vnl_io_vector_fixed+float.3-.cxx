#include "vsl/vsl_vector_io.hxx"
#include <vnl/io/vnl_io_vector_fixed.hxx>
VNL_IO_VECTOR_FIXED_INSTANTIATE(float, 3);
using f3 = vnl_vector_fixed<float, 3>;
VSL_VECTOR_IO_INSTANTIATE(f3);
