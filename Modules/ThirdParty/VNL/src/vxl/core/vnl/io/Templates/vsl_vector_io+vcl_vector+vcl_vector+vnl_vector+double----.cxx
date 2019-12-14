#include "vsl/vsl_vector_io.hxx"
#include <vnl/io/vnl_io_vector.h>

using cvec_cvec_nvec_double = std::vector<std::vector<vnl_vector<double>>>;
VSL_VECTOR_IO_INSTANTIATE(cvec_cvec_nvec_double);
