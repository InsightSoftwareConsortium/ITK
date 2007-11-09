#include <vsl/vsl_vector_io.txx>
#include <vnl/io/vnl_io_vector.h>

typedef vcl_vector < vcl_vector< vnl_vector<double> > > cvec_cvec_nvec_double;
VSL_VECTOR_IO_INSTANTIATE( cvec_cvec_nvec_double );
