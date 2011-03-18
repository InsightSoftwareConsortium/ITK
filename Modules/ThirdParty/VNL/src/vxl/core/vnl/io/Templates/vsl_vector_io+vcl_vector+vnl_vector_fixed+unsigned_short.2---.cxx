#include <vsl/vsl_vector_io.txx>
#include <vnl/io/vnl_io_vector_fixed.h>

typedef vcl_vector < vnl_vector_fixed<unsigned short, 2> > vect_nvecf;
VSL_VECTOR_IO_INSTANTIATE( vect_nvecf );
