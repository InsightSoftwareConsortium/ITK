#include <vsl/vsl_map_io.hxx>
#include <vsl/vsl_vector_io.hxx>
#include <vcl_string.h>
#include <vcl_vector.h>
#include <vnl/io/vnl_io_vector.h>
typedef vcl_vector< vnl_vector<double> > value;
VSL_MAP_IO_INSTANTIATE(vcl_string, value, vcl_less<vcl_string>);
