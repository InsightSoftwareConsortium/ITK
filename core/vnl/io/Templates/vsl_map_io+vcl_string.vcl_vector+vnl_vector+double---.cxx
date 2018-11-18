#include <string>
#include <vector>
#include <vsl/vsl_map_io.hxx>
#include <vsl/vsl_vector_io.hxx>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <vnl/io/vnl_io_vector.h>
typedef std::vector< vnl_vector<double> > value;
VSL_MAP_IO_INSTANTIATE(std::string, value, std::less<std::string>);
