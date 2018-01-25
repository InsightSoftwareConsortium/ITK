#include <string>
#include <vector>
#include <vsl/vsl_map_io.hxx>
#include <vsl/vsl_vector_io.hxx>
#include <vcl_compiler.h>
#include <vnl/io/vnl_io_vector.h>
typedef std::vector< vnl_vector<double> > value;
VSL_MAP_IO_INSTANTIATE(std::string, value, std::less<std::string>);
