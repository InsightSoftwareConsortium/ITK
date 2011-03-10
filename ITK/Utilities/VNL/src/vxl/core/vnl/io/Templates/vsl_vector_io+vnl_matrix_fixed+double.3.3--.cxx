#include <vsl/vsl_vector_io.txx>
#include <vnl/io/vnl_io_matrix_fixed.txx>
// use a typedef to remove commas from the macro parameter that cause errors
typedef vnl_matrix_fixed<double,3,3> vnl_matrix_fixed_double_3_3;
VSL_VECTOR_IO_INSTANTIATE(vnl_matrix_fixed_double_3_3);
