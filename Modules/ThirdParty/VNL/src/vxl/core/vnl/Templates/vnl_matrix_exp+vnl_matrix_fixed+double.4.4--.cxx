#include <vnl/vnl_matrix_exp.txx>
#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,4,4> T;
VNL_MATRIX_EXP_INSTANTIATE( T );
