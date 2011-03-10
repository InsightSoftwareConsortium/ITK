#include <vnl/vnl_matrix_exp.txx>
#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,3,3> T;
VNL_MATRIX_EXP_INSTANTIATE( T );
