#include <vnl/vnl_matrix_exp.hxx>
#include <vnl/vnl_matrix_fixed.h>

typedef vnl_matrix_fixed<double,1,1> T11;
VNL_MATRIX_EXP_INSTANTIATE( T11 );

typedef vnl_matrix_fixed<double,2,2> T22;
VNL_MATRIX_EXP_INSTANTIATE( T22 );

typedef vnl_matrix_fixed<double,3,3> T33;
VNL_MATRIX_EXP_INSTANTIATE( T33 );

typedef vnl_matrix_fixed<double,4,4> T44;
VNL_MATRIX_EXP_INSTANTIATE( T44 );
