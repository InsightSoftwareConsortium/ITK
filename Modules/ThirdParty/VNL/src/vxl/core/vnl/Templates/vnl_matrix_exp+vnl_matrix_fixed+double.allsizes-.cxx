#include "vnl/vnl_matrix_exp.hxx"
#include "vnl/vnl_matrix_fixed.h"

using T11 = vnl_matrix_fixed<double, 1, 1>;
VNL_MATRIX_EXP_INSTANTIATE(T11);

using T22 = vnl_matrix_fixed<double, 2, 2>;
VNL_MATRIX_EXP_INSTANTIATE(T22);

using T33 = vnl_matrix_fixed<double, 3, 3>;
VNL_MATRIX_EXP_INSTANTIATE(T33);

using T44 = vnl_matrix_fixed<double, 4, 4>;
VNL_MATRIX_EXP_INSTANTIATE(T44);
