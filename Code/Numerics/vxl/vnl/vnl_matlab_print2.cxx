/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vnl_matlab_print2.h"
#include <vnl/vnl_complex.h>

VNL_MATLAB_PRINT2_INSTANTIATE(vnl_vector<float>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_vector<double>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_vector<vnl_double_complex>);

VNL_MATLAB_PRINT2_INSTANTIATE(vnl_matrix<float>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_matrix<double>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_matrix<vnl_double_complex>);
