// This is vxl/vnl/vnl_matlab_print2.cxx

/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif
#include "vnl_matlab_print2.h"
#include <vcl_complex.h>

VNL_MATLAB_PRINT2_INSTANTIATE(vnl_vector<float>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_vector<double>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_vector<vcl_complex<double> >);

VNL_MATLAB_PRINT2_INSTANTIATE(vnl_matrix<float>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_matrix<double>);
VNL_MATLAB_PRINT2_INSTANTIATE(vnl_matrix<vcl_complex<double> >);
