#include <vcl_algorithm.txx>

#ifdef GNU_LIBSTDCXX_V3
# include <vcl_vector.h>
VCL_SORT_INSTANTIATE(vcl_vector<double>::iterator, double);
VCL_SORT_INSTANTIATE(vcl_vector<double*>::iterator, double*);
#endif

VCL_SORT_INSTANTIATE(double*, double);
VCL_SORT_INSTANTIATE(double**, double*);
