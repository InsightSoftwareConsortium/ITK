#include <vcl_algorithm.txx>

// Also instantiated in vcl_vector+uint-.cxx
//#ifdef GNU_LIBSTDCXX_V3
//# include <vcl_vector.h>
//VCL_FIND_INSTANTIATE(vcl_vector<unsigned>::iterator, unsigned);
//VCL_FIND_INSTANTIATE(vcl_vector<unsigned>::const_iterator, unsigned);
//#endif

VCL_SORT_INSTANTIATE(unsigned*, unsigned);
VCL_FIND_INSTANTIATE(unsigned*, unsigned);
VCL_FIND_INSTANTIATE(unsigned const*, unsigned);

#if defined(VCL_EGCS) && !defined(GNU_LIBSTDCXX_V3)
template unsigned *unique(unsigned *, unsigned *);
#endif
