#include <vcl_algorithm.txx>

VCL_SORT_INSTANTIATE(unsigned*, unsigned);
VCL_FIND_INSTANTIATE(unsigned*, unsigned);
VCL_FIND_INSTANTIATE(unsigned const*, unsigned);

#if defined(VCL_EGCS) && !defined(GNU_LIBSTDCXX_V3)
template unsigned *unique(unsigned *, unsigned *);
#endif
