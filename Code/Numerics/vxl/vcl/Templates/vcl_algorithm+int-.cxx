#include <vcl_iterator.h>
#include <vcl_iostream.h>
#include <vcl_algorithm.txx>

VCL_SORT_INSTANTIATE(int *, int);
VCL_FIND_INSTANTIATE(int *, int);
VCL_FIND_INSTANTIATE(int *, unsigned);
VCL_COPY_INSTANTIATE(int *, vcl_ostream_iterator<int>);

#if defined(VCL_EGCS) && !defined(GNU_LIBSTDCXX_V3)
template int * max_element(int * , int * );
#endif

#if defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
template int * fill_n(int *, int, int const &);
template int * unique(int *, int *);
#endif
