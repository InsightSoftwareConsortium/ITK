#include <vcl_iterator.h>
#include <vcl_iostream.h>
#include <vcl_algorithm.txx>
#include <vcl_vector.h>

#ifdef GNU_LIBSTDCXX_V3
VCL_FIND_INSTANTIATE(vcl_vector<int>::iterator, int);
VCL_FIND_INSTANTIATE(vcl_vector<int>::iterator, unsigned);
#endif

VCL_SORT_INSTANTIATE(int *, int);
VCL_FIND_INSTANTIATE(int *, int);
VCL_FIND_INSTANTIATE(int *, unsigned);
VCL_COPY_INSTANTIATE(int *, vcl_ostream_iterator<int>);

#if defined(VCL_EGCS) && !defined(GNU_LIBSTDCXX_V3)
template int * max_element(int * , int * );
#endif

#if defined(VCL_GCC_295)
# ifdef GNU_LIBSTDCXX_V3
namespace std {
  template int * std::fill_n<int *, int, int>(int *, int, int const &);
}
# else
template int * fill_n(int *, int, int const &);
template int * unique(int *, int *);
# endif
#endif
