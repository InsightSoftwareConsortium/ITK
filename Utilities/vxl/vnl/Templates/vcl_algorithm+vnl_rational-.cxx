#include <vnl/vnl_rational.h>
#include <vcl_algorithm.txx>

#ifdef GNU_LIBSTDCXX_V3
# include <vcl_vector.h>
VCL_SORT_INSTANTIATE(vcl_vector<vnl_rational>::iterator, vnl_rational);
namespace std {
  template vnl_rational* std::fill_n<vnl_rational*, unsigned long, vnl_rational>(vnl_rational*, unsigned long, vnl_rational const&);
}
#endif

VCL_SORT_INSTANTIATE(vnl_rational*, vnl_rational);
