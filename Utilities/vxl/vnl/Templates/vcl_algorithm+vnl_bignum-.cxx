#include <vnl/vnl_bignum.h>
#include <vcl_algorithm.txx>

#ifdef GNU_LIBSTDCXX_V3
# include <vcl_vector.h>
VCL_SORT_INSTANTIATE(vcl_vector<vnl_bignum>::iterator, vnl_bignum);
namespace std {
  template vnl_bignum* std::fill_n<vnl_bignum*, unsigned long, vnl_bignum>(vnl_bignum*, unsigned long, vnl_bignum const&);
}
#endif

VCL_SORT_INSTANTIATE(vnl_bignum*, vnl_bignum);
