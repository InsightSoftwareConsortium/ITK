#include <vnl/vnl_bignum.h>
#include <vnl/vnl_bignum_traits.h>
#include <vnl/vnl_vector.txx>
VNL_VECTOR_INSTANTIATE(vnl_bignum);
#ifdef GNU_LIBSTDCXX_V3
template vnl_bignum vnl_vector_ssd<vnl_bignum> (vnl_vector<vnl_bignum> const&, vnl_vector<vnl_bignum> const&);
#endif
