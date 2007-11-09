#include <vnl/vnl_rational.h>
#include <vnl/vnl_rational_traits.h>
#include <vnl/vnl_vector.txx>
VNL_VECTOR_INSTANTIATE(vnl_rational);
#ifdef GNU_LIBSTDCXX_V3
template vnl_rational vnl_vector_ssd<vnl_rational> (vnl_vector<vnl_rational> const&, vnl_vector<vnl_rational> const&);
#endif
