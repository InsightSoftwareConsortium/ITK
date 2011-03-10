#include <vnl/vnl_vector.txx>
VNL_VECTOR_INSTANTIATE(double);
#ifdef GNU_LIBSTDCXX_V3
template double vnl_vector_ssd<double> (vnl_vector<double> const&, vnl_vector<double> const&);
#endif
