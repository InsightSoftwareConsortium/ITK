#include <vnl/vnl_matrix_fixed.txx>
template class vnl_matrix_fixed<double,4,4>;
#if defined(VCL_GCC_27)
template ostream& operator<<(ostream&,vnl_matrix_fixed<double,4,4> const&);
#endif
