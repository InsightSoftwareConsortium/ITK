#include <vnl/vnl_matrix_fixed.txx>
template class vnl_matrix_fixed<float,3,3>;
#if defined(VCL_GCC_27)
template ostream& operator<<(ostream&,vnl_matrix_fixed<float,3,3> const&);
template vnl_matrix<float> operator*(const float &s, const vnl_matrix_fixed<float,3,3> &M);
#endif
