#include <vnl/vnl_matrix_fixed.txx>
template class vnl_matrix_fixed<double,3,3>;

#if defined(VCL_GCC_27)
template ostream& operator<<(ostream&,vnl_matrix_fixed<double,3,3> const&);
template ostream& operator>>(istream&, vnl_matrix_fixed<double,3,3>);
template vnl_matrix<double> operator*(const double &s, const vnl_matrix_fixed<double,3,3> &M);
#endif
